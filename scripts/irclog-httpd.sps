;;; irclog-httpd.sps -- HTTP Server providing an interface to IRC logs

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

#!r6rs
(import (except (rnrs) delete-file file-exists?)
        (rnrs r5rs)
        (srfi :2 and-let*)
        (srfi :8 receive)
        (spells parameter)
        (spells alist)
        (spells table)
        (only (srfi :13 strings)
              string-join string-concatenate string-prefix?)
        (only (srfi :1 lists) unfold drop filter-map iota)
        (spells string-utils)
        (spells pathname)
        (spells filesys)
        (spells time-lib)
        (spells foreign)
        (spells misc)
        (spells tracing)
        (fmt)
        (sxml simple)
        (sxml transform)
        (sbank glib-daemon)
        (sbank gobject)
        (sbank soup)
        (sbank typelib)
        (sbank ctypes basic)
        (irclogs utils)
        (irclogs))

(soup-setup!)
(typelib-import
 (prefix (only ("GLib" #f)
               thread-init
               main-loop-new main-loop-run
               timeout-add-seconds idle-add io-add-watch io-channel-unix-new
               markup-escape-text)
         g-)
 (prefix (only ("Soup" #f) <server> <address> status-get-phrase)
         soup-))

(define (main argv)
  (let ((config
         (case (length argv)
           ((1) (default-config))
           ((2) (merge-config (default-config) (read-config (cadr argv))))
           (else
            (bail-out "usage: irclog-httpd.sps [config-file]")))))
    (irclog-httpd config)))

(define (default-config)
  '((port 8001)
    (interface "127.0.0.1")
    ;; Where static files are to be found (location on the local
    ;; filesystem)
    (static-files "./static")

    ;; This path is is stripped from requests and only requests
    ;; matching that path are considered
    (path-prefix "")

    (irclogs

     ;; This URL is used to refer to the served content
     (base-url "/")

     ;; Where to look for log files
     (log-dir ".")

     ;; Where to put in state (must be a writable directory)
     (state-dir "./.irclogs-state")))
  )

(define (merge-config config new-values)
  (let loop ((result config) (vals new-values))
    (if (null? vals)
        result
        (cond ((assq (caar vals) config)
               => (lambda (old-entry)
                    (loop (cons (car vals) (filter (lambda (e)
                                                     (not (eq? e old-entry)))
                                                   result))
                          (cdr vals))))
              (else
               (loop (cons (car vals) result)
                     (cdr vals)))))))

(define (read-config filename)
  (call-with-input-file (x->namestring filename)
    (lambda (port)
      (unfold eof-object? values (lambda (x) (read port)) (read port)))))

(define-record-type task-result
  (fields task-id msg val))

(define (task-result-values result)
  (values (task-result-task-id result)
          (task-result-msg result)
          (task-result-val result)))

(define new-task-id
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))

(define (config-ref config name)
  (car (assq-ref config name)))

(define (instantiate-server config)
  (let ((address (send <soup-address>
                   (new (config-ref config 'interface)
                        (config-ref config 'port)))))
    (send address (resolve-sync #f))
    (let ((server
           (send <soup-server> (new/props 'interface address
                                          'server-header "irclog-httpd"))))
      (unless server
        (bail-out "Unable to bind to server port {0}\n"
                  (config-ref config 'port)))
      server)))

(define (irclog-httpd config)
  (g-thread-init #f)
  (g-install-signal-handler '(int) (lambda (sig)
                                     (println "Received signal {0}, exiting" sig)
                                     (exit 1)))
  (parameterize ((null-ok-always-on? #t)) ;; Needed for field access, will go away
    (let ((server (instantiate-server config))
          (irclogs (make-irclogs (assq-ref config 'irclogs)))
          (path-prefix (config-ref config 'path-prefix))
          (n-active-tasks 0)
          (task-table (make-table 'eqv))
          (scheduler (make-scheduler)))
      (define (task-title task-id)
        (table-ref task-table task-id))
      (define (scheduler-idle-callback)
        (scheduler-work scheduler
                        (lambda (result)
                          (receive (task-id msg val) (task-result-values result)
                            (val)
                            (send server (unpause-message msg))
                            (set! n-active-tasks (- n-active-tasks 1))
                            (println "task {0} finished; {1} still active"
                                     task-id n-active-tasks)))
                        (lambda (result)
                          (receive (task-id msg val) (task-result-values result)
                            (println "<task {0} '{1}'> yielded value {2}"
                                     task-id (task-title task-id) val)
                            (when (and (not (eq? 'cancelled val))
                                       (eq? 'chunked (send (send msg (get-response-headers))
                                                       (get-encoding))))
                              (send server (unpause-message msg)))
                            (not (eq? 'cancelled val))))))
      (define (defer-task title msg proc)
        (let ((task-id (new-task-id)))
          (let ((had-work? (scheduler-has-work? scheduler)))
            (scheduler-enqueue! scheduler (lambda (yield)
                                            (make-task-result
                                             task-id
                                             msg
                                             (proc (lambda (v)
                                                     (yield (make-task-result task-id msg v)))))))
            (set! n-active-tasks (+ n-active-tasks 1))
            (println "task {0} enqueued; {1} now active" task-id n-active-tasks)
            (table-set! task-table task-id title)
            (when (not had-work?)
              (g-idle-add scheduler-idle-callback)))
          task-id))
      (irclogs 'update-state)
      (g-timeout-add-seconds
       60
       (lambda ()
         (irclogs 'update-state)
         #t))
      (g-idle-add scheduler-idle-callback)
      (send server
        (add-handler path-prefix
                     (wrap-handler (irclogs-handler defer-task irclogs)
                                   path-prefix)))
      (let ((static-prefix (string-append path-prefix "/static")))
        (send server
          (add-handler static-prefix
                       (wrap-handler
                        (static-file-handler
                         (pathname-as-directory (config-ref config 'static-files))
                         1)
                        path-prefix))))
      (send server (run-async))
      (println "Waiting for requests...")
      (g-main-loop-run (g-main-loop-new #f #t)))))

(define (wrap-handler handler path-prefix)
  (lambda (server msg path query client)
    (let ((method (send msg (get 'method)))
          (req-headers (send msg (get-request-headers)))
          (query-alist (map (lambda (e)
                              (cons (string->symbol (car e)) (cdr e)))
                            (ghash->alist query)))
          (stripped-path
           (if (string-prefix? path-prefix path)
               (substring path (string-length path-prefix) (string-length path)))))
      (println "{0} {1} (=> {2}) {3} HTTP/1.{4}"
               method path stripped-path query-alist (send msg (get 'http-version)))
      (send req-headers
        (foreach (lambda (name value)
                   (println "{0}: {1}" name value))))
      (let ((body (send msg (get-request-body))))
        (when (> (send body (get-length)) 0)
          (println (send body (get-data)))))
      (receive (code last-modified generate-response)
               (handler server msg stripped-path query-alist)
        (define (log-response)
          (println " -> {0} {1}"
                   (send msg (get-status-code))
                   (send msg (get 'reason-phrase))))
        (define (add-headers)
          (when last-modified
            (add-response-headers!
             msg
             (cons "Last-Modified" (time-utc->http-date last-modified)))))
        (define (do-respond)
          (when generate-response
            (generate-response))
          (case code
            ((defer)
             (send server (pause-message msg))
             (println " -> deferred"))
            (else
             (add-headers)
             (send msg (set-status (soup-status code)))
             (log-response))))
        (define (not-modified)
          (add-headers)
          (send msg (set-status (soup-status 'not-modified)))
          (log-response))
        (cond ((and last-modified (send req-headers (get "If-Modified-Since")))
               => (lambda (date-str)
                    (cond ((and-let* ((hdr-date (parse-http-date date-str)))
                             (time>? last-modified (date->time-utc hdr-date)))
                           (do-respond))
                          (else
                           (not-modified)))))
              (else
               (do-respond)))))))

(define (msg-method msg)
  (string->symbol (string-downcase (send msg (get 'method)))))

(define root-pathname (make-pathname '/ '() #f))

(define (make-counter-task n)
  (lambda (yield)
    (let loop ((i 0))
      (when (< i n)
        (yield i)
        (sleep-seconds 0.1)
        (loop (+ i 1))))))


(define (render-counter-page n)
  (let ((task (make-counter-task n)))
    `((div
       (^ (align "center"))
       (h1 ,(ssubst "Counting up to {0} (starting at 0)" (- n 1)))
       (p (task
           ,(lambda (port)
              (let ((timer (start-timer)))
                (task (lambda (i)
                        (fprintf port "count: {0}<br/>" i)
                        (yield/c #t)))
                (fprintf port "Successfully counted from 0 to {0} in {1} seconds.<br/>"
                         (- n 1) (fmt #f (num (inexact (timer)) 10 4)))))))))))

(define (counter-page-renderer query)
  (render-counter-page 30))

(define (split-sxml-response response)
  (if (and (pair? response)
           (pair? (car response))
           (eq? (caar response) 'meta))
      (values (cdar response) (cdr response))
      (values '() response)))

(define (js-cdata str)
  (xml-cdata-escape str (lambda (s)
                          (string-append "/* <![CDATA[ */\n" s "\n /* ]]> */"))))

(define (make-heads base-url title meta)
  `((title ,title)
    ,@(filter-map
       (lambda (entry)
         (and (pair? entry)
              (case (car entry)
                ((js-include)
                 `(script (^ (type "text/javascript")
                             (src ,(string-append base-url "static/" (cadr entry))))
                          "//" ; force end tag
                          ))
                ((js-text)
                 `(script (^ (type "text/javascript"))
                          ,(lambda (port)
                             (display (js-cdata (cadr entry)) port))))
                (else
                 #f))))
       meta)))

(define xhtml-doctype
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")

(define (handle-sxml-page base-url msg code title defer response)
  (let-values (((port flush) (make-soup-output-port+flusher (send msg (get-response-body))))
               ((meta sxml) (split-sxml-response response)))
    (let* ((http-version (send msg (get 'http-version)))
           (last-proc #f)
           (output-k #f)
           (work-k #f)
           (escape #f)
           (task-result #f)
           (tasks-done? #f)
           (message-done? #f)
           (first-escape? #t)
           (sxml
            (pre-post-order
             sxml
             `((task *PREORDER* .
                     ,(lambda (tag proc)
                        (define (decorated port)
                          (call/cc
                           (lambda (k)
                             (set! output-k k)
                             (defer title msg
                               (lambda (yield)
                                 (set! task-result
                                       (parameterize ((current-yield
                                                       (lambda (v)
                                                         (cond (message-done?
                                                                (yield 'cancelled))
                                                               (else
                                                                (flush-output-port port)
                                                                (flush)
                                                                (yield v))))))
                                         (proc port)))
                                 (lambda ()
                                   (call/cc
                                    (lambda (k)
                                      (set! work-k k)
                                      (set! tasks-done? (eq? decorated last-proc))
                                      (output-k))))))
                             (escape))))
                        (set! last-proc decorated)
                        decorated))
               (task-result *PREORDER* .
                            ,(lambda (tag)
                               (lambda (port)
                                 (cond ((procedure? task-result)
                                        (task-result port))
                                       (task-result
                                        (sxml->xml task-result port))))))
               (*DEFAULT* . ,list)))))
      (add-response-headers! msg '("Content-Type" . "text/html; charset=utf-8"))
      ;; we defer the response when using HTTP 1.0 and there was a
      ;; task involved (as HTTP 1.0 doesn't support chunked encoding)
      (let ((defer? (and (= http-version 0) last-proc)))
        (values
         (if defer? 'defer code)
         #f
         (lambda ()
           (when (> http-version 0)
             (send (send msg (get-response-body))
               (set-accumulate #f)))

           (when (and last-proc (> http-version 0))
             (send (send msg (get-response-headers))
               (set-encoding 'chunked))
             (send msg (connect 'finished (lambda (msg)
                                            (set! message-done? #t)))))

           (let ((tport (transcoded-port port (make-transcoder (utf-8-codec)))))
             (put-string tport xhtml-doctype)
             (call/cc
              (lambda (k)
                (set! escape k)
                (xhtml-page tport base-url (make-heads base-url title meta) sxml)))
             (unless message-done?
               (flush-output-port tport)
               (flush))
             (when defer?
               (send msg (set-status (soup-status 'ok))))
             (cond ((and last-proc tasks-done?)
                    (close-output-port tport)
                    (work-k)) ; here we escape back into the idle worker
                   ((or (not last-proc) first-escape?)
                    (set! first-escape? #f)
                    (when (not last-proc)
                      (close-output-port tport))
                    ;; This the (only) exit point of this procedure
                    )
                   (else
                    ;; we have some not-finished tasks, and it's not the
                    ;; first escape, so we go on working
                    (work-k))))))))))

(define (handle-page base-url msg code title defer proc . args)
  (let ((method (msg-method msg)))
    (case method
      ((get head)
       (handle-sxml-page base-url msg code title defer (apply proc args)))
      (else
       (values 'not-implemented #f #f)))))

(define (irclogs-handler defer irclogs)
  (define (invoke msg-name)
    (lambda args
      (apply irclogs msg-name args)))
  (lambda (server msg path query)
    (let* ((just-meta? (string=? (send msg (get-method)) "HEAD"))
           (pathname (x->pathname path))
           (comps (pathname-directory pathname))
           (n-comps (length comps))
           (base-url (irclogs 'base-url)))
      (define (not-found)
        (handle-page base-url msg 'not-found "Page not found" defer render-error-page 'not-found))
      (cond ((and (pathname=? pathname root-pathname)
                  (irclogs 'render-overview/html just-meta? #f #f query))
             => (lambda (sxml)
                  (handle-page base-url msg 'ok (page-title) defer (lambda () sxml))))
            ((pathname-file pathname)
             (receive (found? ready?)
                      (cond ((pathname=? (pathname-with-file pathname #f) root-pathname)
                             (let ((filename (file-namestring pathname)))
                               (cond ((string=? filename "count.scm")
                                      (handle-page base-url msg 'ok "Counter task" defer
                                                   counter-page-renderer query))
                                     (else
                                      (values #f #f)))))
                            (else
                             (values #f #f)))
               (if found?
                   ready?
                   (let ((uri (send (send msg (get-uri)) (to-string #f))))
                     (values 'moved-permanently
                             #f
                             (lambda ()
                               (send (send msg (get-response-headers))
                                 (append "Location" (string-append uri "/")))))))))
            ((or (and (= n-comps 1)
                      (irclogs 'render-overview/html just-meta? (car comps) #f query))
                 (and (= n-comps 2)
                      (irclogs 'render-overview/html just-meta? (car comps) (cadr comps) query))
                 (and (= n-comps 3)
                      (apply irclogs 'render-log/html just-meta? comps)))
             => (lambda (response)
                  (handle-page base-url msg 'ok (apply page-title comps) defer (lambda () response))))
            (else
             (not-found))))))

(define page-title
  (case-lambda
    ((tag channel date)
     (ssubst "IRC log for {0}/{1} {2}" tag channel date))
    ((tag channel)
     (ssubst "IRC activity for {0}/{1}" tag channel))
    ((tag)
     (ssubst "IRC activity for {0} channels" tag))
    (()
     "IRC activity overview")))

(define (current-year)
  (date-year (current-date 0)))

(define *file-types*
  '(("css" . "text/css")
    ("html" . "text/html")
    ("txt" . "text/plain")))

(define (file-content-type pathname)
  (or (assoc-ref *file-types* (file-type (pathname-file pathname)))
      "application/octet-stream"))

(define (add-response-headers! msg . hdrs)
  (let ((resp-hdrs (send msg (get-response-headers))))
    (for-each (lambda (hdr/val)
                (send resp-hdrs (append (car hdr/val) (cdr hdr/val))))
              hdrs)))

(define http-date-fmt "~a, ~d ~b ~Y ~H:~M:~S GMT")

(define (time-utc->http-date time)
  (date->string (time-utc->date time 0) http-date-fmt))

(define (parse-http-date s)
  (guard (c (#t #f))
    (date-with-zone-offset (string->date s http-date-fmt) 0)))

(define (static-file-handler base strip)
  (lambda (server msg path query)
    (let* ((rel-path (and-let* ((req-path (x->pathname path))
                                (dirs (pathname-directory req-path))
                                (stripped (and (>= (length dirs) strip)
                                               (drop dirs strip))))
                       (make-pathname #f stripped (pathname-file req-path))))
           (pathname (and rel-path (pathname-join base rel-path))))
      (cond ((and pathname
                  (file-exists? pathname)
                  (file-readable? pathname)
                  (not (file-directory? pathname)))
             (let ((content-type (file-content-type pathname)))
               (define (add-headers!)
                 (add-response-headers!
                  msg
                  (cons "Content-Type" content-type)
                  (cons "Content-Length" (number->string (file-size-in-bytes pathname)))))
               (case (msg-method msg)
                 ((get)
                  (values
                   'ok
                   (file-modification-time pathname)
                   (lambda ()
                     (call-with-port (open-file-input-port (x->namestring pathname))
                       (lambda (port)
                         (let ((content (get-bytevector-all port)))
                           (if (eof-object? content)
                               (add-headers!)
                               (send msg (set-response content-type content)))))))))
                 ((head)
                  (values
                   'ok
                   (file-modification-time pathname)
                   add-headers!))
                 (else
                  (values 'not-implemented #f #f)))))
            (else
             (values (if (and pathname (file-exists? pathname)) 'forbidden 'not-found) #f #f))))))

(define (xhtml-page port base-url head sxml)
  (define (static name)
    (string-append base-url "static/" name))
  (sxml->xml
   `(html (^ (xmlns "http://www.w3.org/1999/xhtml")
             (lang "en")
             (xml:lang "en"))
          (head
           (meta (^ (name "GENERATOR")
                    (content "IRClogs by Andreas Rottmann, based on code by MJ Ray")))
           ,@head
           (link (^ (rel "stylesheet") (type "text/css") (charset "utf-8") (media "all")
                    (href ,(static "common.css"))))
           (link (^ (rel "stylesheet") (type "text/css") (charset "utf-8") (media "all")
                    (href ,(static "irclogs.css"))))
           (style (^ (type "text/css"))
             ,(string-concatenate
               (append
                (list "\n")
                (map
                 (lambda (x)
                   (string-substitute ".n<0> { background: #<1> }\n"
                                      (list x (integer->color x))
                                      'abrackets))
                 (iota 27))
                (map
                 (lambda (x)
                   (string-substitute ".me-l<0> { background: #<1> }\n"
                                      (list x (integer->color x))
                                      'abrackets))
                 (iota 10)))))
           )
          (body ,@sxml))
   port))

(define (render-error-page code)
  (let ((code-num (soup-status code)))
    `((div (^ (align "center"))
           (h1 ,code-num)
           (p ,(soup-status-get-phrase code-num))))))

(define (integer->bits num base) ; int int -> (int ...)
  (if (= num 0)
      '()
      (cons (modulo num base) (integer->bits (quotient num base) base))))

(define colors '("e" "c" "a"))

(define (integer->color num) ; int -> str
  (let ((nb (append (integer->bits num 3) '(0 0 0))))
    (string-append
     (list-ref colors (list-ref nb 0))
     (list-ref colors (list-ref nb 1))
     (list-ref colors (list-ref nb 2)))))

(define (bail-out msg . args)
  (string-substitute (current-error-port) msg args 'braces)
  (newline (current-error-port))
  (exit 1))

(main (command-line))

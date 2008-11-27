;;; irclog-httpd.sps -- HTTP Server providing an interface to IRC logs

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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
        (xitomatl srfi and-let*)
        (spells receive)
        (spells parameter)
        (spells alist)
        (spells table)
        (only (spells strings) string-join string-concatenate)
        (only (spells lists) unfold drop)
        (spells string-substitute)
        (spells pathname)
        (spells filesys)
        (spells time-lib)
        (spells foreign)
        (spells misc)
        (spells tracing)
        (sxml simple)
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
               timeout-add-seconds idle-add
               markup-escape-text)
         g-)
 (prefix (only ("Soup" #f) <server> status-get-phrase)
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
    (static-files "./static")
    (irclogs
     (base-url "/")
     (log-dir ".")
     (state-dir "./.irclogs-state"))))

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

(define (irclog-httpd config)
  (g-thread-init #f)
  (let ((port (car (assq-ref config 'port))))
    (parameterize ((null-ok-always-on? #t)) ;; Needed for field access, will go away
      (let ((server (send <soup-server> (new/props 'port port
                                                   'server-header "irclog-httpd")))
            (irclogs (make-irclogs (assq-ref config 'irclogs)))
            (n-active-tasks 0)
            (task-table (make-table 'eqv))
            (scheduler (make-scheduler)))
        (define (task-title task-id)
          (table-ref task-table task-id))
        (define (scheduler-idle-callback user-data)
          (scheduler-work scheduler
                          (lambda (result)
                            (receive (task-id msg val) (task-result-values result)
                              (set! n-active-tasks (- n-active-tasks 1))
                              (println "task {0} finished; {1} still active"
                                       task-id n-active-tasks)
                              (handle-page (irclogs 'base-url) msg 'ok (task-title task-id)
                                           (lambda () val))
                              (table-set! task-table task-id #f)
                              (log-msg-result task-id msg)
                              (send server (unpause-message msg))))
                          (lambda (result)
                            (receive (task-id msg val) (task-result-values result)
                              (println "<task {0} '{1}'> yielded value {2}"
                                       task-id (task-title task-id) val)
                              #t))))
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
                (g-idle-add scheduler-idle-callback (integer->pointer 0))))
            task-id))
        (unless server
          (bail-out "Unable to bind to server port {0}\n" port))
        (irclogs 'update-state)
        (g-timeout-add-seconds
         60
         (lambda (user-data) (irclogs 'update-state) #t)
         (integer->pointer 0)) ;; Workaround, will go away
        (g-idle-add scheduler-idle-callback (integer->pointer 0))
        (send server
          (add-handler #f (wrap-handler (irclogs-handler defer-task irclogs)))
          (add-handler "/static/" (wrap-handler
                                   (static-file-handler
                                    (pathname-as-directory (car (assq-ref config 'static-files)))
                                    1)))
          (run-async))
        (println "Waiting for requests...")
        (g-main-loop-run (g-main-loop-new #f #t))))))

(define (wrap-handler handler)
  ;; Note that `user-data' will go away when I get around to implement hiding it
  (lambda (server msg path query client user-data)
    (let ((method (send msg (get 'method))))
      (println "{0} {1} HTTP/1.{2}" method  path (send msg (get 'http-version)))
      (send (send msg (get-request-headers))
        (foreach (lambda (name value user-data)
                   (println "{0}: {1}" name value))))
      (let ((body (send msg (get-request-body))))
        (when (> (send body (get-length)) 0)
          (println (send body (get-data)))))
      (let ((task-id (handler server msg path query)))
        (if task-id
            (println " -> <task {0}>" task-id)
            (log-msg-result #f msg))))))

(define (log-msg-result task-id msg)
  (let ((prefix (if task-id
                    (ssubst "<task {0}>" task-id)
                    "")))
    (println "{0} -> {1} {2}" prefix (send msg (get 'status-code)) (send msg (get 'reason-phrase)))))

(define (msg-method msg)
  (string->symbol (string-downcase (send msg (get 'method)))))

(define (handle-get/head msg code renderer)
  (let ((method (msg-method msg)))
    (if (memq method '(get head))
        (receive (content-type content) (renderer)
          (case method
            ((head)
             (add-response-headers!
              msg
              "Content-Length" (number->string (bytevector-length content))))
            (else
             (send msg (set-response content-type 'copy content))))
          (send msg (set-status (soup-status code))))
        (send msg (set-status (soup-status 'not-implemented))))))

(define root-pathname (make-pathname '/ '() #f))

(define (test-task yield)
  (sleep-seconds 1)
  (yield 'test-1)
  (sleep-seconds 2)
  (yield 'test-2)
  (sleep-seconds 3)
  `((h1 "Test task finished")))

(define (handle-page base-url msg code title proc . args)
  (handle-get/head
   msg code
   (lambda ()
     (values "text/html"
             (string->utf8
              (xhtml-page base-url title (apply proc args))))))
  #f)

(define (irclogs-handler defer-task irclogs)
  (define (invoke msg-name)
    (lambda args
      (apply irclogs msg-name args)))
  (lambda (server msg path query)
    (define (handle-task title proc)
      (send server (pause-message msg))
      (defer-task title msg proc))
    (let* ((pathname (x->pathname path))
           (comps (pathname-directory pathname))
           (n-comps (length comps))
           (base-url (irclogs 'base-url)))
      (define (not-found)
        (handle-page base-url msg 'not-found "Page not found" render-error-page 'not-found)
        (send msg (set-status (soup-status 'not-found)))
        #f)
      (cond ((pathname=? pathname root-pathname)
             (handle-page base-url msg 'ok  (page-title)
                          (invoke 'render-overview/html) (current-year) #f #f))
            ((pathname-file pathname)
             (or (and (pathname=? (pathname-with-file pathname #f) root-pathname)
                      (let ((filename (file-namestring pathname)))
                        (cond ((string=? filename "test.scm")
                               (handle-task "Test task" test-task))
                              (else
                               #f))))
                 (let ((uri (send (send msg (get-uri)) (to-string #f))))
                   (send (send msg (get-response-headers))
                     (append "Location" (string-append uri "/")))
                   (send msg (set-status (soup-status 'moved-permanently)))
                   #f)))
            ((or (and (= n-comps 1)
                      (irclogs 'render-overview/html (current-year) (car comps) #f))
                 (and (= n-comps 2)
                      (irclogs 'render-overview/html (current-year) (car comps) (cadr comps)))
                 (and (= n-comps 3)
                      (apply irclogs 'render-log/html comps)))
             => (lambda (sxml)
                  (handle-page base-url msg 'ok (apply page-title comps) (lambda () sxml))))
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
    ("txt" . "text/plain")))

(define (file-content-type pathname)
  (or (assoc-ref *file-types* (file-type (pathname-file pathname)))
      "application/octet-stream"))

(define (add-response-headers! msg . hdrs)
  (let ((resp-hdrs (send msg (get-response-headers))))
    (for-each (lambda (hdr/val)
                (send resp-hdrs (append (car hdr/val) (cadr hdr/val))))
              hdrs)))

(define (static-file-handler base strip)
  (lambda (server msg path query)
    (define (ok!)
      (send msg (set-status (soup-status 'ok))))
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
               (case (msg-method msg)
                 ((get)
                  (call-with-port (open-file-input-port (x->namestring pathname))
                    (lambda (port)
                      (send msg (set-response content-type 'copy (get-bytevector-all port)))
                      (ok!))))
                 ((head)
                  (add-response-headers!
                   msg
                   "Content-Type" content-type
                   "Content-Length" (number->string (file-size-in-bytes pathname)))
                  (ok!))
                 (else
                  (send msg (set-status (soup-status 'not-implemented)))))))
            (else
             (send msg
               (set-status (soup-status
                            (if (and pathname (file-exists? pathname)) 'forbidden 'not-found)))))))
    #f))

(define (xhtml-page base-url title sxml)
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
   (call-with-string-output-port
     (lambda (port)
       (sxml->xml
        `(html (^ (xmlns "http://www.w3.org/1999/xhtml")
                  (lang "en")
                  (xml:lang "en"))
               (head
                (meta (^ (name "GENERATOR")
                         (content "irc2html.sps by MJ Ray, hacked by Andreas Rottmann")))
                (title ,title)
                (link (^ (rel "stylesheet") (type "text/css") (charset "utf-8") (media "all")
                         (href ,(string-append base-url "static/screen.css"))))
                (style (^ (type "text/css"))
                  ,(string-concatenate
                    (append
                     (list "\n")
                     (map
                      (lambda (x)
                        (string-substitute ".n<0> { background: #<1> }\n"
                                           (list x (integer->color x))
                                           'abrackets))
                      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))))))
               (body ,@sxml))
        port)))))

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

(define (println fmt . args)
  (string-substitute #t fmt args 'braces)
  (newline))

(define (ssubst fmt . args)
  (string-substitute #f fmt args 'braces))

(define (bail-out msg . args)
  (string-substitute (current-error-port) msg args)
  (newline (current-error-port)))

(main (command-line))
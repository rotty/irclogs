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
        (only (spells strings) string-join string-concatenate)
        (only (spells lists) unfold drop)
        (spells string-substitute)
        (spells pathname)
        (spells filesys)
        (spells time-lib)
        (spells foreign)
        (spells tracing)
        (sxml simple)
        (sbank soup)
        (sbank typelib)
        (sbank ctypes basic)
        (irclogs))

(soup-setup!)
(typelib-import
 (prefix (only ("GLib" #f)
               thread-init
               main-loop-new main-loop-run
               timeout-add-seconds
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

(define (irclog-httpd config)
  (g-thread-init #f)
  (let ((port (car (assq-ref config 'port))))
    (parameterize ((null-ok-always-on? #t)) ;; Needed for field access, will go away
      (let ((server (send <soup-server> (new/props 'port port
                                                   'server-header "irclog-httpd")))
            (irclogs (make-irclogs (assq-ref config 'irclogs))))
        (unless server
          (bail-out "Unable to bind to server port {0}\n" port))
        (irclogs 'update-state)
        (g-timeout-add-seconds
         60
         (lambda (user-data) (irclogs 'update-state) #t)
         (integer->pointer 0)) ;; Workaround, will go away
        (send server
          (add-handler #f (wrap-handler (irclogs-handler irclogs)))
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
      (handler (string->symbol (string-downcase method)) msg path query)
      (println " -> {0} {1}" (send msg (get 'status-code)) (send msg (get 'reason-phrase))))))

(define (handle-get/head method msg code renderer)
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
      (send msg (set-status (soup-status 'not-implemented)))))

(define (irclogs-handler irclogs)
  (lambda (method msg path query)
    (define (handle-page code title message . args)
      (handle-get/head
       method msg code
       (lambda ()
         (values "text/html"
                 (string->utf8
                  (xhtml-page irclogs title
                              (if (procedure? message)
                                  (apply message args)
                                  (apply irclogs message args))))))))
    (let* ((pathname (x->pathname path))
           (comps (pathname-directory pathname))
           (n-comps (length comps)))
      (cond ((pathname=? pathname (make-pathname '/ '() #f))
             (handle-page 'ok  (page-title) 'render-overview/html (current-year) #f #f))
            ((pathname-file pathname)
             (let ((uri (send (send msg (get-uri)) (to-string #f))))
               (send (send msg (get-response-headers))
                 (append "Location" (string-append uri "/")))
               (send msg (set-status (soup-status 'moved-permanently)))))
            ((or (and (= n-comps 1)
                      (irclogs 'render-overview/html (current-year) (car comps) #f))
                 (and (= n-comps 2)
                      (irclogs 'render-overview/html (current-year) (car comps) (cadr comps)))
                 (and (= n-comps 3)
                      (apply irclogs 'render-log/html comps)))
             => (lambda (sxml)
                  (handle-page 'ok (apply page-title comps) (lambda () sxml))))
            (else
             (handle-page 'not-found "Page not found" render-error-page 'not-found)
             (send msg (set-status (soup-status 'not-found))))))))

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
  (lambda (method msg path query)
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
               (case method
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
                            (if (and pathname (file-exists? pathname)) 'forbidden 'not-found)))))))))

(define (xhtml-page irclogs title sxml)
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
                         (href ,(string-append (irclogs 'base-url) "static/screen.css"))))
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
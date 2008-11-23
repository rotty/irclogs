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
(import (rnrs)
        (rnrs r5rs)
        (spells receive)
        (spells parameter)
        (only (spells strings) string-join)
        (spells string-substitute)
        (spells pathname)
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
               markup-escape-text)
         g-)
 (prefix (only ("Soup" #f) <server> status-get-phrase)
         soup-))

(define (main argv)
  (let ((default-port 8001))
    (receive (log-dir port)
             (case (length argv)
               ((1) (values "." default-port))
               ((2) (values (cadr argv) default-port))
               ((3) (values (cadr argv) (string->number (caddr argv))))
               (else
                (bail-out "usage: irclog-httpd.sps [directory [port]]")))
      (irclog-httpd (make-irclogs `((log-dir ,log-dir))) port))))

(define (irclog-httpd irclogs port)
  (g-thread-init #f)
  (parameterize ((null-ok-always-on? #t)) ;; Needed for field access, will go away
    (let ((server (send <soup-server> (new/props 'port port
                                                 'server-header "simple-httpd"))))
      (unless server
        (bail-out "Unable to bind to server port {0}\n" port))
      (irclogs 'update-state)
      (send server
        (add-handler #f (irclogs-handler irclogs))
        (run-async))
      (println "Waiting for requests...")
      (g-main-loop-run (g-main-loop-new #f #t)))))

;; Note that `user-data' will go away when I get around to implement hiding it
(define (irclogs-handler irclogs)
  (lambda (server msg path query client user-data)
    (let ((method (send msg (get 'method))))
      (println "{0} {1} HTTP/1.{2}" method  path (send msg (get 'http-version)))
      (send (send msg (get-request-headers))
        (foreach (lambda (name value user-data)
                   (println "{0}: {1}" name value))))
      (let ((body (send msg (get-request-body))))
        (when (> (send body (get-length)) 0)
          (println (send body (get-data)))))
      (cond ((member method '("GET" "HEAD"))
             (do-get irclogs (string->symbol (string-downcase method)) msg path query))
            (else
             (send msg (set-status (soup-status 'not-implemented)))))
      (println " -> {0} {1}" (send msg (get 'status-code)) (send msg (get 'reason-phrase))))))

(define (do-get irclogs method msg path query)
  (define (handle-page code title message . args)
    (let ((content (xhtml-page title (if (procedure? message)
                                         (apply message args)
                                         (apply irclogs message args)))))
      (case method
        ((head)
         (send (send msg (get-response-headers))
           (append "Content-Length" (number->string (bytevector-length content)))))
        (else
         (send msg (set-response "text/html" 'copy content)))))
    (send msg (set-status (soup-status code))))
  (let ((pathname (x->pathname path)))
    (cond ((pathname=? pathname (make-pathname '/ '() #f))
           (handle-page 'ok "IRC activity overview" 'render-overview/html))
          ((pathname-file pathname)
           (let ((uri (send (send msg (get-uri)) (to-string #f))))
             (send (send msg (get-response-headers))
               (append "Location" (string-append uri "/")))
             (send msg (set-status (soup-status 'moved-permanently)))))
          (else
           (handle-page 'not-found "Page not found" render-error-page 'not-found)
           (send msg (set-status (soup-status 'not-found)))))))

(define (xhtml-page title sxml)
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
                (style (^ (type "text/css"))
                  "<!--
                           body { background: #fff; color: #000; }
                           a:link,a:visited,a:active { background: #0ff; color: #006; }
                           #foot { font-size: x-small; }
                           "
                  ,@(map
                     (lambda (x)
                       (string-substitute ".n<0> { background: #<1> }\n"
                                          (list x (integer->color x))
                                          'abrackets))
                     '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
             
                  "
             .meta { color: #999 }
             // -->"))
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
;;; irclog-httpd.sps -- HTTP Server providing an interface to IRC logs

;; Copyright (C) 2008, 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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
        (only (srfi :1) unfold)
        (spells alist)
        (spells pathname)
        (spells string-utils)
        (ocelotl net soup-httpd)
        (ocelotl net httpd basic-handlers)
        (ocelotl net httpd file-directory-handlers)
        (irclogs webapp))

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
    (path-prefix ())
    (static-prefix ("static"))
    
    (irclogs

     ;; This URL is used to refer to the served content
     (base-url "/")
     (static-url "static/")

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

(define (config-ref config name)
  (car (assq-ref config name)))

(define (config->httpd-options config)
  (make-httpd-options with-port (config-ref config 'port)
                      with-interface (config-ref config 'interface)))

(define (irclogs-handler irclogs config)
  (let ((path-prefix (config-ref config 'path-prefix))
        (static-files (config-ref config 'static-files))
        (static-prefix (config-ref config 'static-prefix)))
    (let ((app-handler
           (make-path-prefix-handler
            path-prefix
            (lambda (path request)
              (irclogs 'dispatch path request))
            null-request-handler)))
      (if static-files
          (make-path-prefix-handler
           static-prefix
           (rooted-file-handler static-files)
           app-handler)
          app-handler))))

(define (irclog-httpd config)
  (let ((irclogs (make-irclogs (assq-ref config 'irclogs))))
    (httpd 
     (with-request-handler (irclogs-handler irclogs config)
       (config->httpd-options config))
     (lambda (httpd)
       (irclogs 'update-state)
       (httpd/add-timeout httpd 60 (lambda ()
                                     (irclogs 'update-state)
                                     #t))))))

(define (bail-out msg . args)
  (string-substitute (current-error-port) msg args 'braces)
  (newline (current-error-port))
  (exit 1))

(main (command-line))

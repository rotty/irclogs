;;; page.sls --- 

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is still quite ad-hoc, regarding the use of the "meta" block;
;; probably it would be better to use a data structure similiar to
;; Tekuti's requests.

;;; Code:
#!r6rs

(library (irclogs page)
  (export shtml-response-page
          not-found-response-page)
  (import (rnrs)
          (only (srfi :1) filter-map iota)
          (only (srfi :13) string-concatenate)
          (spells match)
          (spells string-utils)
          (xitomatl ssax extras)
          (spenet http)
          (spenet httpd responses))

(define (shtml-response-page irclogs request shtml)
  (make-http-response
   (http-request/version request)
   (http-status ok)
   #f
   `((content-type . "text/html; charset=utf-8"))
   (lambda (iport oport httpd)
     (put-sxml-response-body/tasks
      httpd
      oport
      xhtml-doctype
      (match shtml
        ((('meta . meta) . body)
         (shtml-page-template irclogs
                              (make-head (irclogs 'static-url) meta)
                              body))
        (body
         (shtml-page-template irclogs '() body)))))))

(define (not-found-response-page irclogs request)
  (make-http-response
   (http-request/version request)
   (http-status not-found)
   #f
   `((content-type . "text/html; charset=utf-8"))
   (lambda (iport oport httpd)
     (put-sxml-response-body
      oport
      xhtml-doctype
      (shtml-page-template irclogs '() `((p "Not found")))))))

(define (make-head static-url meta)
  `(,@(filter-map
       (lambda (entry)
         (and (pair? entry)
              (case (car entry)
                ((js-include)
                 `(script (^ (type "text/javascript")
                             (src ,(string-append static-url (cadr entry))))
                          "//" ; force end tag
                          ))
                ((js-text)
                 `(script (^ (type "text/javascript"))
                          ,(lambda (port)
                             (display (js-cdata (cadr entry)) port))))
                (else
                 entry))))
       meta)))

(define (shtml-page-template irclogs head body)
  (define (static name)
    (string-append (irclogs 'static-url) name))
  
  `(html (^ (xmlns "http://www.w3.org/1999/xhtml")
            (lang "en")
            (xml:lang "en"))
         (head
          (meta
           (^ (name "GENERATOR")
              (content "IRClogs by Andreas Rottmann, based on code by MJ Ray")))
          ,@head
          (link
           (^ (rel "stylesheet") (type "text/css") (charset "utf-8") (media "all")
              (href ,(static "common.css"))))
          (link
           (^ (rel "stylesheet") (type "text/css") (charset "utf-8") (media "all")
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
         (body (div (^ (id "wrap"))
                    ,@body))))

(define (js-cdata str)
  (xml-cdata-escape str
                    (lambda (s)
                      (string-append "/* <![CDATA[ */\n" s "\n /* ]]> */"))))

(define (integer->bits num base) ; int int -> (int ...)
  (if (= num 0)
      '()
      (cons (mod num base) (integer->bits (div num base) base))))

(define colors '("e" "c" "a"))

(define (integer->color num) ; int -> str
  (let ((nb (append (integer->bits num 3) '(0 0 0))))
    (string-append
     (list-ref colors (list-ref nb 0))
     (list-ref colors (list-ref nb 1))
     (list-ref colors (list-ref nb 2)))))

)

;; Local Variables:
;; scheme-indent-styles: ((match 1))
;; End:

;;; utils.sls --- Utilities for the irclogs system.

;; Copyright (C) 2008, 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (irclogs utils)
  (export
    list-intersperse
    
    make-timer start-timer

    time-*
    date-with-zone-offset
    mk-date
    date+days date+days->time-utc
    parse-date unparse-date
    todays-date
    date-day=?

    in-stream
    
    ssubst fprintf println
    url-escape
    trim-path

    pathname-add-file-type
    pathname-has-file-type?
    pathname-strip-file-type
    
    ident-sre
    sexp->alist-matcher
    
    irregex-submatches
    irregex-submatch-alist
    
    host-impl-info-shtml
   )
  (import (except (rnrs)
                  string-copy string->list
                  string-titlecase string-downcase string-upcase
                  string-hash string-for-each)
          (only (srfi :1) drop-while drop-right last)
          (srfi :8 receive)
          (srfi :13 strings)
          (srfi :14 char-sets)
          (srfi :19 time)
          (srfi :26 cut)
          (srfi :39 parameters)
          (wak riastreams)
          (wak irregex)
          (spells opt-args)
          (spells alist)
          (spells misc)
          (spells string-utils)
          (spells pathname)
          (spells time-lib)
          (spells tracing)
          (ocelotl net uri))

  (define (list-intersperse lst item)
    (if (null? lst)
        lst
        (let loop ((lst (cdr lst)) (result (cons (car lst) '())))
          (if (null? lst)
              (reverse result)
              (loop (cdr lst) (cons* (car lst) item result))))))
  
  (define (make-timer)
    (let ((start-time (current-time))
          (stop-time #f))
      (define (elapsed)
        (time-difference (or stop-time (current-time)) start-time))
      (letrec ((timer (case-lambda
                        ((action)
                         (case action
                           ((start)
                            (set! start-time (current-time))
                            (set! stop-time #f))
                           ((stop) (set! stop-time (current-time)))
                           ((step) (set! start-time (add-duration (current-time) (elapsed))))
                           ((elapsed)
                            (let ((diff (elapsed)))
                              (+ (time-second diff) (/ (time-nanosecond diff) #e1e9))))))
                        (()
                         (timer 'elapsed)))))
        timer)))

  (define (start-timer)
    (let ((timer (make-timer)))
      (timer 'start)
      timer))

  (define (time-* n time)
    (make-time (time-type time) (* n (time-nanosecond time)) (* n (time-second time))))

  (define (date-with-zone-offset date tz-offset)
    (make-date (date-nanosecond date)
               (date-second date)
               (date-minute date)
               (date-hour date)
               (date-day date)
               (date-month date)
               (date-year date)
               tz-offset))

  (define (mk-date year month day)
    (make-date 0 0 0 0 day month year 0))

  (define one-day (make-time time-duration 0 (* 24 60 60)))

  (define (date+days->time-utc date n-days)
    (let ((step (time-* n-days one-day)))
      (add-duration (date->time-utc date) step)))

  (define (date+days date n-days)
    (time-utc->date (date+days->time-utc date n-days) 0))

  (define (todays-date zone-offset)
    (let ((now (current-date zone-offset)))
      (make-date 0 0 0 0 (date-day now) (date-month now) (date-year now)
                 zone-offset)))

  (define (date-day=? d1 d2)
    (and (= (date-day d1) (date-day d2))
         (= (date-month d1) (date-month d2))
         (= (date-year d1) (date-year d2))))

  (define isodate-fmt "~Y-~m-~d")

  (define (unparse-date date)
    (date->string date isodate-fmt))

  (define (parse-date s)
    (cond ((string=? s "today")
           (todays-date 0))
          (else
           (guard (c (#t #f))
             (date-with-zone-offset (string->date s isodate-fmt) 0)))))
  
  (define (println fmt . args)
    (string-substitute #t fmt args 'braces)
    (newline))

  (define (ssubst fmt . args)
    (string-substitute #f fmt args 'braces))

  (define (fprintf port fmt . args)
    (string-substitute port fmt args 'braces))

  (define scheme-impl-urls
    '((ikarus . "http://www.cs.indiana.edu/~aghuloum/ikarus/")
      (mzscheme . "http://racket-lang.org/")))

  (define (host-impl-info-shtml)
    (let* ((impl (scheme-implementation))
           (url (assq-ref scheme-impl-urls (scheme-implementation)))
           (impl-name (ssubst "{0} Scheme" (string-titlecase (symbol->string impl)))))
      (if url
          `(a (^ (href ,url)) ,impl-name)
          impl-name)))

  (define url-escape
    (let ((safe-cs (char-set-union char-set:letter
                                   char-set:digit
                                   (string->char-set "$-_.+!*'(),/"))))
      (define (encode code)
        (string-append "%" (number->string code 16)))
      (lambda (s safe-add)
        (let ((safe-cs (char-set-union safe-cs (string->char-set safe-add))))
          (str-escape
           (lambda (c)
             (if (char-set-contains? safe-cs c)
                 (string c)
                 (let ((code (char->integer c)))
                   (cond ((< code 256)
                          (encode code))
                         (else
                          (let ((utf8 (bytevector->u8-list (string->utf8 (string c)))))
                            (string-concatenate (map encode utf8))))))))
           s)))))

  (define (str-escape escaper str)
    (string-concatenate-reverse
     (string-fold (lambda (c parts)
                    (cons (escaper c) parts))
                  '()
                  str)))

  (define (pathname-add-file-type pathname type)
    (let ((file (pathname-file pathname)))
      (pathname-with-file pathname
                          (make-file (file-name file)
                                     (append (file-types file) (list type))))))
  
  (define (trim-path path)
    (reverse
     (drop-while (lambda (elt)
                   (string=? elt ""))
                 (reverse
                  (drop-while (lambda (elt)
                                (string=? elt ""))
                              path)))))

  (define (irregex-submatch-alist match names)
    (map (lambda (name)
           (cons name (irregex-match-substring match name)))
         names))

  (define (irregex-submatches match names)
    (map (cut irregex-match-substring match <>) names))

  (define (sexp->alist-matcher expr)
    (define (submatcher mapper)
      (let ((sub-matchers (map sexp->alist-matcher (cdr expr))))
        (lambda (vals)
          (mapper (lambda (m) (m vals)) sub-matchers))))
    (case (car expr)
      ((and) (submatcher and-map))
      ((or)  (submatcher or-map))
      (else
       (let ((rx (irregex (cadr expr))))
         (lambda (vals)
           (let ((val (assq-ref vals (car expr))))
             (irregex-match rx val)))))))

  (define ident-sre '(+ (or alnum #\- #\_ #\* #\+)))

  (define (pathname-has-file-type? pathname type)
    (let ((file (pathname-file pathname)))
      (and=> (and file (file-type file)) (cut string=? type <>))))

  ;; Taken from conjure, probably makes sense to put into spells
  (define (pathname-strip-file-type pathname type)
    (let* ((file (pathname-file pathname))
           (types (file-types file)))
      (if (and (not (null? types))
               (equal? (last types) type))
          (pathname-with-file pathname
                              (make-file (file-name file) (drop-right types 1)))
          pathname)))
)

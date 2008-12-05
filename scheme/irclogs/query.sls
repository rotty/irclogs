;;; query.sls --- Query string handling.

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

;; This library parses the query string provided by the HTML form.

;;; Code:

(library (irclogs query)
  (export parse-query)
  (import (rnrs)
          (xitomatl srfi and-let*)
          (xitomatl irregex)
          (only (spells lists) any unfold filter-map)
          (only (spells strings) string-contains)
          (spells alist)
          (spells misc)
          (irclogs parse)
          (irclogs utils))

  (define (parse-query q base-date n-days)
    (let ((parts
           (let ((port (open-string-input-port q)))
             (let loop ((i 0) (parts '()))
               (guard (c ((or (i/o-read-error? c) (lexical-violation? c))
                          (reverse (cons (substring q i (string-length q))
                                         parts))))
                 (let* ((i (port-position port))
                        (datum (get-datum port)))
                   (if (eof-object? datum)
                       (reverse parts)
                       (loop i (cons datum parts)))))))))
      (define (parts-ref key)
        (any (lambda (part)
               (and (pair? part)
                    (eq? key (car part))
                    (pair? (cdr part))
                    (null? (cddr part))
                    (cadr part)))
             parts))
      (values (or (and-let* ((d (parts-ref 'date)))
                    (parse-date d))
                  base-date)
              (or (and-let* ((d (parts-ref 'days)))
                    (and (integer? d) d))
                  n-days)
              (query-parts->matcher parts))))

  (define (query-parts->matcher parts)
    (define (->str x)
      (cond ((symbol? x) (symbol->string x))
            ((string? x) x)
            (else        #f)))
    (define (msg-str-matcher s)
      (lambda (entry)
        (string-contains (irc-log-entry-message entry) s)))
    (define (msg-irx-matcher x)
      (guard (c (#t #f))
        (let ((irx (irregex x)))
          (lambda (entry)
            (irregex-search irx (irc-log-entry-message entry))))))
    (define (nick-str-matcher x)
      (and-let* ((s (->str x)))
        (lambda (entry)
          (and-let* ((nick (irc-log-entry-nick entry)))
            (string=? nick s)))))
    (let ((submatchers (filter-map (lambda (part)
                                     (cond ((pair? part)
                                            (case (car part)
                                              ((rx)
                                               (msg-irx-matcher (cadr part)))
                                              ((nick)
                                               (nick-str-matcher (cadr part)))
                                              (else
                                               #f)))
                                           ((->str part)
                                            => (lambda (s)
                                                 (msg-str-matcher s)))))
                                   parts)))
      (lambda (entry)
        (and-map (lambda (match)
                   (match entry))
                 submatchers))))

  )


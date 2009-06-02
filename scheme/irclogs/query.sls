;;; query.sls --- Query string handling.

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

;; This library parses the query string provided by the HTML form.

;;; Code:
#!r6rs

(library (irclogs query)
  (export query->search
          search->query

          search-base-date
          search-n-days
          search-match-expr
          search-matcher
          redate-search

          match-expr-id
          match-expr-tag
          match-expr-children
          match-expr-matcher

          match-expr->shtml)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :1 lists) any unfold filter-map concatenate)
          (only (srfi :13 strings) string-contains string-join)
          (srfi :19 time)
          (xitomatl irregex)
          (spells alist)
          (spells misc)
          (spells tracing)
          (spells match)
          (fmt)
          (irclogs parse)
          (irclogs utils))

  (define-record-type search
    (fields base-date n-days match-expr))

  (define (search-matcher search)
    (match-expr-matcher (search-match-expr search)))

  ;; Return a search with @2 as new base date, and the number of days
  ;; reduced, as to reach back to the same date as @1.
  (define (redate-search search base-date)
    (make-search base-date
                 (- (search-n-days search)
                    (exact
                     (truncate
                      (- (date->julian-day
                          (search-base-date search))
                         (date->julian-day base-date)))))
                 (search-match-expr search)))

  (define-record-type match-expr
    (fields id tag children matcher))

  (define (query->search q base-date n-days)
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
      (make-search
       (or (and-let* ((d (parts-ref 'date)))
                      (parse-date d))
                    base-date)
       (or (and-let* ((d (parts-ref 'days)))
             (and (integer? d) d))
           n-days)
       (query-parts->match-expr parts))))

  (define (search->query search)
    (url-escape
     (call-with-string-output-port
       (lambda (port)
         (write (list 'date (date->string (search-base-date search) "~1")) port)
         (display " " port)
         (write (list 'days (search-n-days search)) port)
         (display " " port)
         (write
          (match-expr->sexp (search-match-expr search))
          port)))
     ""))

  (define (match-expr->sexp me)
    (cond ((and (match-expr? me)
                (match-expr-tag me))
           => (lambda (tag)
                (cons tag (map match-expr->sexp (match-expr-children me)))))
          ((match-expr? me)
           (car (match-expr-children me)))
          (else
           me)))

  (define (match-exprs/ids first-id lst)
    (let loop ((match-exprs '()) (i first-id) (lst lst))
      (define (handle-leaf match-expr)
        (if match-expr
            (loop (cons match-expr match-exprs) (+ i 1) (cdr lst))
            (loop match-exprs i (cdr lst))))
      (define (handle-compound maker children)
        (receive (next-id sub-exprs) (match-exprs/ids i children)
          (loop (cons (maker sub-exprs) match-exprs) next-id (cdr lst))))
      (if (null? lst)
          (values i (reverse match-exprs))
          (match (car lst)
            (('rx rx)      (handle-leaf (msg-rx-match-expr i rx)))
            (('nick nick)  (handle-leaf (nick-str-match-expr i nick)))
            (('and . subs) (handle-compound and-match-expr subs))
            (elt
             (cond ((->str elt)
                    => (lambda (s)
                         (handle-leaf (msg-str-match-expr i s))))
                   (else
                    (loop match-exprs i (cdr lst)))))))))

  (define (query-parts->match-expr parts)
    (let*-values (((next-id subs) (match-exprs/ids 0 parts))
                  ((and-subs other-subs)
                   (partition (lambda (sub)
                                (eq? (match-expr-tag sub) 'and))
                              subs)))
      (and-match-expr (append (concatenate
                               (map match-expr-children and-subs))
                              other-subs))))

  (define (and-match-expr children)
    (make-match-expr
     #f
     'and
     children
     (let ((submatchers (map match-expr-matcher children)))
       (lambda (entry)
         (and-map (lambda (match)
                    (match entry))
                  submatchers)))))

  (define (msg-str-match-expr id s)
    (and (string? s)
         (make-match-expr
          id
          #f
          (list s)
          (lambda (entry)
            (string-contains (irc-log-entry-message entry) s)))))

  (define (msg-rx-match-expr id x)
    (guard (c (#t #f))
      (let ((irx (irregex x)))
        (make-match-expr
         id
         'rx
         (list x)
         (lambda (entry)
           (irregex-search irx (irc-log-entry-message entry)))))))

  (define (nick-str-match-expr id x)
    (and-let* ((s (->str x)))
      (make-match-expr
       id
       'nick
       (list x)
       (lambda (entry)
         (and-let* ((nick (irc-log-entry-nick entry)))
           (string=? nick s))))))

  (define (->str x)
    (cond ((symbol? x) (symbol->string x))
          ((string? x) x)
          (else        #f)))


;;; UI/HTML-related functionality follows

  (define (match-expr->shtml m)
    (define (children-shtml)
      (map match-expr->shtml (match-expr-children m)))
    (define (children-text)
      (string-join (map (lambda (child)
                          (fmt #f (wrt child)))
                        (match-expr-children m))
                   " "))
    (define (shtml-list)
      (cond ((and (not (match-expr-id m)) (match-expr-tag m))
             => (lambda (tag)
                  `(("(" ,tag " " ,@(children-shtml) ")"))))
            ((match-expr-tag m)
             => (lambda (tag)
                  `(("(" ,tag " " ,(children-text) ")"))))
            (else (list (children-text)))))
    (cond ((match-expr-id m)
           => (lambda (id)
                `(span (^ (class ,(id->css-class id)))
                       ,@(shtml-list))))
          (else
           (shtml-list))))

  (define (id->css-class id)
    (string-append "me-l" (number->string (mod id 10))))

  )

;; Local Variables:
;; scheme-indent-styles: ((match 1))
;; End:

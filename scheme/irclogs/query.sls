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
#!r6rs

(library (irclogs query)
  (export query->search

          search-base-date
          search-n-days
          search-match-expr
          search-matcher

          match-expr-id
          match-expr-tag
          match-expr-children
          match-expr-matcher

          match-expr->shtml)
  (import (rnrs)
          (xitomatl srfi and-let*)
          (xitomatl irregex)
          (spells receive)
          (only (spells lists) any unfold filter-map)
          (only (spells strings) string-contains string-join)
          (spells alist)
          (spells misc)
          (spells tracing)
          (fmt)
          (irclogs parse)
          (irclogs utils))

  (define-record-type search
    (fields base-date n-days match-expr))

  (define (search-matcher search)
    (match-expr-matcher (search-match-expr search)))

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

  (define (match-exprs/ids first-id lst)
    (let loop ((match-exprs '()) (i first-id) (lst lst))
      (define (handle-leaf match-expr)
        (if match-expr
            (loop (cons match-expr match-exprs) (+ i 1) (cdr lst))
            (loop match-exprs i (cdr lst))))
      (if (null? lst)
          (values i (reverse match-exprs))
          (cond ((and (pair? (car lst))
                      (pair? (cdar lst))
                      (null? (cddar lst)))
                 (case (caar lst)
                   ((rx)   (handle-leaf (msg-rx-match-expr i (cadar lst))))
                   ((nick) (handle-leaf (nick-str-match-expr i (cadar lst))))
                   (else   (loop match-exprs i (cdr lst)))))
                ((->str (car lst))
                 => (lambda (s)
                      (handle-leaf (msg-str-match-expr i s))))
                (else
                 (loop match-exprs i (cdr lst)))))))

  (define (query-parts->match-expr parts)
    (receive (next-id children) (match-exprs/ids 0 parts)
      (make-match-expr
       #f
       'and
       children
       (let ((submatchers (map match-expr-matcher children)))
         (lambda (entry)
           (and-map (lambda (match)
                      (match entry))
                    submatchers))))))

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

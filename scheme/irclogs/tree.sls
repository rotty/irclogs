;;; tree.sls --- Handle a tree of IRC log files

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

;;; Code:
#!r6rs

(library (irclogs tree)
  (export make-log-tree
          fold-log-tree
          open-log-stream)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1) concatenate split-at)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :13) string-concatenate)
          (srfi :19 time)
          (spells alist)
          (spells misc)
          (spells lazy)
          (spells lazy-streams)
          (spells gc)
          (spells filesys)
          (spells irregex)
          (spells pathname)
          (spells gzip)
          (spells tracing)
          (irclogs utils)
          (irclogs parse))

(define-record-type log-tree
  (fields dir struct))

(define (fold-log-tree tree restrictions proc . seeds)
  (define (file-combiner file-entry loc rxs placeholders . seeds)
    (let ((without-gz (pathname-strip-file-type file-entry "gz")))
      (cond ((and-let* (((null? (cdr rxs)))
                        (m (irregex-match (car rxs) (file-namestring without-gz))))
               (and (file-readable? file-entry)
                    ;; Prefer non-gzipped version, if existing
                    (or (eq? without-gz file-entry)
                        (not (file-exists? without-gz))
                        (not (file-readable? without-gz)))
                    m))
             => (lambda (match)
                  (let ((vals (concatenate
                               (cons (irregex-submatch-alist
                                      match
                                      (car placeholders))
                                     loc))))
                    (receive new-seeds (apply proc vals file-entry seeds)
                      (apply values #t loc rxs placeholders new-seeds)))))
            (else
             (apply values #t loc rxs placeholders seeds)))))
  (define (dir-combiner dir-entry loc rxs placeholders . seeds)
    (cond ((and (not (null? (cdr rxs)))
                (irregex-match (car rxs) (file-namestring dir-entry)))
           => (lambda (match)
                (apply values
                       #t
                       #t
                       (lambda (old-loc old-rxs old-placeholders . new-seeds)
                         (apply values #t loc rxs placeholders new-seeds))
                       (cons (irregex-submatch-alist match
                                                     (car placeholders))
                             loc)
                       (cdr rxs)
                       (cdr placeholders)
                       seeds)))
          (else
           (apply values #t #f #f loc rxs placeholders seeds))))
  (let* ((struct (log-tree-struct tree))
         (log-tree-rxs (map (lambda (part)
                              (construct-rx part restrictions))
                            struct))
         (placeholders (map extract-placeholders (log-tree-struct tree))))
    (receive (loc rxs placeholders . seeds)
             (apply
              directory-fold-tree*
              (log-tree-dir tree)
              file-combiner
              dir-combiner
              '()
              log-tree-rxs
              placeholders
              seeds)
      (apply values seeds))))

(define (open-log-file tree tag channel date)
  (let ((path (log-path tree tag channel date)))
    (or (and (file-exists? path)
             (file-readable? path)
             (transcoded-port (open-file-input-port (x->namestring path))
                              (native-transcoder)))
        (and-let* ((path-gz (pathname-add-file-type path "gz"))
                   ((file-exists? path-gz))
                   ((file-readable? path-gz)))
          (transcoded-port (open-gz-file-input-port path-gz)
                           (native-transcoder))))))

(define (log-path tree tag channel date)
  (vals->pathname (log-tree-dir tree)
                  `((tag . ,tag)
                    (channel . ,channel)
                    (year . ,(num->str (date-year date) 4))
                    (month . ,(num->str (date-month date) 2))
                    (day . ,(num->str (date-day date) 2)))
                  (log-tree-struct tree)))

(define log-port-guardian (make-guardian))

(define (open-log-stream tree tag channel date)
  ;; Close ports not in use anymore
  (do ((port (log-port-guardian) (log-port-guardian)))
      ((not port))
    (close-port port))
  (let ((port (open-log-file tree tag channel date)))
    (cond (port
           (log-port-guardian port)
           (port->irc-log-entry-stream port date))
          (else
           #f))))

;;; Utilities

(define sre-alist
  `((year . (>= 4 digit))
    (month . (** 1 2 digit))
    (day .   (** 1 2 digit))
    (tag . ,ident-sre)
    (channel . (: (+ "#") ,ident-sre))))

(define (construct-rx part restrictions)
  (define (->sre x)
    (cond ((symbol? x)
           `(submatch-named ,x
                            ,(or (assq-ref restrictions x)
                                 (assq-ref sre-alist x)
                                 (error 'construct-rx "unknown shorthand" x))))
          (else x)))
  (if (pair? part)
      (sre->irregex (cons ': (map ->sre part)))
      (sre->irregex (->sre part))))

(define (extract-placeholders part)
  (cond ((pair? part)
         (filter symbol? part))
        ((symbol? part)
         (list part))
        (else
         '())))

(define (resolve-template template vals)
  (cond ((symbol? template)
         (assq-ref vals template))
        ((pair? template)
         (string-concatenate (map (lambda (part) (resolve-template part vals)) template)))
        (else
         template)))

(define (num->str n width)
  (let* ((s (number->string n))
         (len (string-length s)))
    (if (> width len)
        (string-append (make-string (- width len) #\0) s)
        s)))

(define (vals->pathname base vals template)
  (receive (dir-parts file-part)
           (split-at (map (lambda (part) (resolve-template part vals)) template)
                     (- (length template) 1))
    (make-pathname (pathname-origin base)
                   (append (pathname-directory base) dir-parts)
                   (if (pair? (car file-part))
                       (string-concatenate (car file-part))
                       (car file-part)))))

)

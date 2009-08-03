;;; cache.sls --- Handling the cache for the log files

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

;; TODO: Make `tree-struct' not have `year' as an implicit first
;; component.

;;; Code:
#!r6rs

(library (irclogs cache)
  (export make-cache
          update-cache
          cache-get)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1) fold iota)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :19 time)
          (spells alist)
          (spells foof-loop)
          (spells match)
          (only (spells record-types)
                define-record-type*
                define-functional-fields)
          (spells irregex)
          (spells filesys)
          (spells pathname)
          (spells string-utils)
          (irclogs parse)
          (irclogs tree)
          (irclogs utils))

(define-record-type* cache
  (make-cache cache-dir log-dir tree-struct fender)
  ((last-update #f)))

(define-functional-fields cache
  cache-dir log-dir tree-struct fender)

(define iso-date+time-fmt "~Y-~m-~d ~H:~M:~S ~z")

(define (update-cache cache)
  (receive (cache-dir log-dir tree-struct fender)
           (cache-components cache)
    (let* ((update-file (pathname-with-file cache-dir "last-update"))
           (last-update
            (or (cache-last-update cache)
                (read-time-utc update-file)))
          (now (current-date 0)))
      (create-directory* cache-dir)
      (for-each
       (match-lambda
        (((year tag channel) . changed-logs)
         (let ((state-file (state-file-pathname cache-dir year tag channel)))
           (println "{0} {1}/{2}: {3} days updated"
                    year tag channel (length changed-logs))
           (call-with-output-file/atomic state-file
             (lambda (port)
               (write (update-state
                       (if (file-exists? state-file)
                           (call-with-input-file (x->namestring state-file) read)
                           '())
                       year
                       changed-logs)
                      port))))))
       (log-tree-update-list log-dir tree-struct last-update fender))
      (call-with-output-file/atomic update-file
        (lambda (port)
          (write (date->string now iso-date+time-fmt) port))))))

(define (read-time-utc update-file)
  (and-let* (((file-exists? update-file))
             (date-str (call-with-input-file (x->namestring update-file)
                         read)))
    (date->time-utc (string->date date-str iso-date+time-fmt))))

(define (state-file-pathname cache-dir year tag channel)
  (pathname-with-file cache-dir
                      (make-file (ssubst "{0}-{1}-{2}" year tag channel)
                                 "state")))

;; Calculate the list of files that have changed since `last-update'
;; and need their state files be updated.
;; The returned list has the following structure:
;; (((<year> <tag> <channel>) ((<month> <day>) <path>) ...) ...)
(define (log-tree-update-list log-dir tree-struct last-update match?)
  (define (update-entry vals path lst)
    (if (and (or (not match?)
                 (match? vals))
             (or (not last-update)
                 (time>? (file-modification-time path) last-update)))
        (let ((key (list (assq-ref vals 'year)
                         (assq-ref vals 'tag)
                         (assq-ref vals 'channel)))
              (month/day (list (and-let* ((m (assq-ref vals 'month)))
                                 (string->number m))
                               (and-let* ((d (assq-ref vals 'day)))
                                 (string->number d)))))
          (println "* {0} modified, adding to update list" (x->namestring path))
          (cond ((assoc key lst)
                 => (lambda (entry)
                      (cons (cons key (cons (list month/day path) (cdr entry)))
                            (filter (lambda (elt)
                                      (not (eq? elt entry)))
                                    lst))))
                (else
                 (cons (cons key (list (list month/day path))) lst))))
        lst))
  (if last-update
      (fold (lambda (year entries)
              (fold-log-tree log-dir
                             (cons (number->string year) tree-struct)
                             (lambda (vals path lst)
                               (update-entry (cons `(year . ,year) vals) path lst))
                             entries))
            '()
            (year-range (time-utc->date last-update 0) (todays-date 0)))
      (fold-log-tree log-dir (cons 'year tree-struct) update-entry '())))

(define (year-range start-date end-date)
  (let ((start-year (date-year start-date))
        (end-year (date-year end-date)))
    (iota (+ (- end-year start-year) 1) start-year)))

(define (cache-get cache tag channel base-date n-days)
  (let* ((start-time (date+days->time-utc base-date (- n-days)))
         (start-date (time-utc->date start-time 0))
         (end-time (date->time-utc base-date))
         (end-date base-date)
         (cache-dir (cache-cache-dir cache))
         (fender (cache-fender cache)))
    (define (date-match? year)
      (or (eqv? base-date #f) (<= (date-year start-date) year (date-year end-date))))
    (define (filter-days year entries)
      (if (not base-date)
          entries
          (filter (lambda (entry)
                    (match-let (((month day) (car entry)))
                      (let ((entry-time (date->time-utc (mk-date year month day))))
                        (and (time<=? start-time entry-time)
                             (time<=? entry-time end-time)))))
                  entries)))
    (define (state-file-combiner entry state)
      (if (and (pathname-has-file-type? entry "state")
               (file-readable? entry))
          (receive (st-year st-tag st-channel) (parse-state-pathname entry)
            (if (and st-year st-tag st-channel
                     (date-match? st-year)
                     (or (eqv? tag #f)  (string=? tag st-tag))
                     (or (eqv? channel #f) (string=? channel st-channel))
                     (fender `((year . ,st-year)
                               (tag . ,st-tag)
                               (channel . ,st-channel))))
                (let ((entries
                       (filter-days st-year
                                    (call-with-input-file (x->namestring entry)
                                      read))))
                  (cons (cons* st-year st-tag st-channel entries) state))
                state))
          state))
    (cond ((file-exists? cache-dir)
           (directory-fold cache-dir state-file-combiner '()))
          (else
           (create-directory* cache-dir)
           '()))))

(define (update-state state year changed-logs)
  (let ((changed-days (map car changed-logs)))
    (append (map (match-lambda
                  (((and (month day) key) changed-file)
                   (cons key (log-file-status changed-file
                                              (mk-date year month day)))))
                 changed-logs)
            (filter (lambda (entry)
                      (not (member (car entry) changed-days)))
                    state))))

(define (log-file-status path date)
  (call-with-input-file (x->namestring path)
    (lambda (port)
      (loop ((for entry (in-stream (port->irc-log-entry-stream port date)))
             (with count 0 (if (member (irc-log-entry-type entry) '("<" "*"))
                               (+ count 1)
                               count)))
        => `((message-count ,count))))))

(define parse-state-pathname
  (let ((rx (sre->irregex `(: (submatch-named year (+ digit)) "-"
                              (submatch-named tag ,ident-sre) "-"
                              (submatch-named channel (: (+ "#" ,ident-sre)))
                              ".state"))))
    (lambda (pathname)
      (cond ((irregex-match rx (file-namestring pathname))
             => (lambda (match)
                  (receive (year tag channel)
                           (apply values
                                  (irregex-submatches match '(year tag channel)))
                    (values (string->number year) tag channel))))
            (else
             (values #f #f #f))))))
)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:

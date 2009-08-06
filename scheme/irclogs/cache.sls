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
          (spells tracing)
          (irclogs parse)
          (irclogs tree)
          (irclogs utils))

(define-record-type* cache
  (make-cache cache-dir tree fender)
  ())

(define-functional-fields cache
  cache-dir tree fender)

(define iso-date+time-fmt "~Y-~m-~d ~H:~M:~S ~z")

(define (update-cache cache)
  (receive (cache-dir tree fender)
           (cache-components cache)
    (let* ((update-file (pathname-with-file cache-dir "last-update"))
           (last-update (read-time-utc update-file))
          (now (current-date 0)))
      (create-directory* cache-dir)
      (for-each
       (match-lambda
        (((year tag channel) . changed-days)
         (let ((state-file (state-file-pathname cache-dir year tag channel)))
           (println "{0} {1}/{2}: {3} days updated"
                    year tag channel (length changed-days))
           (call-with-output-file/atomic state-file
             (lambda (port)
               (write (update-state
                       (if (file-exists? state-file)
                           (call-with-input-file (x->namestring state-file) read)
                           '())
                       tree
                       tag
                       channel
                       year
                       changed-days)
                      port))))))
       (log-tree-update-list tree last-update fender))
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
;; (((<year> <tag> <channel>) (<month> <day>) ...) ...)
(define (log-tree-update-list tree last-update match?)
  (define (update-entry vals path lst)
    (if (and (or (not match?)
                 (match? vals))
             (or (not last-update)
                 (time>? (file-modification-time path) last-update)))
        (let ((key (list (string->number (assq-ref vals 'year))
                         (assq-ref vals 'tag)
                         (assq-ref vals 'channel)))
              (month/day (list (string->number (assq-ref vals 'month))
                               (string->number (assq-ref vals 'day)))))
          (println "* {0} modified, adding to update list" (x->namestring path))
          (cond ((assoc key lst)
                 => (lambda (entry)
                      (cons (cons key (cons month/day (cdr entry)))
                            (filter (lambda (elt)
                                      (not (eq? elt entry)))
                                    lst))))
                (else
                 (cons (cons key (list month/day)) lst))))
        lst))
  (let ((restrictions
         (if last-update
             `((year . ,(year-range-sre (time-utc->date last-update 0)
                                        (todays-date 0))))
             '())))
    (fold-log-tree tree
                   restrictions
                   update-entry
                   '())))

(define (year-range-sre start-date end-date)
  (loop ((for year (up-from (date-year start-date) (to (+ (date-year end-date) 1))))
         (for result (listing (number->string year))))
    => `(or ,@result)))

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

(define (update-state state tree tag channel year changed-days)
  (define (log-status date)
    (loop ((for entry (in-stream (open-log-stream tree tag channel date)))
           (with count 0 (if (member (irc-log-entry-type entry) '("<" "*"))
                             (+ count 1)
                             count)))
      => `((message-count ,count))))
  (append (map (match-lambda
                ((and (month day) m/d)
                 (cons m/d (log-status (mk-date year month day)))))
               changed-days)
          (filter (lambda (entry)
                    (not (member (car entry) changed-days)))
                  state)))

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

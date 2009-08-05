;;; window.sls --- "sliding window" for searching with context

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

(library (irclogs window)
  (export make-search-window
          search-window-add-entry
          search-window->hit
          search-hit-index
          search-hit-entries
          search-hit-size
          search-hit-first-date
          search-hit-last-date
          search-hit-matches
          in-search-hits)
  (import (rnrs)
          (srfi :8 receive)
          (srfi :19 time)
          (spells foof-loop)
          (spells lazy-streams)
          (spells tracing)
          (spenet wt-tree)
          (irclogs utils)
          (irclogs parse))

(define time-wt-type (make-wt-tree-type time<?))

(define empty-tree (make-wt-tree time-wt-type))

(define-record-type (search-window %make-search-window search-window?)
  (fields context matcher counter matches hit-time entries))

(define (make-search-window context matcher)
  (%make-search-window context matcher 0 #f #f empty-tree))

(define-record-type search-hit
  (fields index matches entries))

(define (search-hit-size hit)
  (vector-length (search-hit-entries hit)))

(define (search-hit-first-date hit)
  (irc-log-entry-date (vector-ref (search-hit-entries hit) 0)))

(define (search-hit-last-date hit)
  (let ((entries (search-hit-entries hit)))
    (irc-log-entry-date (vector-ref entries (- (vector-length entries) 1)))))

(define (search-window->hit window)
  (let ((hit-time (search-window-hit-time window))
        (entries (search-window-entries window)))
    (and hit-time
         (make-search-hit
          (wt-tree/rank entries hit-time)
          (search-window-matches window)
          (let ((vec (make-vector (wt-tree/size entries))))
            (wt-tree/fold (lambda (time entry i)
                            (vector-set! vec i entry)
                            (- i 1))
                          (- (wt-tree/size entries) 1)
                          entries)
            vec)))))

(define (empty-search-window window)
  (make-search-window (search-window-context window)
                      (search-window-matcher window)))

;; We abuse the (otherwise unused) nanosecond time field to get unique
;; time stamps within a search window
(define (uniquify-time time counter)
  (make-time (time-type time) (mod counter 10e9) (time-second time)))

(define (deuniquify-time time)
  (make-time (time-type time) 0 (time-second time)))

(define (search-window-add-entry window entry)
  (let ((entries (search-window-entries window))
        (hit-time (search-window-hit-time window))
        (context (search-window-context window))
        (matcher (search-window-matcher window))
        (counter (search-window-counter window))
        (matches (search-window-matches window))
        (time (irc-log-entry-time-utc entry)))
    (let ((new-matches (matcher entry)))
      (define (slide-window hit-time entry-time)
        (values
          (%make-search-window
           context
           matcher
           (+ counter 1)
           (or new-matches matches)
           hit-time
           (wt-tree/add (wt-tree/split>
                         entries
                         (subtract-duration (or hit-time time) context))
                        entry-time
                        entry))
          #f))
      (cond
        ((eqv? time #f)
         (values window #f))
        ((and hit-time
              (or new-matches (time>? (time-difference time hit-time) context)))
         ;; Context exceeded; returns a new window with `entry' being
         ;; the (only) initial entry, as well as a `search hit'
         ;; record corresponding to the old window
         (values
           (%make-search-window context
                                matcher
                                1
                                new-matches
                                (if new-matches time #f)
                                (wt-tree/add empty-tree time entry))
           (search-window->hit window)))
        (new-matches
         (let ((new-hit-time (uniquify-time time counter)))
           (slide-window new-hit-time new-hit-time)))
        (else
         (slide-window hit-time (uniquify-time time counter)))))))

(define-syntax in-search-hits
  (syntax-rules ()
    ((_ (hit-var window-var counter-var) (window-expr stream-expr) cont . env)
     (cont
      ()                       ;Outer bindings
      ((window-var window-expr ;Loop variables
                   new-window)
       (counter-var 0 new-counter)
       (stream stream-expr new-stream))
      (((new-window hit-var new-counter new-stream) ;Entry bindings
        (loop continue ((for entry s (in-stream stream))
                        (with window window-var)
                        (with counter counter-var (+ counter 1)))
          => (values (empty-search-window window)
                     (search-window->hit window)
                     counter
                     s)
          (receive (new-window hit)
                   (search-window-add-entry window entry)
            (if hit
                (values new-window hit counter (stream-cdr s))
                (continue (=> window new-window)))))))
      ((not hit-var) (stream-null? stream)) ;Termination conditions
      ()                                    ;Body bindings
      (((counter-var) new-counter))         ;Final bindings
      . env))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:

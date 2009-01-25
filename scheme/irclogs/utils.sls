;;; utils.sls --- Utilities for the irclogs system.

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

;;; Code:
#!r6rs

(library (irclogs utils)
  (export

   make-timer start-timer

   time-*
   date-with-zone-offset
   mk-date
   date+days date+days->time-utc
   parse-date unparse-date
   fold-days-between

   current-yield yield/c

   make-scheduler scheduler? scheduler-work scheduler-enqueue!
   scheduler-has-work?

   ssubst fprintf println

   host-impl-info-shtml
   )
  (import (rnrs)
          (srfi :8 receive)
          (spells opt-args)
          (spells alist)
          (spells queue)
          (spells time-lib)
          (spells parameter)
          (spells misc)
          (spells string-substitute)
          (spells tracing))

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

  (define *one-day* (make-time time-duration 0 (* 24 60 60)))

  (define (date+days->time-utc date n-days)
    (let ((step (time-* n-days *one-day*)))
      (add-duration (date->time-utc date) step)))

  (define (date+days date n-days)
    (time-utc->date (date+days->time-utc date n-days) 0))

  (define isodate-fmt "~Y-~m-~d")

  (define (unparse-date date)
    (date->string date isodate-fmt))

  (define (parse-date s)
    (guard (c (#t #f))
      (date-with-zone-offset (string->date s isodate-fmt) 0)))

  (define (fold-days-between start-day end-day proc . seeds)
    (let* ((start (date->time-utc start-day))
           (end (add-duration (date->time-utc end-day) *one-day*)))
      (receive (step time-cmp?)
               (if (time<? start end)
                   (values *one-day* time>=?)
                   (values (time-* -1 *one-day*) time<=?))
        (let loop ((cur start) (seeds seeds))
          (if (time-cmp? cur end)
              (apply values seeds)
              (let ((cur-date (time-utc->date cur 0)))
                (loop (add-duration cur step)
                      (receive new-seeds (apply proc cur-date seeds) new-seeds))))))))

  (define current-yield (make-parameter #f))

  (define (yield/c v)
    ((current-yield) v))

  (define-record-type scheduler
    (fields
     (mutable tasks))
    (protocol (lambda (p)
                (lambda ()
                  (p (empty-queue))))))

  (define *engine-escape* #f)
  (define *engine-entrance* #f)

  (define (yield . vals)
    (call/cc
     (lambda (k)
       (apply *engine-escape* k vals))))

  (define make-engine
    (lambda (proc)
      (lambda (success failure)
        (let ((engine-succeeded? #f))
          (receive (resume . results)
                   (call/cc
                    (lambda (k)
                      (set! *engine-escape* k)
                      (receive results
                               (call/cc
                                (lambda (k)
                                  (set! *engine-entrance* k)
                                  (receive vals (proc yield)
                                    (apply *engine-entrance* vals))))
                        (set! engine-succeeded? #t)
                        (apply values #f results))))
            (if engine-succeeded?
                (apply success results)
                (failure
                 (make-engine
                  (lambda (yield)
                    (resume 'resume)))
                 (apply values results))))))))

  (define (scheduler-work scheduler consumer resume?)
    (and (scheduler-has-work? scheduler)
         (receive (task remaining-tasks) (queue-remove (scheduler-tasks scheduler))
           (scheduler-tasks-set! scheduler remaining-tasks)
           (task consumer
                 (lambda (e . args)
                   (if (apply resume? args)
                       (scheduler-tasks-set! scheduler
                                             (queue-insert (scheduler-tasks scheduler) e)))))
           #t)))

  (define (scheduler-has-work? scheduler)
    (not (queue-empty? (scheduler-tasks scheduler))))

  (define (scheduler-enqueue! scheduler proc)
    (scheduler-tasks-set! scheduler
                          (queue-insert (scheduler-tasks scheduler)
                                        (make-engine proc))))

  (define (println fmt . args)
    (string-substitute #t fmt args 'braces)
    (newline))

  (define (ssubst fmt . args)
    (string-substitute #f fmt args 'braces))

  (define (fprintf port fmt . args)
    (string-substitute port fmt args 'braces))

  (define scheme-impl-urls
    '((ikarus . "http://www.cs.indiana.edu/~aghuloum/ikarus/")))

  (define (host-impl-info-shtml)
    (let* ((dialect (scheme-dialect))
           (url (assq-ref scheme-impl-urls dialect))
           (impl-name (ssubst "{0} Scheme" (string-titlecase (symbol->string dialect)))))
      (if url
          `(a (^ (href ,url)) ,impl-name)
          impl-name)))

  )

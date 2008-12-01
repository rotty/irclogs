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

(library (irclogs utils)
  (export make-timer start-timer

          make-scheduler scheduler? scheduler-work scheduler-enqueue!
          scheduler-has-work?)
  (import (rnrs)
          (spells receive)
          (spells opt-args)
          (spells queue)
          (spells time-lib)
          (only (spells assert) cout)
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

  )
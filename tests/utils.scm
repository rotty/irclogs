;;; utils.scm --- Unit tests for (irclogs utils)

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

;;; Code:

(define (multi-step-task n name)
  (lambda (yield)
    (let loop ((i 0))
      (cond ((< i n)
             (yield (list name i))
             (loop (+ i 1)))
            (else
             (list name 'done))))))

(define (single-step-task name)
  (multi-step-task 1 name))

(define (scheduler-test-1)
  (let ((scheduler (make-scheduler)))
    (scheduler-enqueue! scheduler (multi-step-task 3 'task-1))
    (scheduler-enqueue! scheduler (multi-step-task 2 'task-2))
    (scheduler-enqueue! scheduler (single-step-task 'task-3))
    (let ((result '()))
      (let loop ()
        (if (scheduler-work scheduler
                            (lambda (v)
                              (set! result (cons v result)))
                            (lambda (v)
                              (set! result (cons v result))
                              #t))
            (loop)))
      (reverse result))))

(define-test-suite scheduler-tests
  "Scheduler")

(define-test-case scheduler-tests task-interleaving ()
  (test-equal '((task-1 0) (task-2 0) (task-3 0)
                (task-1 1) (task-2 1) (task-3 done)
                (task-1 2) (task-2 done)
                (task-1 done))
              (scheduler-test-1)))

(run-test-suite scheduler-tests)

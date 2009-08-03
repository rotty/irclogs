;;; query.scm --- Unit tests for query string handling.

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

(define (minutes->duration m)
  (make-time time-duration 0 (* m 60)))

(define (q-dt q)
  (let ((s (query->search q (mk-date 2008 12 5) 42 (minutes->duration 5))))
    (list (date->string (search-base-date s) "~1")
          (search-n-days s))))

(define (q-m q)
  (let ((s (query->search q (mk-date 2008 12 5) 42 (minutes->duration 5))))
    (lambda (str)
      ((search-matcher s)
       (make-irc-log-entry (current-date) 0 "" "dogbert" str)))))

(define (bool x)
  (if x #t #f))

(define-test-suite query-tests
  "Query handling")

(define-test-case query-tests default ()
  (test-equal '("2008-12-05" 42) (q-dt ""))
  (test-equal '("2008-12-05" 43) (q-dt "(days 43)")))

(define-test-case query-tests matching ()
  (let ((m1? (q-m ") foo bar")))
    (test-eqv #t (bool (m1? ") foo bar")))
    (test-eqv #t (bool (m1? " ) foo bar ")))
    (test-eqv #f (bool (m1? " foo bar")))))

(run-test-suite query-tests)

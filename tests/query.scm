;;; query.scm --- Unit tests for query string handling.

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

(define (q-dt q)
  (receive (date days match?) (parse-query q (mk-date 2008 12 5) 42)
    (list (unparse-date date) days)))

(define (q-m q)
  (receive (date days match?) (parse-query q (mk-date 2008 12 5) 42)
    (lambda (s)
      (match? (make-irc-log-entry 0 0 0 "" "dogbert" s)))))

(define (bool x)
  (if x #t #f))

(testeez "query defaulst"
  (test/equal "default propagation" (q-dt "") '("2008-12-05" 42))
  (test/equal "default override (days)" (q-dt "(days 43)") '("2008-12-05" 43)))

(let ((m1? (q-m ") foo bar")))
  (testeez "match"
    (test-true "m1 (equiv)" (bool (m1? ") foo bar")))
    (test-true "m1 (middle)" (bool (m1? " ) foo bar ")))
    (test-false "m1 (no)" (bool (m1? " foo bar")))))

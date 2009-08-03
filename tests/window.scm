;;; window.scm --- Tests for sliding search window

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

(define (nick-matcher nick)
  (lambda (entry)
    (and=> (irc-log-entry-nick entry) (lambda (n) (string=? n nick)))))

(define test-log
  (map (match-lambda
        ((h m s nick msg)
         (make-irc-log-entry (make-date 0 s m h 1 1 1970 0) 0 ">" nick msg)))
       '((09 59 56 "zippy" "yow")
         (10 00 00 "dogbert" "wuff"))))

(define (hit->list hit)
  (list (search-hit-index hit)
        (map (lambda (entry)
               (list (irc-log-entry-nick entry)
                     (irc-log-entry-message entry)))
             (vector->list (search-hit-entries hit)))))

(define (maybe-cons-hit hit lst)
  (if hit
      (cons (hit->list hit) lst)
      lst))

(define (simple-search window log)
  (loop continue ((for entry (in-list log))
                  (with window window)
                  (with hit-data '()))
    => (reverse (maybe-cons-hit (search-window->hit window) hit-data))
    (receive (new-window hit) (search-window-add-entry window entry)
      (continue (=> window new-window)
                (=> hit-data (maybe-cons-hit hit hit-data))))))

(define-test-suite window-tests
  "Sliding search window")

(define-test-case window-tests hit-last ()
  (let ((window (make-search-window (make-time time-duration 0 10)
                                    (nick-matcher "dogbert"))))
    (test-equal '((1 (("zippy" "yow") ("dogbert" "wuff"))))
      (simple-search window test-log))))

(run-test-suite window-tests)

;; Local Variables:
;; scheme-indent-styles: (foof-loop trc-testing)
;; End:

;;; parse.sls --- Parse IRC logs

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


(library (irclogs parse)
  (export irc-log-entry-hours
          irc-log-entry-minutes
          irc-log-entry-seconds
          irc-log-entry-type
          irc-log-entry-nick
          irc-log-entry-message
          fold-irc-log-file
          parse-irc-log-file)
  (import (rnrs)
          (xitomatl srfi and-let*)
          (spells receive)
          (spells pathname)
          (spells misc)
          (spells tracing)
          (xitomatl irregex))

  (define ident-sre '(+ (or alnum #\- #\_ #\* #\+)))

  (define log-formats
    (map
     (lambda (entry)
       (cons (sre->irregex (car entry))
             (cdr entry)))
     `(((: (submatch-named hours (** 1 2 digit)) ":" (submatch-named minutes (** 1 2 digit))
           " " (submatch-named type (+ (~ white))) " "
           (submatch-named nick ,ident-sre) (? (or ":" ">"))
           (submatch-named line (* any)))
        (hours minutes #f type nick line))
       ((: (submatch-named hours (** 1 2 digit)) ":" (submatch-named minutes (** 1 2 digit))
           ":" (submatch-named seconds (** 1 2 digit))
           " " (submatch-named type (or "<" "*"))
           (submatch-named nick ,ident-sre) ">"
           (submatch-named line (* any)))
        (hours minutes seconds type nick line)))))

  (define-record-type irc-log-entry
    (fields hours minutes seconds type nick message))
  
  (define (parse-line str)              ; str -> list
    (define (submatches match names)
      (map (lambda (name)
             (if (or (symbol? name) (integer? name))
                 (irregex-match-substring match name)
                 name))
           names))
    (or (or-map (lambda (irx/tmpl)
                  (and-let* ((match (irregex-match (car irx/tmpl) str)))
                    (apply make-irc-log-entry (submatches match (cadr irx/tmpl)))))
                log-formats)
        (make-irc-log-entry #f #f #f #f #f str)))

  (define (parse-irc-log-file port)
    (reverse (fold-irc-log-file port cons '())))
  
  (define (fold-irc-log-file port combiner . seeds)
    (let loop ((seeds seeds))
      (let ((line (get-line port)))
        (if (eof-object? line)
            (apply values seeds)
            (receive new-seeds (apply combiner (parse-line line) seeds)
              (loop new-seeds)))))))

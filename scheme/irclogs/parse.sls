;;; parse.sls --- Parse IRC logs

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
#!r6rs

(library (irclogs parse)
  (export make-irc-log-entry
          irc-log-entry-hours
          irc-log-entry-minutes
          irc-log-entry-seconds
          irc-log-entry-type
          irc-log-entry-nick
          irc-log-entry-message
          read-irc-log-line)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (spells pathname)
          (spells misc)
          (spells tracing)
          (xitomatl irregex))

  (define special-sre '("[]\\`_^{|}"))
  (define nick-sre `(: (or alpha ,special-sre) (* (or alnum ,special-sre "-"))))

  (define tm-h-m-s-sre '(: (submatch-named hours (** 1 2 digit)) ":"
                           (submatch-named minutes (** 1 2 digit)) ":"
                           (submatch-named seconds (** 1 2 digit))))
  (define log-formats
    (map
     (lambda (entry)
       (cons (sre->irregex (car entry))
             (cdr entry)))

     `(
       ;; format used by irssi
       ((: (submatch-named hours (** 1 2 digit)) ":" (submatch-named minutes (** 1 2 digit))
           (+ white) (submatch-named type (+ (~ white))) (+ white)
           (submatch-named nick ,nick-sre) (? (or ":" ">")) (+ white)
           (submatch-named line (* any)))
        (hours minutes #f type nick line))
       ;; format of http://tunes.org/~nef/logs/scheme/
       ((: ,tm-h-m-s-sre (+ white)
           (submatch-named type (or "*" "<")) (? white)
           (submatch-named nick ,nick-sre) (? ">") (+ white)
           (submatch-named line (* any)))
        (hours minutes seconds type nick line))
       ((: ,tm-h-m-s-sre (+ white) "---" (+ white) (+ (~ white)) ":" (+ white)
           (submatch-named nick ,nick-sre) (+ white)
           (submatch-named line (* any)))
        (hours minutes seconds "---" nick line))
       )))

  (define-record-type irc-log-entry
    (fields hours minutes seconds type nick message))

  (define (parse-line str)
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

  ;;@ Read a line from @1, returning either an irc-log-entry, or the
  ;;eof-object.
  (define (read-irc-log-line port)
    (let ((line (get-line port)))
      (if (eof-object? line)
          line
          (parse-line line)))))

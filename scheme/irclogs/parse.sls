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
          irc-log-entry-date
          irc-log-entry-type
          irc-log-entry-nick
          irc-log-entry-message
          irc-log-entry-time-utc
          irc-log-reader)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :19 time)
          (spells pathname)
          (spells misc)
          (spells match)
          (spells tracing)
          (xitomatl irregex))

  (define special-sre '("[]\\`_^{|}"))
  (define nick-sre `(: (or alpha ,special-sre) (* (or alnum ,special-sre "-"))))

  (define tm-h-m-s-sre '(: (submatch-named hour (** 1 2 digit)) ":"
                           (submatch-named minute (** 1 2 digit)) ":"
                           (submatch-named second (** 1 2 digit))))
  (define log-formats
    (map
     (lambda (entry)
       (cons (sre->irregex (car entry))
             (cdr entry)))

     `(
       ;; format used by irssi
       ((: (submatch-named hour (** 1 2 digit))
           ":" (submatch-named minute (** 1 2 digit))
           (+ white) (submatch-named type (+ (~ white))) (+ white)
           (submatch-named nick ,nick-sre) (? (or ":" ">")) (+ white)
           (submatch-named message (* any)))
        (hour minute #f type nick message))
       ;; format of http://tunes.org/~nef/logs/scheme/
       ((: ,tm-h-m-s-sre (+ white)
           (submatch-named type (or "*" "<")) (? white)
           (submatch-named nick ,nick-sre) (? ">") (+ white)
           (submatch-named message (* any)))
        (hour minute second type nick message))
       ((: ,tm-h-m-s-sre (+ white) "---" (+ white) (+ (~ white)) ":" (+ white)
           (submatch-named nick ,nick-sre) (+ white)
           (submatch-named message (* any)))
        (hour minute second "---" nick message))
       )))

  (define-record-type irc-log-entry
    (fields date type nick message))

  (define (irc-log-entry-time-utc entry)
    (date->time-utc (irc-log-entry-date entry)))

  (define (date-with-h-m-s date h m s)
    (make-date 0 s h m
               (date-day date)
               (date-month date)
               (date-year date)
               (date-zone-offset date)))
  
  (define (parse-line str date)
    (define (submatches match names)
      (map (lambda (name)
             (if (or (symbol? name) (integer? name))
                 (irregex-match-substring match name)
                 name))
           names))
    (or (or-map (lambda (irx/tmpl)
                  (and-let* ((m (irregex-match (car irx/tmpl) str)))
                    (match-let (((hour minute second type nick message)
                                 (submatches m (cadr irx/tmpl))))
                      (let ((h (string->number hour))
                            (m (string->number minute))
                            (s (or (and=> second string->number) 0)))
                        (make-irc-log-entry (date-with-h-m-s date h m s)
                                            type
                                            nick
                                            message)))))
                log-formats)
        (make-irc-log-entry #f #f #f str)))

  (define (irc-log-reader date)
    (lambda (port)
      (let ((line (get-line port)))
        (if (eof-object? line)
            line
            (parse-line line date))))))


;; Local Variables:
;; scheme-indent-styles: ((match-let 1))
;; End:

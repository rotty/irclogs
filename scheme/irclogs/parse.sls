;;; parse.sls --- Parse IRC logs

;; Copyright (C) 2008-2010 Andreas Rottmann <a.rottmann@gmx.at>

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
          irc-log-entry-count
          irc-log-entry-type
          irc-log-entry-nick
          irc-log-entry-message
          irc-log-entry-time-utc
          read-irc-log-entry
          port->irc-log-entry-stream)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :19 time)
          (srfi :45 lazy)
          (spells pathname)
          (spells misc)
          (spells match)
          (spells gc)
          (wak riastreams)
          (spells tracing)
          (xitomatl irregex)
          (irclogs utils))

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
    (fields date count type nick message))

  (define (irc-log-entry-time-utc entry)
    (and=> (irc-log-entry-date entry) date->time-utc))

  (define (date-with-h-m-s date h m s)
    (make-date 0 s m h
               (date-day date)
               (date-month date)
               (date-year date)
               (date-zone-offset date)))
  
  (define (parse-line str date count)
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
                                            count
                                            type
                                            nick
                                            message)))))
                log-formats)
        (make-irc-log-entry #f count #f #f str)))

  (define (port->irc-log-entry-stream port date)
    (lazy
     (let recur ((count 0))
       (let ((entry (read-irc-log-entry port date count)))
         (if (eof-object? entry)
             stream-nil
             (stream-cons entry (recur (+ count 1))))))))

  (define (read-irc-log-entry port date count)
    (let ((line (get-line port)))
      (if (eof-object? line)
          line
          (parse-line line date count)))))

;; Local Variables:
;; scheme-indent-styles: ((match-let 1))
;; End:

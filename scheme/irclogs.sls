;;; irclogs.sls --- An interface to IRC logs

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
#!r6rs

(library (irclogs)
  (export make-irclogs)
  (import (except (rnrs) file-exists? delete-file
                  list->vector vector->list vector-fill! vector-for-each
                  vector-map)
          (only (srfi :1 lists)
                append-map concatenate count drop
                last make-list split-at)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :13)
                string-concatenate substring/shared)
          (srfi :43 vectors)
          (spells opt-args)
          (spells alist)
          (spells misc)
          (spells time-lib)
          (spells pathname)
          (spells filesys)
          (spells string-utils)
          (spells define-values)
          (spells foof-loop)
          (spells tracing)
          (spells irregex)
          (spells fmt)
          (prometheus)
          (spenet path-dispatch)
          (xitomatl ssax extras)
          (spenet http)
          (spenet httpd responses)
          (spenet utils)
          (irclogs tree)
          (irclogs cache)
          (irclogs parse)
          (irclogs query)
          (irclogs utils)
          (irclogs page))

  (define (msum numbers) ; (int ...) -> int
    (if (null? numbers)
        0
        (+ (* (car numbers) (length numbers)) (msum (cdr numbers)))))

  (define colors '("e" "c" "a"))
  (define color-override
    '()) ;; FIXME: Use config

  (define (word->integer name) ; str -> int
    (let ((override (assoc name color-override)))
      (if override
          (cadr override)
          (msum (map char->integer (string->list name))))))

  (define link-rx (irregex "\\b[a-z]+://[^\\s<>]+"))

  (define (linkify str) ; str -> shtml
    (concatenate
     (reverse
      (irregex-fold link-rx
                    (lambda (i m markup)
                      (let ((url (irregex-match-substring m)))
                        (cons (list (substring/shared
                                     str
                                     i
                                     (irregex-match-start-index m 0))
                                    `(a (^ (href ,url)) ,url))
                              markup)))
                    '()
                    str
                    (lambda (i markup)
                      (cons (list (substring/shared str i (string-length str)))
                            markup))))))

  (define (center sxml)
    `(div (^ (class "centered"))
          ,sxml))

  (define (log-entry->shtml e count nick-entry-count anchor? wrap-time)
    (define (timed-row class data)
      (let* ((h (irc-log-entry-hours e))
             (m (irc-log-entry-minutes e))
             (s (irc-log-entry-seconds e)))
        `(tr (^ (class ,(if (= (mod nick-entry-count 2) 0) "even" "odd"))
                ,@(if anchor?
                      `((id ,(ssubst "e{0}" count)))
                      '()))
             (td (^ (class "time")) ,(wrap-time (ssubst "{0}:{1}" h m) count))
             (td (^ (class ,class)) ,@data))))
    (define (nick-class)
      (ssubst "n{0}" (mod (word->integer (irc-log-entry-nick e)) 26)))
    (let ((kind (cond ((irc-log-entry-type e)
                       => (lambda (type)
                            (cond ((string=? type "<") 'message)
                                  ((string=? type "*") 'action)
                                  (else                'event))))
                      (else
                       'meta)))
          (message (linkify (irc-log-entry-message e))))
      (define (nick-row left right)
        (values (+ nick-entry-count 1)
                (timed-row (nick-class)
                           (list left (irc-log-entry-nick e) right message))))
      (case kind
        ((message)
         (nick-row "<" ">"))
        ((action)
         (nick-row "*" " "))
        ((event)
         (values nick-entry-count
                 (timed-row "event" (list (irc-log-entry-type e) " "
                                          (irc-log-entry-nick e) " " message))))
        (else
         (values nick-entry-count
                 `(tr (td (^ (class "meta") (colspan 2)) ,message)))))))

  (define (filter-fold-log-file/shtml port pred proc anchor? wrap-time . seeds)
    (loop continue ((for entry (in-port port read-irc-log-line))
                    (for count (up-from 0))
                    (with nick-entry-count 0)
                    (with seeds seeds))
      => (apply values count seeds)
      (if (pred entry)
          (let*-values (((n-count shtml)
                         (log-entry->shtml entry count nick-entry-count
                                           anchor? wrap-time))
                        (new-seeds (apply proc shtml seeds)))
            (continue (=> nick-entry-count n-count)
                      (=> seeds new-seeds)))
          (continue))))

  (define (footer self)
    (center
     `(div (^ (id "foot"))
           (p "Powered by the " (a (^ (href ,(self 'homepage-url))) "IRClogs System")
              ", running on " ,(host-impl-info-shtml))
           ,@(self 'footer-sxml)
           #;
           (p (a (^ (href "http://validator.w3.org/check?uri=referer"))
                 (img (^ (src "http://www.w3.org/Icons/valid-xhtml10-blue")
                         (alt "Valid XHTML 1.0 Strict")
                         (height 23)
                         (width 66))))))))

  (define (channel-days-tds self tag channel pin? days prop-vec . args)
    (let-optionals* args ((start 0)
                          (end (vector-length days))
                          (with-day? #f))
      (let ((static-url (self 'static-url))
            (base-url (self 'base-url)))
        (define (text day count)
          (cond ((and with-day? count)
                 (ssubst "{0} ({1})" (caddr day) count))
                (with-day?
                  (ssubst "{0}" (caddr day)))
                (count
                 (ssubst "({0})" count))
                (else
                 "")))
        (define (day-tds day props)
          (append
           (if pin?
               `((td (^ (class "pin"))
                      ,(date-link base-url tag channel (apply mk-date day)
                                  `(img (^ (src ,(string-append static-url "pin.png"))
                                           (alt "v")
                                           (width 16) (height 16)))
                                  '())))
                       '())
           `((td (^ (class "day-info"))
                 ,(cond ((assq-ref props 'message-count)
                         => (lambda (count)
                              (day-link base-url tag channel (apply mk-date day)
                                        (text day (car count)))))
                        (else
                         (text day #f)))))))
        (let loop ((markup '()) (i (- end 1)))
          (if (< i start)
              markup
              (loop (append (day-tds (vector-ref days i) (vector-ref prop-vec i)) markup)
                    (- i 1)))))))

  (define (days-month-borders days . start+end)
    (let-optionals* start+end ((start 0) (end (vector-length days)))
      (let loop ((borders '()) (cur-month #f) (end-i (- end 1)) (i (- end 1)))
        (define (cur-borders)
          (cons (+ i 1) (+ end-i 1)))
        (cond ((< i start)
               (cons (cur-borders) borders))
              ((not cur-month)
               (loop borders (cadr (vector-ref days i)) end-i (- i 1)))
              ((= cur-month (cadr (vector-ref days i)))
               (loop borders cur-month end-i (- i 1)))
              (else
               (loop (cons (cur-borders) borders) (cadr (vector-ref days i)) i (- i 1)))))))

  (define (search-form base-url tag channel q c)
    `(form (^ (id "search")
              (action ,(url-escape (ssubst "{0}{1}/{2}/" base-url tag channel) "/"))
              (accept-charset "utf-8"))
           (input (^ (name "q") (title "What to search for")
                     (size 42) (maxlength 2048)
                     ,@(if q `((value ,q)) '())))
           " with "
           (input (^ (name "c") (title "Context in minutes")
                     (size 3) (maxlength 10)
                     ,@(if c `((value ,c)) '())))
           " minutes of context"
           (br)
           (input (^ (type "submit") (name "search-btn") (value "Search")))))

  (define (channel-monthly-table self tag channel n-columns days prop-vec start end)
    (let ((count (exact (truncate (ceiling (/ (- end start) n-columns))))))
      `(table
        (^ (class "mactivity"))
        ,@(let loop ((markup '()) (i start))
            (if (>= i end)
                (reverse markup)
                (let* ((n-vals (min n-columns (- end i)))
                       (n-empty (if (= count 1) 0 (* 2 (- n-columns n-vals)))))
                  (loop (append
                         `((tr ,@(channel-days-tds self tag channel #t days prop-vec
                                                   i (+ i n-vals) #t)
                               ,@(make-list n-empty '(td))))
                         markup)
                        (+ i n-columns))))))))

  (define (search-log self oport tag channel date search)
    (let ((day-url (day-url (self 'base-url) tag channel date)))
      (define (render-hit shtml first?)
        (when first?
          (sxml->xml
           `(tr (th (^ (colspan 2))
                    (a (^ (href ,day-url)) ,(unparse-date date))))
           oport))
        (sxml->xml shtml oport)
        #f)
      (or
        (and-let* ((log-port (self 'open-log-file tag channel date)))
          (call-with-port log-port
            (lambda (log-port)
              (receive (msg-count first?)
                       (filter-fold-log-file/shtml
                        log-port
                        (search-matcher search)
                        render-hit
                        #f ;; no anchors
                        (lambda (time count)
                          `(a (^ (href ,(ssubst "{0}#e{1}" day-url count)))
                              ,time))
                        #t)
                msg-count))))
        0)))

  (define render-log-js
    "$(document).ready(function() { activate_log_options(); });")

  (define breadcrumbs
    (case-lambda
      ((base-url tag channel date link-last?)
       (define (last-link link . args)
         (if link-last?
             (apply link args)
             (last args)))
       (cond ((and tag channel date)
              `(span ,(base-link base-url)
                     " > " ,(tag-link base-url tag)
                     " > " ,(channel-link base-url tag channel)
                     " > " ,(last-link day-link base-url tag channel date (unparse-date date))))
             ((and tag channel)
              `(span ,(base-link base-url)
                     " > " ,(tag-link base-url tag)
                     " > " ,(last-link channel-link base-url tag channel)))
             (tag
              `(span ,(base-link base-url) " > " ,(last-link tag-link base-url tag)))
             (else
              "IRC activity for all networks")))
      ((base-url tag channel date)
       (breadcrumbs base-url tag channel date #f))))

  (define (month-string date)
    (date->string date "~B ~Y"))

  (define (base-link base-url)
    `(a (^ (href ,base-url)) "IRC"))

  (define (tag-link base-url tag)
    `(a (^ (href ,(url-escape (string-append base-url tag "/") "/"))) ,tag))

  (define (channel-link base-url tag channel)
    `(a (^ (href ,(url-escape (string-append base-url tag "/" channel "/") "/"))) ,channel))

  (define date-link
    (case-lambda
      ((base-url tag channel date text attrs)
       `(a (^ (href ,(string-append
                      (url-escape (ssubst "{0}{1}/{2}/" base-url tag channel) "/")
                      "?date=" (unparse-date date)))
              ,@attrs)
           ,text))
      ((base-url tag channel date)
       (date-link base-url tag channel date (unparse-date date) '()))))

  (define (day-link base-url tag channel date text)
    `(a (^ (href ,(day-url base-url tag channel date))) ,text))

  (define (day-url base-url tag channel date)
    (url-escape (ssubst "{0}{1}/{2}/{3}/"
                                      base-url
                                      tag channel
                                      (unparse-date date))
                              "/"))

  (define (state-sort keys state)
    (list-sort (lambda (x y)
                 (let loop ((keys keys))
                   (let ((i (caar keys)))
                     (cond ((null? keys) #f)
                           (((cadar keys) (list-ref x i) (list-ref y i))
                            #t)
                           (((caddar keys) (list-ref x i) (list-ref y i))
                            (loop (cdr keys)))
                           (else
                            #f)))))
               state))

  (define (state-tabularize state)
    (receive (min-date max-date)
             (let loop ((min-date #f) (max-date #f) (dates (state-dates state)))
               (if (null? dates)
                   (values min-date max-date)
                   (loop (list-select < = min-date (car dates))
                         (list-select > = max-date (car dates))
                         (cdr dates))))
      (let ((days (if (and min-date max-date)
                      (list->vector (days-between min-date max-date))
                      '#())))
        (let loop ((channels '()) (state state))
          (if (null? state)
              (values days (reverse channels))
              (let*-values (((head days-alist) (split-at (car state) 3))
                            ((year tag channel) (apply values head)))
                (cond ((assoc-ref channels (list tag channel))
                       => (lambda (entry)
                            (fill-day-vector! (car entry) days year days-alist)
                            (loop channels (cdr state))))
                      (else
                       (let ((day-vec (make-vector (vector-length days) '())))
                         (fill-day-vector! day-vec days year days-alist)
                         (loop (cons (list (list tag channel) day-vec) channels)
                               (cdr state)))))))))))

  (define (fill-day-vector! vec days year days-alist)
    (do ((alist days-alist (cdr alist)))
        ((null? alist) vec)
      (let ((index (vector-binary-search days (cons year (caar alist)) (list-comparator > =))))
        (unless index
          (error 'fill-day-vector! "out-of-range date encountered" (car alist) days))
        (vector-set! vec index (cdar alist)))))

  (define (list-comparator less? equiv?)
    (lambda (l1 l2)
      (list-compare less? equiv? l1 l2)))

  (define (list-select less? equiv? l1 l2)
    (cond ((not l1) l2)
          ((not l2) l1)
          (else
           (let ((cmp (list-compare less? equiv? l1 l2)))
             (cond ((< cmp 0) l1)
                   ((= cmp 0) l1)
                   (else      l2))))))

  (define (list-compare less? equiv? lst1 lst2)
    (let loop ((l1 lst1) (l2 lst2))
      (cond ((and (null? l1) (null? l2)) 0)
            ((null? l1)                 -1)
            ((null? l2)                  1)
            ((less? (car l1) (car l2))  -1)
            ((equiv? (car l1) (car l2)) (loop (cdr l1) (cdr l2)))
            (else                        1))))

  (define (state-dates state)
    (append-map (lambda (entry)
                  (let ((year (car entry)))
                    (map (lambda (day/props)
                           (cons year (car day/props)))
                         (drop entry 3))))
                state))

  (define (days-between start-day end-day)
    (define (day->date day)
      (make-date 0 0 0 0 (caddr day) (cadr day) (car day) 0))
    (fold-days-between (day->date start-day) (day->date end-day)
                       (lambda (date days)
                         (cons (list (date-year date) (date-month date) (date-day date))
                               days))
                       '()))

  (define (query-date query)
    (let ((val (assq-ref query 'date)))
      (and val
           (parse-date val))))

;;; Code following uses the Prometheus object system

  (define-syntax define-privates
    (syntax-rules ()
      ((define-privates name ...)
       (begin (define name (list 'name)) ...))))

  (define-privates
    %set-log-dir! %set-state-dir!
    %set-dir-struct!
    
    %set-base-url!
    %set-homepage-url!
    %set-static-url!

    %cache
    %matcher %set-matcher!
    %set-search-n-days!
    %set-footer-sxml!)

  (define *irclogs* (*the-root-object* 'clone))

  (define-values (irclogs-dispatcher irclogs-path)
    (path-dispatch-rules
     (((path-arg/string*) (path-arg/string*))
      render-overview)
     (((path-arg/string) (path-arg/string) (path-arg/iso-date))
      render-log)
     (else
      #f)))
  
  (define (make-irclogs options)
    (let ((logs (*irclogs* 'clone)))
      (for-each (lambda (entry)
                  (case (car entry)
                    ((log-dir)   (logs %set-log-dir! (pathname-as-directory (cadr entry))))
                    ((state-dir) (logs %set-state-dir! (pathname-as-directory (cadr entry))))
                    ((dir-struct) (logs %set-dir-struct! (cadr entry)))
                    ((base-url)   (logs %set-base-url! (cadr entry)))
                    ((static-url) (logs %set-static-url! (cadr entry)))
                    ((homepage-url) (logs %set-homepage-url! (cadr entry)))
                    ((match)      (logs %set-matcher!
                                        (sexp->alist-matcher (cadr entry))))
                    ((search-n-days) (logs %set-search-n-days! (cadr entry)))
                    ((footer-sxml) (logs %set-footer-sxml! (cdr entry)))
                    (else
                     (error 'make-irclogs "unknown option" entry))))
                options)
      (logs 'add-value-slot! %cache (make-cache (logs 'state-dir)
                                                (logs 'log-dir)
                                                (logs 'dir-struct)
                                                (logs %matcher)))
      (modify-object! logs
        ((dispatch self resend path request)
         (let* ((file-path? (not (or (null? path)
                                     (string=? (last path) ""))))
                (trimmed-path (trim-path path)))
           (cond ((irclogs-dispatcher trimmed-path)
                  => (lambda (renderer)
                       (if file-path?
                           (make-error-response
                            (http-status moved-perm)
                            request
                            (uri-with-directory-path (http-request/uri request)))
                           (or (and=> (renderer self request)
                                      (lambda (shtml)
                                        (shtml-response-page self
                                                             request
                                                             shtml)))
                               (not-found-response-page self request)))))
                 (else
                  (not-found-response-page self request))))))
      logs))

  (define (render-overview self request tag channel)
    (let* ((query (http-request/uri-query-alist request))
           (base-date (or (query-date query) (current-date 0)))
           (context (or (and=> (assq-ref query 'c) string->number) 5))
           (n-days (cond ((and tag channel) 365)
                         (else                7))))
      (cond
       ((and tag channel (assq-ref query 'q))
        => (lambda (q)
             (render-search-task self tag channel base-date q context)))
       (else
        (let ((state (cache-get (self %cache) tag channel base-date n-days)))
          (receive (days rows)
                   (state-tabularize (state-sort `((1 ,string<? ,string=?)
                                                   (2 ,string<? ,string=?)
                                                   (0 ,< ,=))
                                                 state))
            (let ((n-rows (length rows)))
              (cond
                ((= n-rows 0)
                 #f)
                ((and tag channel)
                 (assert (= n-rows 1))
                 (render-channel-overview self tag channel days (cadar rows)))
                (else
                 (render-multi-overview self tag channel base-date days rows))))))))))
  
  (define (render-multi-overview self tag channel base-date days rows)
    (let ((base-url (self 'base-url))
          (n-days (min 7 (vector-length days))))
      (and (> n-days 0)
           `((meta (title ,(ssubst "IRC activity for {0} channels" tag)))
             (h1 ,(breadcrumbs base-url tag channel #f))
             ,(activity-nav-links self tag channel base-date n-days 7)
             (table
              (^ (class "activity"))
              (thead
               (tr (th "Network") (th "Channel")
                   ,@(map (lambda (date)
                            (receive (year month day) (apply values date)
                              `(th ,(ssubst "{0}-{1}" month day))))
                          (vector->list days 0 n-days))))
              (tbody
               ,@(map (lambda (row)
                        (receive (tag channel) (apply values (car row))
                          `(tr (th ,(tag-link base-url tag))
                               (th ,(channel-link base-url tag channel))
                               ,@(channel-days-tds self tag channel #f days
                                                   (cadr row) 0 n-days))))
                      rows)))
             ,(footer self)))))

  (define (render-channel-overview self tag channel days prop-vec)
    (let ((static-url (self 'static-url))
          (base-url (self 'base-url)))
      `((meta (title ,(ssubst "IRC activity for {0}/{1}" tag channel)))
        (h1 ,(breadcrumbs base-url tag channel #f))
        ,(search-form base-url tag channel #f (self 'default-context))
        ,(activity-nav-links self tag channel
                             (apply mk-date (vector-ref days 0))
                             (vector-length days)
                             365)
        ,@(append-map
           (lambda (month-borders)
             (receive (year month day)
                      (apply values (vector-ref days (car month-borders)))
               (list `(h2 ,(month-string (mk-date year month day)))
                     (channel-monthly-table
                      self tag channel 7 days prop-vec
                      (car month-borders) (cdr month-borders)))))
           (days-month-borders days)))))

  (define (render-log self request tag channel date)
    (let ((base-url (self 'base-url)))
      (and-let* ((log-port (self 'open-log-file tag channel date)))
        `((meta (title
                 ,(ssubst "IRC log for {0}/{1} {2}" tag channel date))
                (js-include "jquery.js")
                (js-include "sitelib.js")
                (js-text ,render-log-js))
          (h1 ,(breadcrumbs base-url tag channel date))
          ,(log-nav-links self tag channel date)
          (form (^ (id "options")
                   (action ,(day-url base-url tag channel date)))
                (span
                 "Events: "
                 (input (^ (type "checkbox") (name "events") (value "on"))))
                (input (^ (type "submit") (name "opt-btn") (value "Apply"))))
          ,(center
            `(table (^ (class "log"))
                    ,(lambda (oport)
                       (call-with-port log-port
                         (lambda (log-port)
                           (filter-fold-log-file/shtml
                            log-port
                            (lambda (entry) #t)
                            (lambda (shtml)
                              (sxml->xml shtml oport)
                              (values))
                            #t
                            (lambda (time count)
                              `(a (^ (href ,(ssubst "#e{0}" count)))
                                  ,time))))))))
          ,(footer self)))))

  (define-method (*irclogs* 'update-state self resend)
    (update-cache (self %cache)))
  
  (define-method (*irclogs* 'open-log-file self resend tag channel date)
    (and
      ;; guard against non-served channels
      ((self %matcher) `((tag . ,tag)
                         (channel . ,channel)
                         (year . ,(date-year date))))
      (open-log-file (self 'log-dir)
                     (cons 'year (self 'dir-struct))
                     tag channel date)))

  (define (render-search-task self tag channel base-date q context)
    (let ((search (query->search q base-date (self 'search-n-days) context)))
      `((meta (title ,(ssubst "Search {0}/{1}" tag channel)))
        (h1 ,(breadcrumbs (self 'base-url) tag channel #f #t))
        ,(search-form (self 'base-url) tag channel q context)
        (div (^ (id "search-desc"))
             "Searching for " (code ,@(match-expr->shtml (search-match-expr search))))
        ,(center
          `(table
            (^ (class "log"))
            (task
             ,(lambda (port yield)
                (call/cc
                  (lambda (finish)
                    (let ((timer (start-timer)))
                      (define (escaper date day-count msg-count)
                        (when (>= (timer) (self 'search-timeout))
                          (finish
                           (render-search-footer self
                                                 tag channel
                                                 day-count msg-count (timer)
                                                 (redate-search search date)))))
                      (receive (day-count msg-count)
                               (day-range-search-task self port
                                                      tag channel search
                                                      escaper
                                                      yield)
                        (render-search-footer self tag channel
                                              day-count msg-count (timer) #f)))))))))
        (task-result)
        ,(footer self))))

  (define (render-search-footer self tag channel day-count msg-count seconds cont-search)
    (center
     `(div (^ (id "timing"))
           ,(ssubst "Searched {0} messages on {1} days in {2} seconds"
                    msg-count
                    day-count
                    (fmt #f (num (inexact seconds) 10 4)))
           (br)
           ,@(if cont-search
                 `((a (^ (href
                          ,(ssubst "{0}{1}/?q={2}"
                                   (self 'base-url)
                                   (url-escape (string-append tag "/" channel) "/")
                                   (search->query cont-search))))
                      "Continue search"))
                 '()))))

  (define (day-range-search-task self port tag channel search escaper yield)
    (let ((base-url (self 'base-url)))
      (fold-days-between
       (date+days (search-base-date search) 1) ;; from base-date at 24:00
       (date+days (search-base-date search)
                  (- (search-n-days search)))  ;; back to (- base-date n-days) at 0:00
       (lambda (date day-count msg-count)
         (let ((day-msg-count (search-log self port tag channel date search)))
           (escaper date day-count msg-count)
           (yield #t)
           (values (+ day-count 1) (+ msg-count day-msg-count))))
       0 0)))

  (define (activity-nav-links self tag channel base-date n-days-shown step)
    (let ((next-date (date+days base-date step))
          (prev-date (date+days base-date (- n-days-shown)))
          (loc (string-concatenate (append (if tag (list tag "/") '())
                                           (if channel (list channel "/"))))))
      (define (date-link date text)
        (let ((url (url-escape (ssubst "{0}{1}?date={2}" (self 'base-url) loc (unparse-date date))
                               "/?=")))
          `(a (^ (href ,url)) ,text)))
      `(div (^ (id "nav"))
            ,(date-link next-date "<<")
            ,(date-link prev-date ">>"))))

  (define (log-nav-links self tag channel date)
    (let ((next-date (date+days date 1))
          (prev-date (date+days date -1))
          (loc (string-append tag "/" channel "/")))
      (define (date-link date text)
        (let ((url (url-escape (string-append (self 'base-url) loc (unparse-date date) "/") "/")))
          `(a (^ (href ,url)) ,text)))
      `(div (^ (id "nav"))
            ,(date-link next-date "<<")
            ,(date-link prev-date ">>"))))

  (*irclogs* 'add-value-slot! 'log-dir %set-log-dir! (make-pathname #f '() #f))
  (*irclogs* 'add-value-slot! 'state-dir %set-state-dir! (make-pathname #f '() #f))
  (*irclogs* 'add-value-slot! 'dir-struct %set-dir-struct!
             '(tag (channel "." month "-" day ".log")))
  (*irclogs* 'add-value-slot! 'base-url %set-base-url! "/")
  (*irclogs* 'add-value-slot! 'static-url %set-static-url! "/static/")
  (*irclogs* 'add-value-slot! 'homepage-url %set-homepage-url! "/static/irclogs.html")
  (*irclogs* 'add-value-slot! %matcher %set-matcher! #f)
  (*irclogs* 'add-value-slot! 'search-n-days %set-search-n-days! 14)
  (*irclogs* 'add-value-slot! 'search-timeout 2)
  (*irclogs* 'add-value-slot! 'default-context 5)
  (*irclogs* 'add-value-slot! 'footer-sxml %set-footer-sxml! '())

  )

;; Local Variables:
;; scheme-indent-styles: ((modify-object! 1) foof-loop)
;; End:

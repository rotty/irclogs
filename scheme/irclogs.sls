(library (irclogs)
  (export make-irclogs)
  (import (except (rnrs)
                  file-exists? delete-file
                  string-copy string->list string-titlecase string-downcase string-upcase
                  string-hash string-for-each
                  list->vector vector->list vector-fill! vector-for-each
                  vector-map)
          (xitomatl srfi and-let*)
          (xitomatl srfi vectors)
          (xitomatl srfi char-set)
          (xitomatl irregex)
          (spells opt-args)
          (spells lists)
          (spells strings)
          (spells receive)
          (spells alist)
          (spells misc)
          (spells time-lib)
          (spells pathname)
          (spells filesys)
          (spells string-substitute)
          (spells tracing)
          (fmt)
          (prometheus)
          (sxml simple)
          (irclogs parse)
          (irclogs utils))

  (define (merge-state state changed-logs)
    (let ((changed-days (map car changed-logs)))
      (append (map (lambda (changed-log)
                     (cons (car changed-log)
                           (log-file-status (cadr changed-log))))
                   changed-logs)
              (filter (lambda (entry)
                        (not (member (car entry) changed-days)))
                      state))))

  (define (log-file-status path)
    (let ((entries (call-with-input-file (x->namestring path) parse-irc-log-file)))
      `((message-count
         ,(count (lambda (entry)
                   (member (irc-log-entry-type entry) '("<" "*")))
                 entries)))))

  (define (call-with-output-file/atomic pathname proc)
    (receive (tmp-filename tmp-port) (create-temp-file pathname)
      (call-with-port tmp-port proc)
      (rename-file tmp-filename pathname)))

  (define create-temp-file
    (let ((count 1))
      (lambda (path)
        (let ((prefix (file-namestring path)))
          (let loop ((i count))
            (let ((pathname (pathname-with-file
                             path
                             (string-append prefix (number->string i) ".tmp"))))
              (guard (c ((i/o-file-already-exists-error? c)
                         (loop (+ i 1))))
                (let ((port (open-file-output-port (x->namestring pathname)
                                                   (file-options)
                                                   'block
                                                   (native-transcoder))))
                  (set! count (+ i 1))
                  (values pathname port)))))))))

  (define (submatches match names)
    (map (lambda (name)
           (cons name (irregex-match-substring match name)))
         names))

  (define (fold-log-tree log-dir tree-struct proc . seeds)
    (let ((log-tree-rxs (map construct-rx tree-struct))
          (placeholders (map extract-placeholders tree-struct)))
      (receive (loc rxs placeholders . seeds)
               (apply
                directory-fold-tree*
                log-dir
                (lambda (file-entry loc rxs placeholders . seeds)
                  (cond ((and (null? (cdr rxs))
                              (file-readable? file-entry)
                              (irregex-match (car rxs) (file-namestring file-entry)))
                         => (lambda (match)
                              (let ((vals (concatenate
                                           (cons (submatches match
                                                             (car placeholders))
                                                 loc))))
                                (receive new-seeds (apply proc vals file-entry seeds)
                                  (apply values #t loc rxs placeholders new-seeds)))))
                        (else
                         (apply values #t loc rxs placeholders seeds))))
                (lambda (dir-entry loc rxs placeholders . seeds)
                  (cond ((and (not (null? (cdr rxs)))
                              (irregex-match (car rxs) (file-namestring dir-entry)))
                         => (lambda (match)
                              (apply values
                                     #t
                                     #t
                                     (lambda (old-loc old-rxs old-placeholders . new-seeds)
                                       (apply values #t loc rxs placeholders new-seeds))
                                     (cons (submatches match (car placeholders)) loc)
                                     (cdr rxs)
                                     (cdr placeholders)
                                     seeds)))
                        (else
                         (apply values #t #f #f loc rxs placeholders seeds))))
                '()
                log-tree-rxs
                placeholders
                seeds)
        (apply values seeds))))

  (define (log-tree-update-list log-dir tree-struct last-update match?)
    (define (update-entry vals path lst)
      (if (and (or (not match?)
                   (match? vals))
               (or (not last-update) (time>? (file-modification-time path) last-update)))
          (let ((key (list (assq-ref vals 'year)
                           (assq-ref vals 'tag)
                           (assq-ref vals 'channel)))
                (month/day (list (and-let* ((m (assq-ref vals 'month)))
                                   (string->number m))
                                 (and-let* ((d (assq-ref vals 'day)))
                                   (string->number d)))))
            (println "* {0} modified, adding to update list" (x->namestring path))
            (cond ((assoc key lst)
                   => (lambda (entry)
                        (cons (cons key (cons (list month/day path) (cdr entry)))
                              (filter (lambda (elt)
                                        (not (eq? elt entry)))
                                      lst))))
                  (else
                   (cons (cons key (list (list month/day path))) lst))))
          lst))
    (if last-update
        (fold (lambda (year entries)
                (fold-log-tree log-dir
                               (cons (number->string year) tree-struct)
                               (lambda (vals path lst)
                                 (update-entry (cons `(year . ,year) vals) path lst))
                               entries))
              '()
              (year-range (time-utc->date last-update 0) (current-date 0)))
        (fold-log-tree log-dir (cons 'year tree-struct) update-entry '())))

  (define (year-range start-date end-date)
    (let ((start-year (date-year start-date))
          (end-year (date-year end-date)))
      (iota (+ (- end-year start-year) 1) start-year)))

  (define (pathname-has-type? pathname type)
    (let ((file (pathname-file pathname)))
      (and file
           (not (null? (file-types file)))
           (string=? type (last (file-types file))))))

  (define parse-state-pathname
    (let ((rx (sre->irregex `(: (submatch-named year (+ digit)) "-"
                                (submatch-named tag ,ident-sre) "-"
                                (submatch-named channel (: (+ "#" ,ident-sre)))
                                ".state"))))
      (lambda (pathname)
        (cond ((irregex-match rx (file-namestring pathname))
               => (lambda (match)
                    (receive (year tag channel)
                             (apply values (map cdr (submatches match '(year tag channel))))
                      (values (string->number year) tag channel))))
              (else
               (values #f #f #f))))))



  (define (resolve-template template vals)
    (cond ((symbol? template)
           (assq-ref vals template))
          ((pair? template)
           (string-concatenate (map (lambda (part) (resolve-template part vals)) template)))
          (else
           template)))

  (define (vals->pathname base vals template)
    (receive (dir-parts file-part)
             (split-at (map (lambda (part) (resolve-template part vals)) template)
                       (- (length template) 1))
      (make-pathname (pathname-origin base)
                     (append (pathname-directory base) dir-parts)
                     (if (pair? (car file-part))
                         (string-concatenate (car file-part))
                         (car file-part)))))

  (define (log-path log-dir template tag channel date)
    (vals->pathname log-dir
                    `((tag . ,tag)
                      (channel . ,channel)
                      (year . ,(num->str (date-year date) 4))
                      (month . ,(num->str (date-month date) 2))
                      (day . ,(num->str (date-day date) 2)))
                    template))

  (define (parse-date s)
    (guard (c (#t (values #f #f #f)))
      (let ((date (string->date s "~Y-~m-~d")))
        (values (date-year date) (date-month date) (date-day date)))))

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
                        (cons (list (substring/shared str i (irregex-match-start m))
                                    `(a (^ (href ,url)) ,url))
                              markup)))
                    '()
                    str
                    (lambda (i markup)
                      (cons (list (substring/shared str i (string-length str)))
                            markup))))))

  (define (log-entry->shtml count e)
    (define (timed-row class data)
      `(tr (^ (class ,(if (= (mod count 2) 0) "even" "odd")))
           (td (^ (class "time"))
               ,(ssubst "{0}:{1}" (irc-log-entry-hours e) (irc-log-entry-minutes e)))
           (td (^ (class ,class)) ,@data)))
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
      (case kind
        ((message)
         (timed-row (nick-class) (list "<" (irc-log-entry-nick e) "> " message)))
        ((action)
         (timed-row (nick-class) (list "* " (irc-log-entry-nick e) " " message)))
        ((event)
         (timed-row "event" (list (irc-log-entry-type e) " " (irc-log-entry-nick e) " " message)))
        (else
         `(tr (td (^ (class "meta") (colspan 2)) ,message))))))

  (define (filter-fold-log-file/shtml port pred proc . seeds)
    (apply fold-irc-log-file
           port
           (lambda (entry count . seeds)
             (apply values
                    (if (irc-log-entry-nick entry)
                        (+ count 1)
                        count)
                    (if (pred entry)
                        (receive new-seeds (apply proc (log-entry->shtml count entry) seeds)
                          new-seeds)
                        seeds)))
           0 seeds))

  (define (fold-log-file/shtml port proc . seeds)
    (apply filter-fold-log-file/shtml port (lambda (entry) #t) proc seeds))

  (define (log-file->shtml port)
    (receive (count markup)
             (fold-log-file/shtml port (lambda (shtml markup) (cons shtml markup)) '())
      `(table (^ (class "log")) ,@(reverse markup))))

  (define (q->matcher q)
    (lambda (entry)
      (string-contains (irc-log-entry-message entry) q)))

  (define (footer self)
    `(div (^ (id "foot"))
          (p "Powered by the " (a (^ (href ,(self 'homepage-url))) "IRClogs System")
             ", running on " ,(host-impl-info-shtml))
          #|
          (p (a (^ (href "http://validator.w3.org/check?uri=referer"))
                (img (^ (src "http://www.w3.org/Icons/valid-xhtml10-blue")
                        (alt "Valid XHTML 1.0 Strict")
                        (height 23)
                        (width 66)))))
          |#))

  (define (channel-days-tds base-url tag channel days prop-vec . args)
    (let-optionals* args ((start 0)
                          (end (vector-length days))
                          (with-day? #f))
      (let ()
        (define (text day count)
          (cond ((and with-day? count)
                 (ssubst "{0} ({1})" (caddr day) count))
                (with-day?
                  (ssubst "{0}" (caddr day)))
                (count
                 (ssubst "({0})" count))
                (else
                 "")))
        (define (day-td day props)
          `(td ,(cond ((assq-ref props 'message-count)
                       => (lambda (count)
                            (day-link base-url tag channel (apply mk-date day)
                                      (text day (car count)))))
                      (else
                       (text day #f)))))
        (let loop ((markup '()) (i (- end 1)))
          (if (< i start)
              markup
              (loop (cons (day-td (vector-ref days i) (vector-ref prop-vec i)) markup)
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

  (define (search-form base-url tag channel)
    (let ((title (ssubst "Search {0}/{1}" tag channel)))
      `(form (^ (id "search")
                (action ,(url-escape (ssubst "{0}{1}/{2}/" base-url tag channel) "/"))
                (accept-charset "utf-8"))
             (input (^ (name "q") (title ,title) (size 42) (maxlength 2048)))
             (br)
             (input (^ (type "submit") (name "search-btn") (value "Search"))))))

  (define (render-channel-overview/html base-url tag channel days prop-vec)
    `((h1 ,(breadcrumbs base-url tag channel #f))
      ,(search-form base-url tag channel)
      ,@(append-map
         (lambda (month-borders)
           (receive (year month day) (apply values (vector-ref days (car month-borders)))
             (list `(h2 ,(month-string (mk-date year month day)))
                   (channel-monthly-table base-url tag channel 10 days prop-vec
                                          (car month-borders) (cdr month-borders)))))
         (days-month-borders days))))

  (define (channel-monthly-table base-url tag channel n-columns days prop-vec start end)
    `(table
      (^ (class "mactivity"))
      ,@(let loop ((markup '()) (i start))
          (if (>= i end)
              (reverse markup)
              (let* ((n-vals (min n-columns (- end i)))
                     (n-empty (- n-columns n-vals)))
                (loop (append
                       `((tr ,@(channel-days-tds base-url tag channel days prop-vec
                                                 i (+ i n-vals) #t)
                             ,@(make-list n-empty '(td))))
                       markup)
                      (+ i n-columns)))))))

  (define (log-search-task heading port log-port match?)
    (receive (msg-count first?)
             (filter-fold-log-file/shtml
              log-port
              match?
              (lambda (shtml first?)
                (when first?
                  (sxml->xml `(tr (th (^ (colspan 2)) ,heading)) port))
                (sxml->xml shtml port)
                #f)
              #t)
      (yield/c #t)
      msg-count))

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
                     " > " ,(last-link date-link base-url tag channel date)))
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

  (define (date-link base-url tag channel date-str)
    `(a (^ (href ,(url-escape (ssubst "{0}{1}/{2}/{3}/" base-url tag channel date-str) "/")))
          ,date-str))

  (define (day-link base-url tag channel date text)
    `(a (^ (href ,(url-escape (ssubst "{0}{1}/{2}/{3}/"
                                      base-url
                                      tag channel
                                      (isodate-str date))
                              "/")))
        ,text))

  (define url-escape
    (let ((safe-cs (char-set-union char-set:letter
                                   char-set:digit
                                   (string->char-set "$-_.+!*'(),/"))))
      (define (encode code)
        (string-append "%" (number->string code 16)))
      (lambda (s safe-add)
        (let ((safe-cs (char-set-union safe-cs (string->char-set safe-add))))
          (str-escape (lambda (c)
                        (if (char-set-contains? safe-cs c)
                            (string c)
                            (let ((code (char->integer c)))
                              (cond ((< code 256)
                                     (encode code))
                                    (else
                                     (let ((utf8 (bytevector->u8-list (string->utf8 (string c)))))
                                       (string-concatenate (map encode utf8))))))))
                      s)))))

  (define (num->str n width)
    (fmt #f (pad-char #\0 (pad/left 2 (num n)))))

  (define (str-escape escaper str)
    (string-concatenate-reverse
     (string-fold (lambda (c parts)
                    (cons (escaper c) parts))
                  '()
                  str)))

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

  (define *one-day* (make-time time-duration 0 (* 24 60 60)))

  (define (days-between start-day end-day)
    (define (day->date day)
      (make-date 0 0 0 0 (caddr day) (cadr day) (car day) 0))
    (fold-days-between (day->date start-day) (day->date end-day)
                       (lambda (date days)
                         (cons (list (date-year date) (date-month date) (date-day date))
                               days))
                       '()))

  (define (fold-days-between start-day end-day proc . seeds)
    (let* ((start (date->time-utc start-day))
           (end (add-duration (date->time-utc end-day) *one-day*)))
      (receive (step time-cmp?)
               (if (time<? start end)
                   (values *one-day* time>=?)
                   (values (time-* -1 *one-day*) time<=?))
        (let loop ((cur start) (seeds seeds))
          (if (time-cmp? cur end)
              (apply values seeds)
              (let ((cur-date (time-utc->date cur 0)))
                (loop (add-duration cur step)
                      (receive new-seeds (apply proc cur-date seeds) new-seeds))))))))

  (define (sexp->matcher expr)
    (define (submatcher mapper)
      (let ((sub-matchers (map sexp->matcher (cdr expr))))
        (lambda (vals)
          (mapper (lambda (m) (m vals)) sub-matchers))))
    (case (car expr)
      ((and) (submatcher and-map))
      ((or)  (submatcher or-map))
      (else
       (let ((rx (irregex (cadr expr))))
         (lambda (vals)
           (let ((val (assq-ref vals (car expr))))
             (irregex-match rx val)))))))

  (define ident-sre '(+ (or alnum #\- #\_ #\* #\+)))

  (define sre-alist
    `((year . (>= 4 digit))
      (month . (** 1 2 digit))
      (day .   (** 1 2 digit))
      (tag . ,ident-sre)
      (channel . (: (+ "#") ,ident-sre))))

  (define (construct-rx part)
    (define (->sre x)
      (cond ((symbol? x)
             `(submatch-named ,x ,(or (assq-ref sre-alist x)
                                      (error 'construct-rx "unknown shorthand" x))))
            (else x)))
    (if (pair? part)
        (sre->irregex (cons ': (map ->sre part)))
        (sre->irregex (->sre part))))

  (define (extract-placeholders part)
    (cond ((pair? part)
           (filter symbol? part))
          ((symbol? part)
           (list part))
          (else
           '())))

  (define (date+days->time-utc date n-days)
    (let ((step (time-* n-days *one-day*)))
      (add-duration (date->time-utc date) step)))

  (define (query-date query)
    (let ((val (assq-ref query 'date)))
      (and val
           (irregex-match isodate-rx val)
           (date-with-zone-offset (string->date val isodate-fmt) 0))))

  (define isodate-fmt "~Y-~m-~d")
  (define isodate-rx (irregex '(: (>= 4 digit) "-" (** 1 2 digit) "-" (** 1 2 digit))))

  (define (time-* n time)
    (make-time (time-type time) (* n (time-nanosecond time)) (* n (time-second time))))

  (define (mk-date year month day)
    (make-date 0 0 0 0 day month year 0))

  (define (date+days date n-days)
    (time-utc->date (date+days->time-utc date n-days) 0))

  (define (isodate-str date)
    (date->string date isodate-fmt))

  (define (sign x)
    (cond ((< x 0) -1)
          ((= x 0)  0)
          (else     1)))

;;; Code following uses the Prometheus object system

  (define-syntax define-privates
    (syntax-rules ()
      ((define-privates name ...)
       (begin (define name (list 'name)) ...))))

  (define-syntax let-privates
    (syntax-rules ()
      ((let-privates (name ...) body ...)
       (let ((name (list 'name)) ...)
         body ...))))

  (define-privates
    %set-log-dir! %set-state-dir!
    %set-dir-struct!
    %get-state
    %set-base-url! %set-homepage-url!
    %matcher %set-matcher!
    %render-multi-overview/html
    %render-search-task
    %day-range-search-task
    %activity-nav-links
    %log-nav-links)

  (define *irclogs* (*the-root-object* 'clone))

  (define (make-irclogs options)
    (let ((logs (*irclogs* 'clone)))
      (for-each (lambda (entry)
                  (case (car entry)
                    ((log-dir)   (logs %set-log-dir! (pathname-as-directory (cadr entry))))
                    ((state-dir) (logs %set-state-dir! (pathname-as-directory (cadr entry))))
                    ((dir-struct) (logs %set-dir-struct! (cadr entry)))
                    ((base-url)   (logs %set-base-url! (cadr entry)))
                    ((homepage-url) (logs %set-homepage-url! (cadr entry)))
                    ((match)      (logs %set-matcher! (sexp->matcher (cadr entry))))
                    (else
                     (error 'make-irclogs "unknown option" entry))))
                options)
      logs))

  (define-method (*irclogs* 'render-overview/html self resend just-meta? tag channel query)
    (let ((base-url (self 'base-url))
          (base-date (or (query-date query) (current-date 0)))
          (n-days (cond ((and tag channel) 365)
                        (else                7))))
      (cond
       ((and tag channel (assq-ref query 'q))
        => (lambda (q) (self %render-search-task tag channel base-date 14 q)))
       (else
        (let ((state (self %get-state tag channel base-date n-days)))
          (receive (days rows)
                   (state-tabularize (state-sort `((1 ,string<? ,string=?)
                                                   (2 ,string<? ,string=?)
                                                   (0 ,string<? ,string=?))
                                                 state))
            (let ((n-rows (length rows)))
              (cond ((= n-rows 0)
                     #f)
                    ((and tag channel)
                     (assert (= n-rows 1))
                     (render-channel-overview/html base-url tag channel days (cadar rows)))
                    (else
                     (self %render-multi-overview/html
                           tag channel (or (query-date query) (current-date 0)) days rows))))))))))

  (define-method (*irclogs* %render-multi-overview/html self resend tag channel base-date days rows)
    (let ((base-url (self 'base-url))
          (n-days (min 7 (vector-length days))))
      (and (> n-days 0)
           `((h1 ,(breadcrumbs base-url tag channel #f))
             ,(self %activity-nav-links tag channel base-date n-days 7)
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
                               ,@(channel-days-tds base-url tag channel days
                                                   (cadr row) 0 n-days))))
                      rows)))
             ,(footer self)))))

  (define-method (*irclogs* 'render-log/html self resend just-meta? tag channel date)
    (receive (year month day) (parse-date date)
      (and year month day
           (and-let* ((port (self 'open-log-file tag channel (mk-date year month day))))
             (call-with-port port
               (lambda (port)
                 `((h1 ,(breadcrumbs (self 'base-url) tag channel date))
                   ,(self %log-nav-links tag channel (mk-date year month day))
                   ,(log-file->shtml port)
                   ,(footer self))))))))

  (define-method (*irclogs* 'update-state self resend)
    (let ((state-dir (self 'state-dir))
          (log-dir   (self 'log-dir))
          (tree-struct (self 'dir-struct)))
      (create-directory* state-dir)
      (let* ((date-fmt "~Y-~m-~d ~H:~M:~S ~z")
             (update-file (pathname-with-file state-dir "last-update"))
             (last-update (and-let* ((date-str (and (file-exists? update-file)
                                                    (call-with-input-file (x->namestring update-file)
                                                      read))))
                            (date->time-utc (string->date date-str date-fmt)))))
        (for-each (lambda (entry)
                    (let ((state-file (pathname-with-file
                                       state-dir
                                       (string-substitute "{0}-{1}-{2}.state" (car entry))))
                          (tag (caar entry))
                          (channel (cadar entry))
                          (year (caddar entry)))
                      (println "{0} {1}/{2}: {3} days updated" year tag channel (length (cdr entry)))
                      (call-with-output-file/atomic state-file
                        (lambda (port)
                          (write (merge-state
                                  (if (file-exists? state-file)
                                      (call-with-input-file (x->namestring state-file) read)
                                      '())
                                  (cdr entry))
                                 port)))))
                  (log-tree-update-list log-dir tree-struct last-update (self %matcher)))
        (call-with-output-file/atomic update-file
          (lambda (port)
            (write (date->string (current-date 0) date-fmt) port))))))

  (define-method (*irclogs* %get-state self resend tag channel base-date n-days)
    (let* ((start-time (date+days->time-utc base-date (- n-days)))
           (start-date (time-utc->date start-time 0))
           (end-time (date->time-utc base-date))
           (end-date base-date)
           (state-dir (self 'state-dir)))
      (define (date-match? year)
        (or (eqv? base-date #f) (<= (date-year base-date) year (date-year end-date))))
      (define (filter-days year entries)
        (if (not base-date)
            entries
            (filter (lambda (entry)
                      (receive (month day) (apply values (car entry))
                        (let ((entry-time (date->time-utc (mk-date year month day))))
                          (and (time<=? start-time entry-time)
                               (time<=? entry-time end-time)))))
                    entries)))
      (if (file-exists? state-dir)
          (directory-fold
           state-dir
           (lambda (entry state)
             (if (and (pathname-has-type? entry "state")
                      (file-readable? entry))
                 (receive (st-year st-tag st-channel) (parse-state-pathname entry)
                   (if (and st-year st-tag st-channel
                            (date-match? st-year)
                            (or (eqv? tag #f)  (string=? tag st-tag))
                            (or (eqv? channel #f) (string=? channel st-channel))
                            ((self %matcher) `((year . ,st-year)
                                               (tag . ,st-tag)
                                               (channel . ,st-channel))))
                       (let ((entries
                              (filter-days st-year
                                           (call-with-input-file (x->namestring entry) read))))
                         (cons (cons* st-year st-tag st-channel entries) state))
                       state))
                 state))
           '())
          (begin
            (create-directory* state-dir)
            '()))))


  (define-method (*irclogs* 'open-log-file self resend tag channel date)
    (and
     ;; call %get-state to ensure we are indeed serving that channel
     (not (null? (self %get-state tag channel date 1)))
     (let ((path (log-path (self 'log-dir) (cons 'year (self 'dir-struct)) tag channel date)))
       (and (file-exists? path)
            (file-readable? path)
            (transcoded-port (open-file-input-port (x->namestring path)) (native-transcoder))))))

  (define-method (*irclogs* %render-search-task self resend tag channel base-date n-days q)
    `((h1 ,(breadcrumbs (self 'base-url) tag channel #f #t))
      (table
       (^ (class "log"))
       (task ,(lambda (port)
                (let ((timer (start-timer)))
                  (receive (day-count msg-count)
                           (self %day-range-search-task
                                 port tag channel base-date (date+days base-date (- n-days))
                                 (q->matcher q))
                    `(div (^ (id "timing"))
                          ,(ssubst "Searched {0} messages on {1} days in {2} seconds"
                                   msg-count
                                   day-count
                                   (fmt #f (num (inexact (timer)) 10 4)))))))))
      (task-result)
      ,(footer self)))

  (define-method (*irclogs* %day-range-search-task self resend
                            port tag channel start-date end-date match?)
    (let ((base-url (self 'base-url)))
      (fold-days-between
       start-date
       end-date
       (lambda (date day-count msg-count)
         (values
          (+ day-count 1)
          (+ msg-count
             (or
              (and-let* ((log-port (self 'open-log-file tag channel date)))
                (log-search-task (day-link base-url tag channel date (isodate-str date))
                                 port
                                 log-port
                                 match?))
              0))))
       0 0)))

  (define-method (*irclogs* %activity-nav-links self resend tag channel base-date n-days-shown step)
    (let ((next-date (date+days base-date step))
          (prev-date (date+days base-date (- n-days-shown)))
          (loc (string-concatenate (append (if tag (list tag "/") '())
                                           (if channel (list channel "/"))))))
      (define (date-link date text)
        (let ((url (url-escape (ssubst "{0}{1}?date={2}" (self 'base-url) loc (isodate-str date))
                               "/?=")))
          `(a (^ (href ,url)) ,text)))
      `(div (^ (id "nav"))
            ,(date-link next-date "<<")
            ,(date-link prev-date ">>"))))

  (define-method (*irclogs* %log-nav-links self resend tag channel date)
    (let ((next-date (date+days date 1))
          (prev-date (date+days date -1))
          (loc (string-append tag "/" channel "/")))
      (define (date-link date text)
        (let ((url (url-escape (string-append (self 'base-url) loc (isodate-str date) "/") "/")))
          `(a (^ (href ,url)) ,text)))
      `(div (^ (id "nav"))
            ,(date-link next-date "<<")
            ,(date-link prev-date ">>"))))

  (*irclogs* 'add-value-slot! 'log-dir %set-log-dir! (make-pathname #f '() #f))
  (*irclogs* 'add-value-slot! 'state-dir %set-state-dir! (make-pathname #f '() #f))
  (*irclogs* 'add-value-slot! 'dir-struct %set-dir-struct!
             '(tag (channel "." month "-" day ".log")))
  (*irclogs* 'add-value-slot! 'base-url %set-base-url! "/")
  (*irclogs* 'add-value-slot! 'homepage-url %set-homepage-url! "/static/irclogs.html")
  (*irclogs* 'add-value-slot! %matcher %set-matcher! #f)

  )
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
          (prometheus)
          (irclogs parse))

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

  (define (log-tree-update-list log-dir tree-struct last-update)
    (define (update-entry vals path lst)
      (if (or (not last-update) (time>? (file-modification-time path) last-update))
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

  (define (log-path log-dir template tag channel year month day)
    (vals->pathname log-dir
                    `((tag . ,tag)
                      (channel . ,channel)
                      (year . ,(number->string year))
                      (month . ,(number->string month))
                      (day . ,(number->string day)))
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
    (cond ((irc-log-entry-nick e)
           `(tr (^ (class ,(if (= (mod count 2) 0) "even" "odd")))
                (td (^ (class "time"))
                    ,(ssubst "{0}:{1}" (irc-log-entry-hours e) (irc-log-entry-minutes e)))
                (td (^ (class ,(ssubst "n{0}" (mod (word->integer (irc-log-entry-nick e)) 26))))
                    ,(if (equal? (irc-log-entry-type e) "<") "<" "* ")
                    ,(irc-log-entry-nick e)
                    ,(if (equal? (irc-log-entry-type e) "<") ">" "")
                    ,(linkify (irc-log-entry-message e)))))
          (else
           `(tr (td (^ (class "meta") (colspan 2)))
                ,(linkify (irc-log-entry-message e))))))

  (define (log-file->shtml port)
    (receive (count markup)
             (fold-irc-log-file
              port
              (lambda (entry count markup)
                (values (if (irc-log-entry-nick entry)
                            (+ count 1)
                            count)
                        (cons (log-entry->shtml count entry) markup)))
              0 '())
      `(table ,@(reverse markup))))

  (define (channel-days-tds base-url year tag channel days prop-vec . args)
    (let-optionals* args ((start 0)
                          (end (vector-length days))
                          (with-day? #f))
      (let ()
        (define (text day count)
          (cond ((and with-day? count)
                 (ssubst "{0} ({1})" (cadr day) count))
                (with-day?
                  (ssubst "{0}" (cadr day)))
                (count
                 (ssubst "({0})" count))
                (else
                 "")))
        (define (day-td day props)
          `(td ,(cond ((assq-ref props 'message-count)
                       => (lambda (count)
                            (day-link base-url year tag channel day (text day (car count)))))
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
               (loop borders (car (vector-ref days i)) end-i (- i 1)))
              ((= cur-month (car (vector-ref days i)))
               (loop borders cur-month end-i (- i 1)))
              (else
               (loop (cons (cur-borders) borders) (car (vector-ref days i)) i (- i 1)))))))

  (define (render-channel-overview/html base-url year tag channel days prop-vec)
    `((h1 ,(breadcrumbs base-url tag channel #f))
      ,@(append-map
         (lambda (month-borders)
           (let ((month (car (vector-ref days (car month-borders)))))
             (list `(h2 ,(month-string year month))
                   (channel-monthly-table base-url year tag channel 10 days prop-vec
                                          (car month-borders) (cdr month-borders)))))
         (days-month-borders days))))


  (define (channel-monthly-table base-url year tag channel n-columns days prop-vec start end)
    `(table
      (^ (class "mactivity"))
      ,@(let loop ((markup '()) (i start))
          (if (>= i end)
              (reverse markup)
              (let* ((n-vals (min n-columns (- end i)))
                     (n-empty (- n-columns n-vals)))
                (loop (append
                       `((tr ,@(channel-days-tds base-url year tag channel days prop-vec
                                                 i (+ i n-vals) #t)
                             ,@(make-list n-empty '(td))))
                       markup)
                      (+ i n-columns)))))))

  (define (breadcrumbs base-url tag channel date)
    (cond ((and tag channel date)
           `(span ,(base-link base-url)
                  " > " ,(tag-link base-url tag)
                  " > " ,(channel-link base-url tag channel)
                  " > " ,date))
          ((and tag channel)
           `(span ,(base-link base-url) " > " ,(tag-link base-url tag) " > " ,channel))
          (tag
           `(span ,(base-link base-url) " > " ,tag))
          (else
           "IRC activity for all networks")))

  (define (month-string year month)
    (date->string (make-date 0 0 0 0 1 month year 0) "~B ~Y"))

  (define (base-link base-url)
    `(a (^ (href ,base-url)) "IRC"))

  (define (tag-link base-url tag)
    `(a (^ (href ,(url-escape (string-append base-url tag "/")))) ,tag))

  (define (channel-link base-url tag channel)
    `(a (^ (href ,(url-escape (string-append base-url tag "/" channel "/")))) ,channel))

  (define (day-link base-url year tag channel day text)
    `(a (^ (href ,(url-escape (ssubst "{0}{2}/{3}/{1}-{4}-{5}/"
                                      base-url year tag channel (car day) (cadr day)))))
        ,text))

  (define url-escape
    (let ((safe-cs (char-set-union char-set:letter
                                   char-set:digit
                                   (string->char-set "$-_.+!*'(),/"))))
      (define (encode code)
        (string-append "%" (number->string code 16)))
      (lambda (s)
        (str-escape (lambda (c)
                      (if (char-set-contains? safe-cs c)
                          (string c)
                          (let ((code (char->integer c)))
                            (cond ((< code 256)
                                   (encode code))
                                  (else
                                   (let ((utf8 (bytevector->u8-list (string->utf8 (string c)))))
                                     (string-concatenate (map encode utf8))))))))
                    s))))


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

  (define (state-tabularize year state)
    (receive (min-date max-date)
             (let loop ((min-date #f) (max-date #f) (dates (state-dates state 3)))
               (if (null? dates)
                   (values min-date max-date)
                   (loop (list-select < = min-date (car dates))
                         (list-select > = max-date (car dates))
                         (cdr dates))))
      (let ((days (if (and min-date max-date)
                      (list->vector (days-between year min-date max-date))
                      '#())))
        (values days
                (map (lambda (entry)
                       (receive (head tail) (split-at entry 3)
                         (append head (list (alist->day-vector tail days)))))
                     state)))))

  (define (alist->day-vector alist days)
    (let ((vec (make-vector (vector-length days) '())))
      (do ((alist alist (cdr alist)))
          ((null? alist) vec)
        (let ((index (vector-binary-search days (caar alist) (list-comparator > =))))
          (unless index
            (error 'alist->day-vector "out-of-range date encountered" (car alist) days))
          (vector-set! vec index (cdar alist))))))

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

  (define (state-dates state split)
    (append-map (lambda (entry)
                  (map car (drop entry split)))
                state))

  (define *one-day* (make-time time-duration 0 (* 24 60 60)))

  (define (days-between year start-day end-day)
    (let ((start (date->time-utc (make-date 0 0 0 0 (cadr start-day) (car start-day) year 0)))
          (end (add-duration
                (date->time-utc (make-date 0 0 0 0 (cadr end-day) (car end-day) year 0))
                *one-day*)))
      (let loop ((cur start) (days '()))
        (if (time>=? cur end)
            days
            (let ((cur-date (time-utc->date cur 0)))
              (loop (add-duration cur *one-day*)
                    (cons (list (date-month cur-date) (date-day cur-date))
                          days)))))))

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

  (define (ssubst fmt . args)
    (string-substitute #f fmt args 'braces))

  (define (println fmt . args)
    (string-substitute #t fmt args 'braces)
    (newline))

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

  (define-privates %set-log-dir! %set-state-dir! %set-dir-struct! %get-state %set-base-url!)

  (define *irclogs* (*the-root-object* 'clone))

  (define (make-irclogs options)
    (let ((logs (*irclogs* 'clone)))
      (for-each (lambda (entry)
                  (case (car entry)
                    ((log-dir)   (logs %set-log-dir! (pathname-as-directory (cadr entry))))
                    ((state-dir) (logs %set-state-dir! (pathname-as-directory (cadr entry))))
                    ((dir-struct) (logs %set-dir-struct! (cadr entry)))
                    ((base-url)   (logs %set-base-url! (cadr entry)))
                    (else
                     (error 'make-irclogs "unknown option" entry))))
                options)
      logs))

  (define-method (*irclogs* 'render-overview/html self resend year tag channel)
    (let ((base-url (self 'base-url))
          (state (self %get-state year tag channel)))
      (receive (days rows)
               (state-tabularize year
                                 (state-sort `((1 ,string<? ,string=?)
                                               (2 ,string<? ,string=?))
                                             state))
        (let ((n-rows (length rows)))
          (cond ((= n-rows 0)
                 #f)
                ((and year tag channel)
                 (assert (= n-rows 1))
                 (let ((row (car rows)))
                   (render-channel-overview/html base-url year tag channel days (cadddr row))))
                (else
                 (let ((n-days (min 7 (vector-length days))))
                   `((h1 ,(breadcrumbs base-url tag channel #f))
                     (table
                      (^ (class "activity"))
                      (thead
                       (tr (th "Network") (th "Channel")
                           ,@(map (lambda (day)
                                    `(th ,(ssubst "{0}-{1}" (car day) (cadr day))))
                                  (vector->list days 0 n-days))))
                      (tbody
                       ,@(map (lambda (row)
                                (let ((year (car row))
                                      (tag (cadr row))
                                      (channel (caddr row)))
                                  `(tr (th ,(tag-link base-url tag))
                                       (th ,(channel-link base-url tag channel))
                                       ,@(channel-days-tds base-url year tag channel days
                                                           (cadddr row) 0 n-days))))
                              rows)))))))))))

  (define-method (*irclogs* 'render-log/html self resend tag channel date)
    (receive (year month day) (parse-date date)
      (and year month day
           (let ((path (log-path (self 'log-dir)
                                 (cons 'year (self 'dir-struct))
                                 tag channel year month day)))
             (and (file-exists? path)
                  (file-readable? path)
                  (call-with-input-file (x->namestring path)
                    (lambda (port)
                      `((h1 ,(breadcrumbs (self 'base-url) tag channel date))
                        ,(log-file->shtml port)))))))))

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
                  (log-tree-update-list log-dir tree-struct last-update))
        (call-with-output-file/atomic update-file
          (lambda (port)
            (write (date->string (current-date 0) date-fmt) port))))))

  (define-method (*irclogs* %get-state self resend year tag channel)
    (let ((state-dir (self 'state-dir)))
      (if (file-exists? state-dir)
          (directory-fold state-dir
                          (lambda (entry state)
                            (if (and (pathname-has-type? entry "state")
                                     (file-readable? entry))
                                (receive (st-year st-tag st-channel) (parse-state-pathname entry)
                                  (if (and st-year st-tag st-channel
                                           (or (eqv? year #f) (= year st-year))
                                           (or (eqv? tag #f)  (string=? tag st-tag))
                                           (or (eqv? channel #f) (string=? channel st-channel)))
                                      (let ((entries
                                             (call-with-input-file (x->namestring entry) read)))
                                        (cons (cons* st-year st-tag st-channel entries) state))
                                      state))
                                state))
                          '())
          (begin
            (create-directory* state-dir)
            '()))))

  (*irclogs* 'add-value-slot! 'log-dir %set-log-dir! (make-pathname #f '() #f))
  (*irclogs* 'add-value-slot! 'state-dir %set-state-dir! (make-pathname #f '() #f))
  (*irclogs* 'add-value-slot! 'dir-struct %set-dir-struct!
             '(tag (channel "." month "-" day ".log")))
  (*irclogs* 'add-value-slot! 'base-url %set-base-url! "/")

  )
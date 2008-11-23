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

  (define make-irclogs
    (let ((irclogs (*the-root-object* 'clone)))
      (let-privates (%set-log-dir! %set-state-dir! %set-dir-struct! %get-state)
        (define-method (irclogs 'render-overview/html self resend)
          (render-overview/html (map cdr (self %get-state (current-year) #f #f))))
        (define-method (irclogs 'update-state self resend)
          (update-state (self 'state-dir) (self 'log-dir) (self 'dir-struct)))
        (define-method (irclogs %get-state self resend year tag channel)
          (get-state (self 'state-dir) year tag channel))
        (irclogs 'add-value-slot! 'log-dir %set-log-dir! (make-pathname #f '() #f))
        (irclogs 'add-value-slot! 'state-dir %set-state-dir! (make-pathname #f '() #f))
        (irclogs 'add-value-slot! 'dir-struct %set-dir-struct!
                 '(tag (channel "." month "-" day ".log")))
        (lambda (options)
          (let ((logs (irclogs 'clone)))
            (for-each (lambda (entry)
                        (case (car entry)
                          ((log-dir)   (logs %set-log-dir! (pathname-as-directory (cadr entry))))
                          ((state-dir) (logs %set-state-dir! (pathname-as-directory (cadr entry))))
                          ((dir-struct) (logs %set-dir-struct! (cadr entry)))
                          (else
                           (error 'make-irclogs "unknown option" entry))))
                      options)
            logs)))))

  (define-syntax define-privates
    (syntax-rules ()
      ((define-privates name ...)
       (begin (define name (list 'name)) ...))))

  (define-syntax let-privates
    (syntax-rules ()
      ((let-privates (name ...) body ...)
       (let ((name (list 'name)) ...)
         body ...))))

  (define (update-state state-dir log-dir tree-struct)
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
          (write (date->string (current-date 0) date-fmt) port)))))

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

  (define (current-year)
    (date-year (current-date 0)))

  (define (pathname-has-type? pathname type)
    (let ((file (pathname-file pathname)))
      (and file
           (not (null? (file-types file)))
           (string=? type (last (file-types file))))))

  (define (get-state state-dir year tag channel)
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
          '())))

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

  (define (render-overview/html state)
    (receive (days rows)
             (state-tabularize (current-year) (state-sort `((0 . ,string<?) (1 . ,string<?)) state) 2)
      `((table
         (^ (class "channels"))
         (tr (th "Network") (th "Channel")
             ,@(map (lambda (day)
                      `(th ,(ssubst "{0}-{1}" (car day) (cadr day))))
                    (vector->list days)))
         ,@(map (lambda (row)
                  (let ((tag (car row))
                        (channel (cadr row)))
                    `(tr (td ,(tag-link tag))
                         (td ,(channel-link tag channel))
                         ,@(channel-days-tds tag channel days (caddr row)))))
                rows)))))


  (define (channel-days-tds tag channel days prop-vec)
    (vector-fold-right
     (lambda (i markup day props)
       (cons
        `(td ,(cond ((assq-ref props 'message-count)
                     => (lambda (count)
                          (day-link tag channel day (number->string (car count)))))
                    (else "")))
        markup))
     '()
     days
     prop-vec))

  (define (tag-link tag)
    `(a (^ (href ,(url-escape (string-append tag "/")))) ,tag))

  (define (channel-link tag channel)
    `(a (^ (href ,(url-escape (string-append tag "/" channel)))) ,channel))

  (define (day-link tag channel day text)
    `(a (^ (href ,(url-escape (ssubst "{0}/{1}/{2}-{3}" tag channel (car day) (cadr day)))))
        ,text))
  
  (define url-escape
    (let ((safe-cs (char-set-union char-set:letter
                                   char-set:digit
                                   (string->char-set "$-_.+!*'(),"))))
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
                 (and-map (lambda (key)
                            ((cdr key) (list-ref x (car key)) (list-ref y (car key))))
                          keys))
               state))

  (define (state-tabularize year state split)
    (receive (min-date max-date)
             (let loop ((min-date #f) (max-date #f) (dates (state-dates state split)))
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
                       (receive (head tail) (split-at entry split)
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

  )
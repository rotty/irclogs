#!/usr/bin/mzscheme -qr

;;  irc2html.scm - Convert IRC chat logs into valid HTML with valid CSS
;;  Copyright 2008 Andreas Rottmann <a.rottmann@gmx.at>
;;  Copyright 2003 MJ Ray <mjr@dsl.pipex.com>
;;  Please see the README file for this program for more information.
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#!r6rs
(import (except (rnrs) delete-file file-exists?)
        (rnrs r5rs)
        (srfi :2 and-let*)
        (srfi :8 receive)
        (only (srfi :1 lists) split-at)
        (only (srfi :13 strings)
              string-concatenate
              string-concatenate-reverse
              string-fold)
        (spells alist)
        (spells string-substitute)
        (spells format)
        (spells time-lib)
        (spells filesys)
        (spells pathname)
        (spells misc)
        (spells tracing)
        (only (spells assert) cout)
        (fmt)
        (xitomatl irregex)
        (irclogs utils))

(define colors '("e" "c" "a"))
(define color-override
  (if (file-exists? ".irc2html-colors")
    (call-with-input-file ".irc2html-colors" read)
    '()))

(define (integer->bits num base) ; int int -> (int ...)
  (if (= num 0)
    '()
    (cons (modulo num base) (integer->bits (quotient num base) base))))

(define (integer->color num) ; int -> str
  (let ((nb (append (integer->bits num 3) '(0 0 0))))
    (string-append
      (list-ref colors (list-ref nb 0))
      (list-ref colors (list-ref nb 1))
      (list-ref colors (list-ref nb 2)))))

(define (msum numbers) ; (int ...) -> int
  (if (null? numbers)
    0
    (+ (* (car numbers) (length numbers)) (msum (cdr numbers)))))

;(define (word->color name) ; str -> str
;  (let ((override (assoc name color-override)))
;    (if override
;      (cadr override)
;      (integer->color

(define (word->integer name) ; str -> int
  (let ((override (assoc name color-override)))
    (if override
      (cadr override)
      (msum (map char->integer (string->list name))))))

(define (match-extract convert name)
  (lambda (match)
    (convert (irregex-match-substring match name))))

(define link-re (irregex "\\b[a-z]+://[^\\s<>]+"))

(define (link-string str) ; str -> str
  (irregex-replace/all link-re str
                       "<a href=\"" (match-extract html-escape 0) "\">"
                       (match-extract html-escape 0)
                       "</a>"))

(define (html-escape str) ; str -> str
  (str-escape (lambda (c)
                (case c
                  ((#\<) "&lt;")
                  ((#\>) "&gt;")
                  ((#\&) "&amp;")
                  ((#\") "&quot;")
                  (else (string c))))
              str))

(define (str-escape escaper str)
  (string-concatenate-reverse
   (string-fold (lambda (c parts)
                  (cons (escaper c) parts))
                '()
                str)))
(define ident-sre '(+ (or alnum #\- #\_ #\* #\+)))

(define log-templates
  (map
   (lambda (entry)
     (cons (sre->irregex (car entry))
           (cdr entry)))
   `(((: (submatch-named hours (** 1 2 digit)) ":" (submatch-named minutes (** 1 2 digit))
         " " (submatch-named type (+ (~ white))) " "
         (submatch-named nick ,ident-sre) (? (or ":" ">"))
         (submatch-named line (* any)))
      ,log-line->html (hours minutes #f type nick line))
     ((: (submatch-named hours (** 1 2 digit)) ":" (submatch-named minutes (** 1 2 digit))
         ":" (submatch-named seconds (** 1 2 digit))
         " " (submatch-named type (or "<" "*"))
         (submatch-named nick ,ident-sre) ">"
         (submatch-named line (* any)))
      ,log-line->html (hours minutes seconds type nick line)))))

(define (log-line->html hours minutes seconds type nick line)
  (string-append
   "<li class=\"n" (number->string (modulo (word->integer nick) 26)) "\"> "
   "<span class=\"time\">" hours ":" minutes "</span> "
   (if (equal? type "<") "&lt;<b>" "* <b>")
   nick
   (if (equal? type "<") "</b>&gt;" "</b>")
   (html-escape line)
   "</li>\n"))

(define (color-line str) ; str -> str
  (define (submatches match names)
    (map (lambda (name)
           (if (or (symbol? name) (integer? name))
               (irregex-match-substring match name)
               name))
         names))
  (or (or-map (lambda (irx/tmpl)
                (and-let* ((match (irregex-match (car irx/tmpl) str)))
                  (apply (cadr irx/tmpl) (submatches match (caddr irx/tmpl)))))
              log-templates)
      (string-append "<li class=\"meta\"><em>" (html-escape str) "</em></li>\n")))


;;   (let ((lline (string->list str)))
;;     (if (and (not (null? lline)) (member (car lline) '(#\< #\*)))
;;         (let* ((type (car lline))
;;                (nick (if (equal? type #\<)
;;                          (substring str 1 (- (length lline) (length (member #\> lline))))
;;                          (substring str 2 (- (length lline) (length (member #\space (cddr lline)))))))
;;                (line (list->string (member #\space (cddr lline)))))
;;           )
;;         ))

(define (process-log iport title oport eport) ; port str port port -> void
  (let ((total-timer (start-timer)))
    (display "<?xml version=\"1.0\"?>
      <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
      <html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
      <head>
      <meta name=\"GENERATOR\" content=\"irc2html.sps by MJ Ray, hacked by Andreas Rottmann\"/>
      <title>" oport)
    (display title oport)
    (display "
      </title>
      <style type=\"text/css\">
      <!--
      body { background: #fff; color: #000; }
      a:link,a:visited,a:active { background: #0ff; color: #006; }
      #foot { font-size: x-small; }
      " oport)
      (for-each
        (lambda (x)
	  (format oport ".n~a { background: #~a }\n" x (integer->color x)))
	'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
      (display "
      .meta { color: #999 }
      // -->
      </style>
      </head>
      <body>
      <ol>\n" oport)
    (let loop ()
      (let ((line (get-line iport)))
        (cond ((eof-object? line)
               (let ((time-str (fmt #f (num (real->flonum (total-timer)) 10 5))))
                 (string-substitute
                  oport
                  (string-append
                   "</ol><p id=\"foot\">Automatic markup by irc2html.sps"
                   " in {0} second(s) runtime</p></body></html>\n")
                  (vector time-str)
                  'braces)
                 (string-substitute eport "{0}: {1} second(s)\n" (vector title time-str) 'braces)))
              (else
               (display (link-string (color-line line)) oport)
               (loop)))))))


(define (process-log-tree log-tree-struct log-dir html-tree-struct html-dir title-struct)
  (define (submatches match names)
    (map (lambda (name)
           (cons name (irregex-match-substring match name)))
         names))
  (let ((log-tree-rxs (map construct-rx log-tree-struct))
        (placeholders (map extract-placeholders log-tree-struct))
        (log-dir (pathname-as-directory log-dir))
        (html-dir (pathname-as-directory html-dir)))
    (directory-fold-tree* log-dir
                          (lambda (file-entry loc rxs placeholders)
                            (cond ((and (null? (cdr rxs))
                                        (file-readable? file-entry)
                                        (irregex-match (car rxs) (file-namestring file-entry)))
                                   => (lambda (match)
                                        (let* ((loc (cons (submatches match
                                                                      (car placeholders))
                                                          loc))
                                               (html-filename
                                                (loc->pathname html-dir
                                                               loc
                                                               html-tree-struct)))
                                          (when (file-stale? html-filename file-entry)
                                            (when (file-exists? html-filename)
                                              (delete-file html-filename))
                                            (call-with-file-ports
                                                file-entry
                                                html-filename
                                              (lambda (in-port out-port)
                                                (process-log in-port
                                                             (resolve-template title-struct loc)
                                                             out-port
                                                             (current-error-port)))))))))
                            (values #t loc rxs placeholders))
                          (lambda (dir-entry loc rxs placeholders)
                            (cond ((and (not (null? (cdr rxs)))
                                        (irregex-match (car rxs) (file-namestring dir-entry)))
                                   => (lambda (match)
                                        (values
                                         #t
                                         #t
                                         (lambda ignored (values #t loc rxs placeholders))
                                         (cons (submatches match (car placeholders)) loc)
                                         (cdr rxs)
                                         (cdr placeholders))))
                                  (else
                                   (values #t #f #f loc rxs placeholders))))
                          '()
                          log-tree-rxs
                          placeholders)))

(define sre-alist
  `((year . (>= 4 digit))
    (month . (** 1 2 digit))
    (day .   (** 1 2 digit))
    (tag . ,ident-sre)
    (channel . ,ident-sre)))

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

(define (resolve-template template loc)
  (cond ((symbol? template)
         (or-map (lambda (l)
                   (assq-ref l template))
                 loc))
        ((pair? template)
         (string-concatenate (map (lambda (part) (resolve-template part loc)) template)))
        (else
         template)))

;; @2 is an alist according to which template is substituted
(define (loc->pathname base loc template)
  (receive (dir-parts file-part)
           (split-at (map (lambda (part) (resolve-template part loc)) template)
                     (- (length template) 1))
    (make-pathname (pathname-origin base)
                   (append (pathname-directory base) dir-parts)
                   (if (pair? (car file-part))
                       (string-concatenate (car file-part))
                       (car file-part)))))

(define (call-with-file-ports in-filename out-filename proc)
  (let ((out-dir (pathname-with-file out-filename #f)))
    (call-with-input-file (x->namestring in-filename)
      (lambda (in-port)
        (unless (file-exists? out-dir)
          (create-directory* out-dir))
        (call-with-output-file (x->namestring out-filename)
          (lambda (out-port)
            (proc in-port out-port)))))))

(define (file-stale? file source)
  (or (not (file-exists? file))
      (time>=? (file-modification-time source) (file-modification-time file))))

(define log-tree-structure '(year tag ("#" channel "." month "-" day ".log")))
(define html-tree-structure '(year tag (channel "-" month "-" day ".xhtml")))
(define title-struct '("#" channel " on " tag " - " year "-" month "-" day))

(let ((argv (command-line)))
  (process-log-tree log-tree-structure (cadr argv)
                    html-tree-structure (caddr argv)
                    title-struct))

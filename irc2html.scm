#!/usr/bin/mzscheme -qr

;;  irc2html.scm - Convert IRC chat logs into valid HTML with valid CSS
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

(require (lib "pregexp.ss"))

(define colours '("e" "c" "a"))
(define colour-override
  (if (file-exists? ".irc2html-colours")
    (call-with-input-file ".irc2html-colours" read)
    '()))

(define (integer->bits num base) ; int int -> (int ...)
  (if (= num 0)
    '()
    (cons (modulo num base) (integer->bits (quotient num base) base))))

(define (integer->colour num) ; int -> str
  (let ((nb (append (integer->bits num 3) '(0 0 0))))
    (string-append
      (list-ref colours (list-ref nb 0))
      (list-ref colours (list-ref nb 1))
      (list-ref colours (list-ref nb 2)))))

(define (msum numbers) ; (int ...) -> int
  (if (null? numbers)
    0
    (+ (* (car numbers) (length numbers)) (msum (cdr numbers)))))

;(define (word->colour name) ; str -> str
;  (let ((override (assoc name colour-override)))
;    (if override
;      (cadr override)
;      (integer->colour

(define (word->integer name) ; str -> int
  (let ((override (assoc name colour-override)))
    (if override
      (cadr override)
      (msum (map char->integer (string->list name))))))

(define link-re (pregexp "\\b[a-z]+://[^\\s<>]+"))

(define (link-string str) ; str -> str
  (pregexp-replace* link-re str "<a href=\"\\0\">\\0</a>"))

(define (add-to-list char l) ; char (char ...) -> (char ...)
  (cond ((equal? char #\<) (cons #\; (cons #\t (cons #\l (cons #\& l)))))
        ((equal? char #\>) (cons #\; (cons #\t (cons #\g (cons #\& l)))))
        ((equal? char #\&) (cons #\; (cons #\p (cons #\m (cons #\a (cons #\& l))))))
        (#t (cons char l))))

(define (html-escape str) ; str -> str
  (let iter ((out '()) (in (string->list str)))
    (if (null? in)
      (list->string (reverse out))
      (iter (add-to-list (car in) out) (cdr in)))))

(define (colour-line str) ; str -> str
  (let ((lline (string->list str)))
    (if (and (not (null? lline)) (member (car lline) '(#\< #\*)))
        (let* ((type (car lline))
               (nick (if (equal? type #\<)
                         (substring str 1 (- (length lline) (length (member #\> lline))))
                         (substring str 2 (- (length lline) (length (member #\space (cddr lline)))))))
               (line (list->string (member #\space (cddr lline)))))
          (string-append
            "<li class=\"n"
            (number->string (modulo (word->integer nick) 26))
            "\"> "
            (if (equal? type #\<) "&lt;<b>" "* <b>")
            nick
            (if (equal? type #\<) "</b>&gt;" "</b>")
            (html-escape line)
            "</li>\n"))
        (string-append "<li class=\"meta\"><em>" (html-escape str) "</em></li>"))))

(define (process-log iport title oport eport) ; port str port port -> void
  (let ((start (current-seconds)))
    (display "<?xml version=\"1.0\"?>
      <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
      <html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
      <head>
      <meta name=\"GENERATOR\" content=\"irc2html.scm by MJ Ray\"/>
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
	  (display
	    (format ".n~a { background: #~a }\n" x (integer->colour x))
	    oport))
	'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
      (display "
      .meta { color: #999 }
      // -->
      </style>
      </head>
      <body>
      <ol>\n" oport)
    (let loop ()
      (if (eof-object? (peek-char iport))
          (begin
            (display "</ol><p id=\"foot\">Automatic markup by irc2html.scm in " oport)
            (display (- (current-seconds) start) oport)
            (display " second(s) runtime</p></body></html>" oport)
            (display "logcolour done in " eport)
            (display (- (current-seconds) start) eport)
            (display " second(s)\n" eport)
            (exit)))
      (display (link-string (colour-line (read-line iport))) oport)
      (loop))))

(process-log
  (current-input-port)
  (if (> (vector-length argv) 0) (vector-ref argv 0) "IRC log file")
  (current-output-port)
  (current-error-port))

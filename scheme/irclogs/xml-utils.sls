;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2008 Andreas Rottmann <a dot rottmann at gmx dot at>

;; This file is based on SSAX's SXML-to-HTML.scm and is in the public
;; domain.
;;
;; CDATA escaping based on serializer.scm code from sxml-tools, also
;; public domain.

;;; Commentary:
;;
;; A simple interface to XML parsing and serialization.
;;
;;; Code:
#!r6rs

(library (irclogs xml-utils)
  (export sxml->xml
          xml-cdata-escape
          universal-sxslt-rules)
  (import (rnrs)
          (only (srfi :13)
                string-index
                string-concatenate-reverse)
          (only (xitomatl ssax private-5-1 util)
                make-char-quotator)
          (xitomatl ssax raise)
          (xitomatl ssax tree-trans))

;; Universal transformation rules. Works for all XML.

;;@ A set of @code{pre-post-order} rules that transform any SXML tree
;; into a form suitable for XML serialization by @code{(sxml transform)}'s
;; @code{SRV:send-reply}. Used internally by @code{sxml->xml}.
(define universal-sxslt-rules
  `((^
     ((*DEFAULT* . ,(lambda (attr-key . value) ((enattr attr-key) value))))
     . ,(lambda (trigger . value) (list '^ value)))
    (*ENTITY*    . ,(lambda (tag name) (list "&" name ";")))
    ;; Is this right for entities? I don't have a reference for
    ;; public-id/system-id at the moment...
    (*DEFAULT*   . ,(lambda (tag . elems) (apply (entag tag) elems)))
    (*TEXT*      . ,(lambda (trigger str)
                      (if (string? str) (string->escaped-xml str) str)))))

;;@ Serialize the sxml tree @var{tree} as XML, writing to the textual
;; output port @var{port}.
(define (sxml->xml sxml port)
  (let ((tree (pre-post-order sxml universal-sxslt-rules)))
    (let loop ((tree tree) (result #f))
      (cond
       ((null? tree) result)
       ((not (car tree)) (loop (cdr tree) result))
       ((null? (car tree)) (loop (cdr tree) result))
       ((eq? #t (car tree)) (loop (cdr tree) #t))
       ((pair? (car tree))
        (loop (cdr tree) (loop (car tree) result)))
       ((procedure? (car tree))
        ((car tree) port)
        (loop (cdr tree) #t))
       (else
        (display (car tree) port)
        (loop (cdr tree) #t))))))

;; The following two functions serialize tags and attributes. They are
;; being used in the node handlers for the post-order function, see
;; above.

(define (check-name name)
  (let* ((str (symbol->string name))
         (i (string-index str #\:))
         (head (or (and i (substring str 0 i)) str))
         (tail (and i (substring str (+ i 1) (string-length str)))))
    (and i (string-index (substring str (+ i 1) (string-length str)) #\:)
         (parser-error "Invalid QName: more than one colon" name))
    (for-each
     (lambda (s)
       (and s
            (or (char-alphabetic? (string-ref s 0))
                (eq? (string-ref s 0) #\_)
                (parser-error "Invalid name starting character" s name))
            (string-for-each
             (lambda (c)
               (or (char-alphabetic? c) (string-index "0123456789.-_" c)
                   (parser-error "Invalid name character" c s name)))
             s)))
     (list head tail))))

(define (entag tag)
  (check-name tag)
  (lambda elems
    (if (and (pair? elems) (pair? (car elems)) (eq? '^ (caar elems)))
        (list #\< tag (cdar elems)
              (if (pair? (cdr elems))
                  (list #\> (cdr elems) "</" tag #\>)
                  " />"))
        (list #\< tag
              (if (pair? elems)
                  (list #\> elems "</" tag #\>)
                  " />")))))

(define (enattr attr-key)
  (check-name attr-key)
  (let ((attr-str (symbol->string attr-key)))
    (lambda (value)
      (list #\space attr-str
            "=\"" (and (not (null? value)) value) #\"))))

;; Given a string, check to make sure it does not contain characters
;; such as '<' or '&' that require encoding. Return either the original
;; string, or a list of string fragments with special characters
;; replaced by appropriate character entities.

(define string->escaped-xml
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))

;; Returns #f if a given character `ch' is in XML character range
;; Otherwise, returns a string representing the character reference for that
;; character
(define (xml-char-escaped ch)
  (let ((code (char->integer ch)))
    (if (or (= code 9) (= code 10) (= code 13)
            (and (>= code 32) (<= code 55295))
            (and (>= code 57344) (<= code 65533))
            (>= code 65536))
        #f
        (string-append "&#" (number->string code) ";"
                       ))))

;; Represents a given string `str' as a CDATA section. If @2 is given,
;; it is applied to a string that contains only characters in the XML
;; character range. The default for @2 is to return the string
;; enclosed in "<![CDATA[" and "]]>".
(define xml-cdata-escape
  (case-lambda
    ((str wrap)
     (let ((flush-buffer
            ;; If a `buffer' is non-empty, converts it to a CDATA string and
            ;; cons'es this string to `res'. Returns a new res
            (lambda (buffer res)
              (if (null? buffer)
                  res
                  (cons (wrap (list->string (reverse buffer))) res)))))
       (let loop ((src (string->list str))
                  (buffer '())
                  (res '("")))
         (cond
          ((null? src)
           (string-concatenate-reverse (flush-buffer buffer res)))
          ((xml-char-escaped (car src))
           => (lambda (charref)
                (loop (cdr src)
                      '()
                      (cons charref (flush-buffer buffer res)))))
          ((and (char=? (car src) #\])
                (not (null? buffer))
                (char=? (car buffer) #\]))
           (loop (cdr src)
                 '()
                 (cons (string (car buffer) (car src)) ;= "]]"
                       (flush-buffer (cdr buffer) res))))
          (else                         ; any other character
           (loop (cdr src)
                 (cons (car src) buffer)
                 res))))))
    ((str)
     (xml-cdata-escape str (lambda (s)
                             (string-append "<![CDATA[" s "]]>"))))))

)

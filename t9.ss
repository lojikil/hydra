#!/usr/bin/env vesta
(load 'enyalios.ss)

(define (sample-compiler l out)
    (if (null? l)
        #v
        (let ((obj (generate-code (car l) '() #t {} {})))
            (display "IL code: \n")
            (write obj)
            (newline)
            (il->c (cadr obj) 0 out)
            (newline out)
            (newline out)
            (display "/* ===== */\n\n" out)
            (sample-compiler (cdr l) out))))

;; we have two procedures, foo & bar, that have the same results, but one uses
;; cond and the other uses if, respectively
(define kod0 '(define (foo x) (cond (< x 5) "foo" (< x 10) "bar" else "wut?")))
(define kod1 '(define (bar x) (if (< x 5) "foo" (if (< x 10) "bar" "wut?"))))
(define fdout (open "9test.c" :write))
(sample-compiler (list kod0 kod1) fdout)
(close fdout)

#!/usr/bin/env vesta

(load 'enyalios.ss)

(define (enyalios@load in)
    '())

(define (enyalios@loop code out)
    (if (null? code)
        (display "\nfinished compiling\n")
        (begin
            (if (and
                    (pair? o)
                    (eq? (car o) 'define))
                (display (format "COMPILING: ~a~%" (caadr o)))
                #v)
            (il->c
                (generate-code (car code) '() #f {})
                0
                out)
                (enyalios@loop (cdr code) out))))

(define (enyalios in-file out-file init)
    (let* ((inf (open in-file :read))
          (outf (open out-file :write))
          (code (enyalios@load inf)))
        (display (format "LOADING: ~a~%" in-file))
        (enyalios@loop code outf)
        (close inf)
        (close outf)))

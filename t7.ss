#!/usr/bin/env vesta

(load 'enyalios.ss)

(define (enyalios@load in)
    (with r (read in)
        (cond
            (eof-object? r) '()
            (and
                (pair? r)
                (eq? (car r) 'define)
                (pair? (cadr r)))
                (begin
                    (set-arity! (caadr r) (cdadr r))
                    (cons
                        r
                        (enyalios@load in)))
            (and
                (pair? r)
                (eq? (car r) 'define)
                (pair? (cddr r))
                (or
                    (eq? (caddr r) 'fn)
                    (eq? (caddr r) 'lambda)))
                (begin
                    (set-arity! (cadr r) (car (cdaddr r)))
                    (cons
                        r
                        (enyalios@load in)))
            else
                (cons
                    r
                    (enyalios@load in)))))

(define (enyalios@loop code out)
    (if (null? code)
        (display "\nfinished compiling\n")
        (let ((o (car code)))
            (if (and
                    (pair? o)
                    (eq? (car o) 'define))
                (display (format "COMPILING: ~a~%" (caadr o)))
                #v)
            (il->c
                (generate-code o '() #f {})
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

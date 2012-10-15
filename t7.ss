#!/usr/bin/env vesta

(load 'enyalios.ss)

(define (enyalios@load in)
    (with r (read in)
        (if (eof-object? r)
            '()
            (begin
                (cond
                    (and
                        (pair? r)
                        (or
                            (eq? (car r) 'define)
                            (eq? (car r) 'def))
                        (pair? (cadr r)))
                        (set-arity! (caadr r) (cdadr r))
                    (and
                        (pair? r)
                        (or
                            (eq? (car r) 'define)
                            (eq? (car r) 'def))
                        (pair? (cddr r))
                        (or
                            (eq? (caaddr r) 'fn)
                            (eq? (caaddr r) 'lambda)))
                        (set-arity! (cadr r) (car (cdaddr r)))
                    else #v)
                (cons
                    r
                    (enyalios@load in))))))

(define (enyalios@loop code out)
    (if (null? code)
        (display "\nfinished compiling\n")
        (let ((o (car code)))
            (if (and
                    (pair? o)
                    (or (eq? (car o) 'define)
                        (eq? (car o) 'def)))
                (display (format "COMPILING: ~a~%" (cadr o)))
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

#!/usr/bin/env vesta

(load 'enyalios.ss)

(define (enyalios@load in)
    (with r (read in)
        (if (eof-object? r)
            '()
            (begin
                (if (and
                        (pair? r)
                        (or
                            (eq? (car r) 'define)
                            (eq? (car r) 'def)))
                    (cond
                        (symbol? (cadr r))
                            (if (and
                                (pair? (caddr r))
                                (or
                                    (eq? (car (caddr r)) 'lambda)
                                    (eq? (car (caddr r)) 'fn)))
                                (set-arity! (caadr r) (cdadr r))
                                #v)
                        (pair? (cadr r))
                            (set-arity! (caadr r) (car (cdaddr r)))
                        else #v)
                    #v)
                (cons
                    r
                    (enyalios@load in))))))

(define (enyalios@dump-lambdas lams out)
    (if (null? lams)
        #v
        (begin
            (il->c
                (car lams)
                0
                out)
                (enyalios@dump-lambdas (cdr lams) out))))

(define (enyalios@compile-loop code)
    (if (null? code)
        (begin
            (display "\nfinished compiling\n")
            '())
        (let ((o (car code)))
            (if (and
                    (pair? o)
                    (or (eq? (car o) 'define)
                        (eq? (car o) 'def)))
                (display (format "COMPILING: ~a~%" (cadr o)))
                #v)
            (cons 
                (cadr (generate-code o '() #f {} {}))
                (enyalios@compile-loop (cdr code))))))

(define (enyalios@dump-headers names d out)
    (if (null? names)
        #v
        (let* ((name (car names))
               (data (nth d name)))
            (display "SExp *" out)
            (display (car names) out)
            (display "(" out)
            (if (null? (nth data 1))
                #v
                (display
                    (string-join 
                        (map (fn (x) x "SExp *") (nth data 1))
                        ", ")
                    out))
            (display ");\n" out)
            (enyalios@dump-headers (cdr names) d out))))

(define (enyalios in-file out-file init)
    (let* ((inf (open in-file :read))
          (outf (open out-file :write))
          (code (enyalios@load inf))
          (obj-code (enyalios@compile-loop code)))
        (display (format "LOADED: ~a~%" in-file))
        (enyalios@dump-headers (keys *ulambdas*) *ulambdas* outf)
        (display "\n" outf)
        (enyalios@dump-lambdas *ooblambdas* outf)
        (enyalios@dump-lambdas obj-code outf)
        (close inf)
        (close outf)))

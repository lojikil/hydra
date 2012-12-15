#!/usr/bin/env vesta

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
                                (set-arity! (cadr r) (car (cdaddr r)))
                                #v)
                        (pair? (cadr r))
                            (set-arity! (caadr r) (cdadr r))
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

(define (enyalios@compile-loop code env-name)
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
                (cadr (generate-code o '() #f {} (dict "env" env-name)))
                (enyalios@compile-loop (cdr code))))))

(define (enyalios@dump-prototypes names d out) 
    (if (null? names)
        #v
        (let* ((name (car names))
               (data (nth d name)))
            (display "SExp *" out)
            (display (cmung (car names)) out)
            (display "(" out)
            (if (null? (nth data 1))
                #v
                (display
                    (string-join 
                        (map (fn (x) x "SExp *") (nth data 1))
                        ", ")
                    out))
            (display ");\n" out)
            (enyalios@dump-prototypes (cdr names) d out))))

(define (enyalios in-file out-file init)
    (let* ((inf (open in-file :read))
          (outf (open out-file :write))
          (code (enyalios@load inf))
          (env-name (gensym "enyalios"))
          (obj-code (enyalios@compile-loop code env-name)))
        (display (format "LOADED: ~a~%" in-file))
        (enyalios@dump-prototypes (keys *ulambdas*) *ulambdas* outf)
        (display "Symbol *" outf)
        (display env-name outf)
        (display ";\n\n" outf)
        (enyalios@dump-lambdas *ooblambdas* outf)
        (enyalios@dump-lambdas obj-code outf)
        (close inf)
        (close outf)))

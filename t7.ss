(load 'enyalios.ss)

(define (enyalios@loop in out)
    (with o (read in)
        (if (eof-object? o)
            (display "\nfinished compiling\n")
            (begin
                (if (and
                        (pair? o)
                        (eq? (car o) 'define))
                    (display (format "~a~%" (cadr o)))
                    #v)
                (il->c
                    (generate-code o '() #f {})
                    0
                    out)
                (enyalios@loop in out)))))

(define (enyalios in-file out-file init)
    (let ((inf (open in-file :read))
          (outf (open out-file :write)))
        (enyalios@loop inf outf)
        (close inf)
        (close outf)))

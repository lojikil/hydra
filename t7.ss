(load 'enyalios.ss)

(define (enyalios@loop in out)
    #f)

(define (enyalios in-file out-file init)
    (let ((inf (open in-file :read))
          (outf (open out-file :write)))
        (enyalios@loop inf outf)
        (close inf)
        (close outf)))

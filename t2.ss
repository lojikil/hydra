(load 'enyalios.ss)
(define (show x) (display "x: ") (write x) (newline) x)
(display (compile-procedure '((x y z) (+ x y z)) 'foo #t))


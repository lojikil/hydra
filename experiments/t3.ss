(load "../enyalios.ss")
(define (show x) (display "x: ") (write x) (newline) x)
(display (compile-procedure '((x y z) (+ (+ x 1) (* y 2) z)) 'foo #t))

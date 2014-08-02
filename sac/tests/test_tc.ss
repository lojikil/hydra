(define (foo x)
    (if (> x 10)
        (foo (- x 1))
        x))


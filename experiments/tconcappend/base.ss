(define (base-iota x)
    (if (>= x 0)
        (cons (random 10000) (base-iota (- x 1)))
        '()))

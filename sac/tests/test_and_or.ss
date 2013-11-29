(define (and-test x y z)
    (and
        (> x y)
        (< y z)))

(define (or-test x y z)
    (or
        (> x y)
        (< x z)))

(define (and-test-if x y z)
    (if (and
            (> x y)
            (< y z))
        (display "Yes!\n")
        (display "No?\n")))

(define (or-test-if x y z)
    (if (or
            (> x y)
            (< x z))
        (display "Yes!\n")
        (display "No?\n")))

(define (and-test-cond x y z)
    (cond
        (and (> x y) (< y z)) (display "yes!\n")
        else (display "no!\n")))

(define (or-test-cond x y z)
    (cond
        (or (> x y) (< x z)) (display "yes!\n")
        else (display "no?\n")))

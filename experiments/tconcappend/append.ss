(load 'base.ss)

(define (append-test x y)
    ;(display "y is ")
    ;(write y)
    ;(newline)
    (if (>= x 0)
        (append-test (- x 1) (append y (base-iota x)))
        y))

(write (append-test 1000 '()))
(newline)

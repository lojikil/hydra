;; a simple test to check if Eprime can compile
;; mutual functions

(define (foo x) (bar x))
(define (bar x)
    (if (>= x 10)
        (foo (- x 1))
        x))

(display (bar 20))
(display "\n")

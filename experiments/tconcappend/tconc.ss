(load 'base.ss)

;(define (foo x) (display "in foo\n") (tconc-splice! x (base-iota 10)))

;; there's a bug in how Vesta implements tconc_splice; need to fix that first
;; before this can be used to test... well, just about anything.

(define (tconc-test x y)
    (if (>= x 0)
        (begin
            ;(foo y)
            (tconc-splice! y (make-tconc (base-iota x)))
            (tconc-test (- x 1) y))
        (tconc->pair y)))

(write (tconc-test 1000 (make-tconc '())))
(newline)

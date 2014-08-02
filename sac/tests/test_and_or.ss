(if (eq? (digamma-implementation) 'typhon)
    (load 'typhon_prelude.ss)
    #v)

(load 'assert.ss)

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

;; and-test: one passing test, one failing test
(assert (and-test 10 9 20) #t "and-test true")
(assert (and-test 9 10 20) #f "and-test false")
(assert (or-test 19 8 29) #t "or-test true")
(assert (or-test 9 10 8) #f "or test false")

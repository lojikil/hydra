(define (test0 x)
    (< x 10))

(define (test1 x y)
    (< x y))

(define (test2 x)
    (cons (< x 10) '()))

(define (test3 x y)
    (cons (< x y) '()))

(define (test4 x)
    (if (< x 10)
        'yes
        'no))

(define (test5 x y)
    (if (< x y)
        'yes
        'no))

(define (test6 x)
    (eq? x 10))

(define (test7 x)
    (= x 10))

(define (test8 x)
    (if (eq? x 10)
        'yes
        'no))

(define (test9 x)
    (if (= x 10)
        'yes
        'no))

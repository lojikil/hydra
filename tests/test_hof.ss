;; test compilation of primitives and
;; other functions with Enyalios

(define (null? x) (eq? x '()))

(define (foldl proc init lst)
    (cond
        (null? lst) init
        (null? cdr lst) (proc init (car lst))
        else (foldl proc (proc init (car lst)) (cdr lst))))

(define (bar a b c d e f g h i j k l m n o p)
    (foldl + 0 (list a b c d e f g h i j k l m n o p)))

(define (foo1 a b)
    (+ a b))

(define (foo a b c d e f g h i j k l m n o p)
    (foldl foo1 0 (list a b c d e f g h i j k l m n o p)))

(define (foo2 a b c d e f g h i j k l m n o p)
    (foldl (lambda (x y) (+ x y)) 0 (list a b c d e f g h i j k l m n o p)))

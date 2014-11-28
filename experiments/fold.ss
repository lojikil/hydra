(define (foldr proc init lst)
    (cond
        (null? lst) init
        (null? (cdr lst)) (proc init (car lst))
        else (proc (car lst) (foldr proc init (cdr lst)))))

(define (foldl proc init lst)
    (cond
        (null? lst) init
        (null? cdr lst) (proc init (car lst))
        else (foldl proc (proc init (car lst)) (cdr lst))))

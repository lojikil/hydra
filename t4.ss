(load 'enyalios.ss)
(define x '(define (member? x lst)
    (if (eq? lst '())
        #f
        (if (eq? (car lst) x)
            #t
            (member? x (cdr lst))))))
(display (generate-code x '() #f))

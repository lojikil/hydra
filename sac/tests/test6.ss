(define (foo f x)
    (display "foo: ")
    (display x)
    (display "\n")
    (f x))

(define (main)
    (foo (lambda (z) (* z z)) 10)
    (foo (lambda (z) (display "z == ") (display z) (display "\n") (* z z)) 20))

(main)
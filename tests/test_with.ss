(define (foo x)
    (with z (+ x x)
        (display z)
        (display "\n")
        z))
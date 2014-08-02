(define-struct kons (kar kdr))

(define (foo x)
    (make-kons x (foo (- x 1))))

(define (bar klist)
    (display (kons-kar klist)))

(define (baz klist)
    (if (kons? klist)
        (display "Yes!\n")
        (display "No!?\n")))

(define (main)
    (bar (foo 10)))

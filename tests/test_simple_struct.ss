(define-struct kons (kar kdr))

(define (foo x)
    (make-kons x (foo (- x 1))))

(define (bar klist)
    (display (kons-kar klist)))

(define (main)
    (bar (foo 10)))

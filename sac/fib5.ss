(define (fib n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(display (fib 11))
(newline)
(display (fib 21))
(newline)

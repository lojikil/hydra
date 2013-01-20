; just a simple test to keep around for testing E' with
; you can safely ignore this :D
(define (fib n)
	(if (<= n 1)
		1	
		(+ (fib (- n 1)) (fib (- n 2)))))

(define (scheme_main) 
    (display (fib 10))
    (display "\n")
    (display (fib 20))
    (display "\n")
    (display (fib 30))
    (display "\n")
    (display (fib 32))
    (display "\n"))
(scheme_main)

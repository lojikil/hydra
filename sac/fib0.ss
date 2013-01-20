; just a simple test to keep around for testing E' with
; you can safely ignore this :D
(define (fib i j n)
	(if (<= n 0)
		i	
		(fib (+ i j) i (- n 1))))

(define (scheme_main) 
    (display (fib 0 1 10))
    (display "\n")
    (display (fib 0 1 20))
    (display "\n")
    (display (fib 0 1 30))
    (display "\n")
    (display (fib 0 1 32))
    (display "\n"))
(scheme_main)

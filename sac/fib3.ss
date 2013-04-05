; just a simple test to keep around for testing E' with
; you can safely ignore this :D

;; So, the vmperf hydra seems a bit slower with this now. However, since the
;; fixes to cslice are now in, perhaps the work in the original Hydra can be
;; combined with the other fixes applied in vmperf? Test that tomorrow...
(define (fib n)
	(if (<= n 0)
		0	
        (if (= n 1)
            1
		    (+ (fib (- n 1)) (fib (- n 2))))))

(define (scheme_main) 
    (fib 10)
    (fib 20))
(scheme_main)

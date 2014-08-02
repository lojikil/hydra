(define (foreach proc c)
    (if (empty? c)
        #v
        (begin
            (proc (first c))
            (foreach proc (rest c)))))

(def zip (fn (xs ys)
	(if (empty? xs)
		'()
		(cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys))))))

(define (build-environment environment stack params)
    "Adds a new window to the environment, removes |params| items from the stack
     and binds those values in the new window. It returns a list of environment and
     the new stack."
    ;; rough match; doesn't take into account optional parameters.
    ;; would probably be better to have an inner function that iterates over
    ;; the parameters & returns those that match. It would then be easier to 
    ;; have optional parameters...
    (let ((ls (length stack)) (lp (length params)) (nu-env {}))
        (if (< ls lp)
            (error "non-optional parameters are not statisfied by stack items in build-environment")
            (if (= lp 0)
                (list (cons nu-env environment) (cdr stack))
                (begin 
                    (foreach
                        (lambda (x)
                            (cset! nu-env (car x) (car (cdr x))))
                        (zip params (cslice stack 0 lp)))
                    (list (cons nu-env environment) (cslice stack lp ls)))))))

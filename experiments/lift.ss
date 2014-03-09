#!/usr/bin/env vesta

;; simple test of lambda lifting
;; uses everything defined in free.ss
;; to determine which items to add to 
;; lifted lambda, and which calls to
;; rewrite
(load 'free.ss)

(define (walk-lambda! code lambda-info bounds)
    "walks a define object for lambdas to lift"
    (if (null? code)
        #v
        (let* ((obj (car code)) ;; really need an check here to see if this is actually a lambda
               (ret (collect-free-vars obj bounds '() '()))A
               (nu-bounds (append (car ret) bounds)))
            (cset! lambda-info name ret)
            (walk-lambda! (cdr code) lambda-info nu-bounds))))

(define (lift-lambda code lm)
    "does the actual work of lifting a lambda"
    #f)

(define (rewrite-call code lms)
    "returns a call rewritten with all parameters filled"
    ;; lifted lambda: foo, params: x,y, free: z
    ;; original code: (foo x y)
    ;; new code: (foo x y z)
    #f)

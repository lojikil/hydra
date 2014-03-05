#!/usr/bin/env vesta

;; simple test of lambda lifting
;; uses everything defined in free.ss
;; to determine which items to add to 
;; lifted lambda, and which calls to
;; rewrite
(load 'free.ss)

(define (scan-lambdas code)
    "scans a define object for lambdas to lift"
    #f)

(define (lift-lambda code lm)
    "does the actual work of lifting a lambda"
    #f)

(define (rewrite-call code lms)
    "returns a call rewritten with all parameters filled"
    ;; lifted lambda: foo, params: x,y, free: z
    ;; original code: (foo x y)
    ;; new code: (foo x y z)
    #f)

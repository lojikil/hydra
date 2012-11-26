#!/usr/bin/env vesta
(load "../hydra.ss")
(define code '(car (cdr (cons 1 (cons 2 '())))))
(define hlap (hydra@compile code *tlenv*))
(display "original source code: ")
(write code)
(newline) 
(display "HLAP output: ")
(display hlap)
(newline)
(display (hydra@vm hlap *tlenv* 0 '() '()))
(newline)

;; To Do:
;; tests for:
;;  - basic structures
;;  - basic values
;;  - syntax: if, define, set!
;;  - primitives: car/cdr/cons/&c.
;;  - lambdas
;;  - HOFs
;;  - SRFIs
;;  - Datalog support
;;  - Define-syntax
;;  - Same should be done for Eprime...

;; Hand code the HLAP here, and run tests.

;; need macro/syntax:

;; (hydra-test digamma expected-hlap expected-value)
;; this would be 100x cleaner with syntax...

(define-macro hydra-test (digamma-code expected-hlap expected-value)
    (list 'with 'h (list 'hydra@compile (list 'quote digamma-code) '*tlenv*)
        (list 'if (list 'not (list 'equal? 'h (list 'quote expected-hlap)))
            (list 'display (list 'format "[-] HLAP generation failed for ~a; expected ~a, got ~a~%" (list 'quote digamma-code) (list 'quote expected-hlap) 'h))
            (list 'with 'e (list 'hydra@vm 'h '*tlenv* 0 '() '())
                (list 'if (list 'not (list 'equal? 'e (list 'quote expected-value)))
                    (list 'display (list 'format "[-] Eval failed for ~a; expected ~a, got ~a~%" (list 'quote digamma-code) (list 'quote expected-value) 'e))
                    (list 'display (list 'format "[+] test passed for ~a~%" (list 'quote digamma-code))))))))

(hydra-test (cons 1 2) ((3 2) (3 1) (2)) (1 . 2))
(hydra-test (cons 1 (cons 2 '())) ((4) (3 2) (2) (3 1) (2)) (1 2))
(hydra-test (car (cons 1 2)) ((3 2) (3 1) (2) (0)) 1)
(hydra-test (cdr (cons 1 2)) ((3 2) (3 1) (2) (1)) 2)
(hydra-test #t ((3 #t)) #t)
(hydra-test '(1 2 3) ((3 (1 2 3))) (1 2 3))
(hydra-test "test" ((3 "test")) "test")
(hydra-test (+ 1 2) ((3 0) (3 1) (6) (3 2) (6)) 3)
(hydra-test (+ 1 2 3 4) ((3 0) (3 1) (6) (3 2) (6) (3 3) (6) (3 4) (6)) 10)
(hydra-test (- 1 2) ((3 1) (3 2) (5)) -1)
(hydra-test (- 1 2 3 4) ((3 1) (3 2) (5) (3 3) (5) (3 4) (5)) -8)
(hydra-test (* 1 2) ((3 1) (3 1) (7) (3 2) (7)) 2)
(hydra-test (* 1 2 3 4) ((3 1) (3 1) (7) (3 2) (7) (3 3) (7) (3 4) (7)) 24)
(hydra-test (/ 1 2) ((3 1) (3 2) (8)) 1/2)
(hydra-test (/ 1 2 3 4) ((3 1) (3 2) (8) (3 3) (8) (3 4) (8)) 1/24)
(hydra-test (eq? 1 1) ((3 1) (3 1) (27)) #t)
(hydra-test (= 1 1) ((3 1) (3 1) (26)) #t) ;; unfortunately, the comparison functions in Hydra are arity 2 atm...
(hydra-test (< 1 2) ((3 1) (3 2) (9)) #t)
(hydra-test (< 1 1) ((3 1) (3 1) (9)) #f)
(hydra-test (> 2 1) ((3 2) (3 1) (10)) #t)
(hydra-test (> 1 2) ((3 1) (3 2) (10)) #f)
(hydra-test (<= 1 2) ((3 1) (3 2) (11)) #t)
(hydra-test (<= 2 1) ((3 2) (3 1) (11)) #f)
(hydra-test (<= 2 2) ((3 2) (3 2) (11)) #t)
(hydra-test (>= 2 1) ((3 2) (3 1) (12)) #t)
(hydra-test (>= 1 2) ((3 1) (3 2) (12)) #f)
(hydra-test (>= 1 1) ((3 1) (3 1) (12)) #t)

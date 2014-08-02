(load 'tc.ss)

;; Need to run a series of tests to verify that the type checker is working as we'd expect
;; Basically, we need to be able to type arbitrary expressions, or show that they cannot
;; type check in the gradually typed system that Digamma/PreDigamma is shooting for.

(define *test* 0)

(define (tc-test o0 o1 env)
    (display 
        (format
            "(test ~a): "
            *test*))
    (write o0)
    (display " ")
    (write o1)
    (display " ~> ")
    (write (run* o0 o1 env))
    (newline)
    (set! *test* (+ *test* 1)))

(tc-test 'Any 10 '())
(tc-test 'Any #\c '())
(tc-test 'Any "foo" '())
(tc-test 'Integer 10 '())
(tc-test 'Integer 3/4 '())
(tc-test 'String 10 '())
(tc-test 'Integer '(? x) '((x 10)))
(tc-test 'Nil '() '())
;; there *MUST* be a better dichotomy between these two types
;; one is saying it's a Pair containing an Integer and a String
;; The second is saying both a Pair and an Integer, or a
;; compound type...
;; Maybe run can delineate between the two?
;; (run* (Pair Integer) some-val env) 
;; vs
;; (run* ((Pair Integer)) some-val env)
;; the first being a any Pair and an integer, the second
;; being integer Pair...
(tc-test '(Integer String) '(10 "this is a test") '())
(tc-test '((Pair Integer)) '((10 11 12 13)) '())
(tc-test '(Pair Integer) '((10 11 12 13) 14) '())

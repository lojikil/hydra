(display "display works\n")
(write "write works")

(display "\nan atom a: ")
(write 'a)

(display "\na key: ")
(write :a)

(display "\na string \"test\": ")
(write "test")

(display "\nan integer: ")
(display 1)

(display "\na float: ")
(display 1.3)

(display "\na rational: ")
(write 3/4)

(display "\na complex: ")
(write 3+4i)

(display "\nthe usual boolean cast of characters: ")
(write #t)
(display " ")
(write #f)

(display "\nthe usual goal cast of characters: ")
(write #s)
(display " ")
(write #u)

(display "\na list: ")
(write '(1 2 3 4))

(display "\na vector: ")
(write [1 2 3 4 5])

(display "\na dictionary: ")
(write {:a "test"})

(display "\nkeys from the same dictionary: ")
(write (keys {:a "test"}))

(display "\ndefine test: ")
(define a 1)
(display "a is ")
(write a)

(display "\nset! test: ")
(set! a 2)
(display "a should be equal to 2 now: ")
(write a)

(display "\nif test: ")
(if #t
    (display "in the true branch")
    (display "in the false branch"))

(display "\nif-false test: ")
(if #f
    (display "in the true branch")
    (display "in the false branch"))

(display "\ndefine-if test: ")
(define a1 #t)
(if a1
    (display "in the true branch")
    (display "in the false branch"))

(display "\n+ tests: same numeric types: \n")
(write (+ 1 2))
(newline)
(write (+ 1.0 2.0))
(newline)
(write (+ 1/2 3/4))
(newline)
(write (+ 1+2i 3+4i))

(display "\n+ tests, with different numeric types: \n")
(write (+ 1 2.0))
(newline)
(write (+ 1 2/3))
(newline)
(write (+ 1 3.0+4.0i))
(newline)

(display "\n- tests: same numeric types: \n")
(write (- 1 2))
(newline)
(write (- 1.0 2.0))
(newline)
(write (- 1/2 3/4))
(newline)
(write (- 1+2i 3+4i))

(display "\n- tests, with different numeric types: \n")
(write (- 1 2.0))
(newline)
(write (- 1 2/3))
(newline)
(write (- 1 3.0+4.0i))
(newline)

(display "\n* tests: same numeric types: \n")
(write (* 1 2))
(newline)
(write (* 1.0 2.0))
(newline)
(write (* 1/2 3/4))
(newline)
(write (* 1+2i 3+4i))

(display "\n* tests, with different numeric types: \n")
(write (* 1 2.0))
(newline)
(write (* 1 2/3))
(newline)
(write (* 1 3.0+4.0i))
(newline)

(display "\n/ tests: same numeric types: \n")
(write (/ 1 2))
(newline)
(write (/ 1.0 2.0))
(newline)
(write (/ 1/2 3/4))
(newline)
(write (/ 1+2i 3+4i))

(display "\n/ tests, with different numeric types: \n")
(write (/ 1 2.0))
(newline)
(write (/ 1 2/3))
(newline)
(write (/ 1 3.0+4.0i))
(newline)

(display "multiple argument +: ")
(write (+ 1 2.0 3/4 5+6i))
(newline)

(display "multiple argument -: ")
(write (- 1 2.0 3/4 5+6i))
(newline)

(display "multiple argument *: ")
(write (* 1 2.0 3/4 5+6i))
(newline)

(display "multiple argument /: ")
(write (/ 1 2.0 3/4 5+6i))
(newline)

(display "begin test: ")
(begin
    (display "does ")
    (display "begin ")
    (display "work?"))

(display "\nanonymous lambda test: ")
((lambda () 
    (display "does ")
    (display "lambda ")
    (display "work?")))

(display "\ndefine lambda test: ")
(define foo (lambda (x) (+ x x)))
(display "(foo 10) should be 20: ")
(write (foo 10))

(display "\ndefine lambda test, without explicit fn/lambda: ")
(display "(bar 10) should be 20: ")
(define (bar x) (+ x x))
(write (bar 10))

(display "\nscope test; a2 before should be 1000, in should be equal to 20, and after 1000 again: ")
(define a2 1000)
(display "\nbefore, a2 is ")
(write a2)
((fn (a2) (display "\nin, a2 is ") (write a2)) 20)
(display "\nafter, a2 is ")
(display a2)

(display "\nnested scopes: ")
(define (foo0 x)
    (display "\nx in foo0 is: ")
    (write x)
    (define (foo1 x) (display "\nx in foo1 is ") (write x) (newline))
    (foo1 (+ x 10))
    (display "x in foo0 is: ")
    (write x))
(foo0 20)

(display "\na load test: ")
(load 'load-test0.ss)

(display "\nlcm & gcd tests:\n(gcd 84 99) ")
(display (gcd 84 99))
(display "\n(lcm 114 17) ")
(display (lcm 114 17))

(display "\n")

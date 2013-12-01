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

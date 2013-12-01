(display "display works\n")
(write "write works")
(newline)

(display "an atom a: ")
(write 'a)

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

(display "\n")

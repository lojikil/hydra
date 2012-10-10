(define (say_hello f)
    (cond
        (eq? f "test") (display "hello, tester!\n")
        (eq? f 34) (display "34?\n")
        (<= f 10) (display "A healthy dose of humility\n")
        else (display "lolwut?\n")))

(define (say-foo)
    (display "foo\n"))

(define (say-hello f)
    (display "hello, ")
    (display f)
    (display "\n"))

(say-foo)
(say-hello "test") 

;; test of CASE forms
(define (test-case instr)
    (case instr
        (0) (display "test-case::0\n")
        (1 2) (display "test-case::1 or 2\n")
        (3 4 5 6 7) (display "test-case::3-7\n");
        else (display "test-case::default\n")))


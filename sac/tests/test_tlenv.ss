;; regression test to check if the top-level environment
;; is correctly added to calls to write in anonymous
;; lambdas. When Enyalios' code generator cannot find
;; the current environment's name, it simply generates
;; "tlenv", which *could* be fixed (just add a macro
;; in murts.h), but I'd rather properly fix it in the
;; code system

(define (foreach proc c)
    (if (empty? c)
        #v
        (begin
            (proc (first c))
            (foreach proc (rest c)))))

(define (foo some-var)
    (foreach
        (fn (x) (write (keys x)) (newline))
        some-var))

(define (scheme-main)
    (foo '({test: "baz"} {frob: "boo" blaz: "blah"}))
    (newline))

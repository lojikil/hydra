(define-struct base (some-member))

(define-struct foo base (some-other-member))

(define (main)
    (define blah (make-foo 10 11))
    (display (base-some-member blah))
    (display (foo-some-other-member blah)))

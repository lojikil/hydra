(define (foo x)
    (catch some-error
        (if (< x 10)
            (throw some-error "an error occurred")
            x)))

(define (non-tail-catch x)
    (catch some-other-error
        (if (< x 10)
            (throw some-error "an error occurred")
            #v)
        x))


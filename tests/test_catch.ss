(define (foo x)
    (catch some-error
        (if (< x 10)
            (throw some-error "an error occurred")
            x)))

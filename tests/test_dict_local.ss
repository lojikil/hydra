;; a local copy of test dict, since 
;; it appears I didn't push my changes

(define (test0)
    {this: 'is a: "test"})

(define (test1)
    (let ((x (dict this: 'is a: "test")))
        x))

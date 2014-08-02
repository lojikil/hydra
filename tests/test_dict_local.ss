;; a local copy of test dict, since 
;; it appears I didn't push my changes

(define (test0)
    {this: 'is a: "test"})

(define (test1)
    (let ((x (dict this: 'is a: "test")))
        x))

(define (test-string)
    (let ((x (string #\a #\b #\c)))
        x))

(define (test-string-append)
    (let ((x (string-append "test" " this " "thing")))
        x))

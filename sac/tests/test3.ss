;; This segfaults in Typhon, and throws an error in Vesta.
;; CSI works though:
;1> (define x 1)
;2> (define y '(2 3 4 5))
;3> (define z '((6 7)))
;4> `(,x ,y ,@z)
;(1 (2 3 4 5) (6 7))

(load 'test_qquote.ss)
(define x 1)
(define y '(2 3 4 5))
(define z '((6 7)))
`(,x ,y ,@z)

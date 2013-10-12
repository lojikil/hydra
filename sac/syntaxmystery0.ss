(load 'prelude.ss)
#;(load '../experiments/sr.ss)

(define-syntax define-begin-like
  ()
  ((_ x)
     (define-syntax x
       ()
         ((x . rest) '(begin . rest)))))

(define (p x) (write x) (newline) x)

(with begin (lambda (x) (p "oops"))
    (display "past the with\n")
    (define-begin-like my-begin)
    (display "past the \"define-begin-like\"\n")
    (display my-begin)
    (newline)
    (write (my-begin (p 1) (p 2)))
    (newline))

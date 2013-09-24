(load 'prelude.ss)

(define-syntax define-begin-like
  ()
    ((_ x)
     (define-syntax x
       ()
         ((_ . rest) (begin . rest)))))

(define (p x) (write x) (newline) x)

(with begin (lambda (x y) (p "oops"))
 (define-begin-like my-begin)
      (my-begin (p 1) (p 2)))

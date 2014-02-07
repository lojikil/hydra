(define (quasiquote l)
    "lifted from Nyx, the original self-hosting test interpreter for Digamma."
    (if (eq? (type l) "Pair")
     (if (eq? (type (car l)) "Pair")
      (if (eq? (car (car l)) 'unquote)
       (cons (eval (car (cdr (car l)))) (quasiquote (cdr l)))
       (if (eq? (car (car l)) 'unquote-splice)
        (append (eval (car (cdr (car l)))) (quasiquote (cdr l)))          
        (cons (quasiquote (car l)) (quasiquote (cdr l)))))
      (cons (car l) (quasiquote (cdr l))))
     l))
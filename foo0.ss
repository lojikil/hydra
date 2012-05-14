(define (set-arity! code)
    (if (and (pair? code) (or (eq? (car code) 'def) (eq? (car code) 'define)))
        (if (pair? (cadr code))
            (cset! *fnarit* (caadr code) (- (length (cdadr code)) 1))
            (if (and (pair? (caddr code)) (or (eq? (caaddr code) 'fn) (eq? (caaddr code) 'lambda)))
                (cset! *fnarit* (cadr code) (length (cadr (caddr g))))
                #v))
        #v))


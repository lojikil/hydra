; from: http://en.wikipedia.org/wiki/Unification_(computer_science)

;In Prolog:
;1. A variable which is uninstantiated—i.e. no previous unifications were
;   performed on it—can be unified with an atom, a term, or another unins-
;   tantiated variable, thus effectively becoming its alias. In many
;   modern Prolog dialects and in first-order logic, a variable cannot
;   be unified with a term that contains it; this is the so called
;   occurs check.
;2. Two atoms can only be unified if they are identical.
;3. Similarly, a term can be unified with another term if the top funct-
;   ion symbols and arities of the terms are identical and if the
;   parameters can be unified simultaneously. Note that this is a
;   recursive behavior.

(define (var? x)
    (and (pair? x) (eq? (car x) '?)))

(define (unify o0 o1 env)
    (cond
        (var? o0) 
            (cond
                (var? o1)
                    (let ((v0 (assq (cadr o0) env))
                          (v1 (assq (cadr o1) env)))
                        (cond
                            (eq? v0 #f) (list (cadr o0) o1)
                            (eq? v1 #f) (list (cadr o1) o0)
                            else (unify (cadr v0) (cadr v1) env)))
                else
                    (let ((v0 (assq (cadr o0) env)))
                        (if (eq? v0 #f)
                            (list (cadr o0) o1)
                            (unify v0 o1 env))))
        (var? o1)
            (let ((v1 (assq (cadr o1) env)))
                (if (eq? v1 #f)
                    (list (cadr o1) o0)
                    (unify o0 v1 env)))
        (eq? o0 o1) '()
        (and (pair? o0) (pair? o1))
            (with u-result (unify (car o0) (car o1) env)
                (cond
                    (pair? u-result)
                        (with n-result (unify (cdr o0) (cdr o1) (cons u-result env))
                            (if (eq? n-result #u)
                                #u
                                (append (list u-result) n-result)))
                    (not (eq? u-result #u))
                        (unify (cdr o0) (cdr o1) env)
                    else #u))
        else #u))

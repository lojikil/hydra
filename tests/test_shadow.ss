(define (not x)
    (cond
        (eq? x #f) #t
        (eq? x #u) #s
        (eq? x #s) #u
        else #f))

(define (symbol? x)
    (eq? (type x) "Symbol"))

(define (null? x)
    (eq? (type x) "Null"))

(define (hydra@error x)
    (error x))

(define (hydra@lookup item env)
    " look up item in the current environment, returning #f for not found"
    (cond
        (not (symbol? item)) item ;; to support ((fn (x) (+ x x)) (+ x x) 3)
        (null? env) (hydra@error (format "unbound variable: ~a" item)) 
        (dict-has? (car env) item) (nth (car env) item)
        else (hydra@lookup item (cdr env))))

(define (hydra@set-env! name value environment)
    " sets a value in the current environment, and returns
      an error if that binding has not been previously defined"
    (cond
        (null? environment) (hydra@error (format "SET! error: undefined name \"~a\"" name))
        (dict-has? (car environment) name)
            (cset! (car environment) name value)
        else (hydra@set-env! name value (cdr environment))))

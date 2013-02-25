;; simple syntax-rules experiment...

(define (define-syntax name literals patterns)
    "Simple version of define-syntax; Digamma's spec equates
    define-syntax with Scheme's syntax-rules for simplicity
    (yes, I expect torches when Digamma is released), so this
    is a simple, Enyalios-compliable syntax-rules implementation."
    #f)

(define (match-pattern pat form env literals)
    "Match a syntax-rules pattern defined in pat against form.
    Returns (BOOL BINDINGS*), where BOOL defines success or
    failure, and BINDINGS is an a-list."
    (cond
        (null? pat)
            (if (null? form)
                (list #t env)
                (list #f '()))
        (symbol? pat) ;; i.e. we're on the "bar" part of (foo . bar)
            (list #t (cons (list pat form) env))
        (and
            (memq (car pat) literals)
            (eq? (car pat) (car form)))
            (match-pattern (cdr pat) (cdr form) env literals)
        (symbol? (car pat)) ;; need to check ... here
            (match-pattern (cdr pat) (cdr form) (cons (list past form) env) literals)
        (pair? (car pat)) ;; same as above
            #f
        else (equal? (car pat) (car form))
            (match-pattern (cdr pat) (cdr form) env literals)))

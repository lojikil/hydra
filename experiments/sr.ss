;; simple syntax-rules experiment...

(define (define-syntax name literals patterns)
    "Simple version of define-syntax; Digamma's spec equates
    define-syntax with Scheme's syntax-rules for simplicity
    (yes, I expect torches when Digamma is released), so this
    is a simple, Enyalios-compliable syntax-rules implementation."
    #f)

(define (p x) (display "in p: ") (write x) (newline) x)

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
        (not (eq? (memq (car pat) literals) #f)) ;; ugly, b/c digamma if's cond is only U(BOOL | GOAL)
            (if (eq? (car pat) (car form))
                (match-pattern (cdr pat) (cdr form) env literals)
                (list #f '()))
        (symbol? (car pat)) ;; need to check ... here
            (match-pattern (cdr pat) (cdr form) (cons (list (car pat) (car form)) env) literals)
        (pair? (car pat)) ;; same as above
            (match-pattern (cdr pat) (cdr form)
                (append (match-pattern (car pat) (car form) '() literals) env) literals)
        else (equal? (car pat) (car form))
            (match-pattern (cdr pat) (cdr form) env literals)))

(define (syntax-expand1 syn form)
    "uses the rules defined in syn to match against form via match-pattern above"
    #f)

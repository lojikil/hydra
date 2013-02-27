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

(define (syntax-expand2 rules form literals)
    (if (null? rules)
        (list #f '())
        (with res (match-pattern (caar rules) form '() literals) ;; really need to get internal procs working in Enyalios
            (if (not (eq? (car res) #f))
                (list (cadr res) (cadar rules))
                (syntax-expand2 (cdr rules) form '() literals)))))

(define (build-syntax-result env form out)
    (cond
        (null? form) out
        (symbol? (car form)) ;; need to check ... here & below in pair?
            (with s (assq (car form) env)
                (if (eq? s #f)
                    (build-syntax-result env (cdr form) (append out (list (car form))))
                    (build-syntax-result env (cdr form) (append out (list (cadr s))))))
        (pair? (car form))
            (build-syntax-result env
                (cdr form)
                (append out (list (build-syntax-result env (car form) '()))))
        else
            (build-syntax-result env (cdr form) (append out (list (car form))))))

(define (syntax-expand1 syn form)
    "uses the rules defined in syn to match against form via match-pattern above"
    (let* ((literals (car syn))
           (rules (cdr syn))
           (result (p (syntax-expand2 rules form literals))))
        (if (not (eq? (car result) #f))
            (build-syntax-result (car result) (cadr result) '())
            #f)))

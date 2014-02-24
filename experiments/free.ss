;; simple testing of the free/bound variable analysis...

(define (show x)
    (display "%%-SHOW: ")
    (write x)
    (newline)
    x)

(define (flat-map f x)
    (display "x: ")
    (display x)
    (newline)
    (if (null? x)
        '() 
        (append
            (show (f (car x)))
            (flat-map f (cdr x)))))

(define (define-form? obj)
    (and
        (pair? obj)
        (or
            (eq? (car obj) 'define)
            (eq? (car obj) 'def))))

(define (define-lambda-var? obj)
    (or
        (and
            (symbol? (cadr obj))
            (pair? (caddr obj))
            (or
                (eq? (car (caddr obj)) 'lambda)
                (eq? (car (caddr obj)) 'fn)))))

(define (define-lambda-pair? obj)
    (pair? (cadr obj)))

(define (lambda-form? obj)
    (and
        (pair? obj)
        (or
            (eq? (car obj) 'lambda)
            (eq? (car obj) 'fn))))

(define (begin-form? obj)
    "This is really assuming that begin hasnt'
     been re-bound in the current environment. This is
     fine for PreDigamma, but as that merges with
     Digamma proper, this is going to be problematic..."
    (and
        (pair? obj)
        (eq? (car obj) 'begin)))

(define (collect-free-vars code bound-vars parents-stack free-vars)
    "a simple method to collect free-vars from a piece of code.
     Parameters:

     - `code` the procedure being checked; should be called on the cddr of the body
     - `bound-vars` vars (including the cadr of code, or the parameters) defined therein
     - `parents-stack` spaghetti stack representing parent's environment
     - `free-vars` are any vars that are not defined or parameters

     We should also be checking the arity system, and should not assume that
     appear free are; for instance, Scheme doesn't enforce that items are
     defined prior to use, and there's nothing stopping code from being
     written that way (I do it often, for instance). Would be weird, but
     I wonder if it's reasonable to avoid checking the car of a form
     for being free. There is one other method: pass in the parent's
     collected environment as a another parameter, and check *that*;
     items that are free there can be assumed to be top-level, whereas
     everything else should exist in the parent's spaghetti stack of
     free/bound vars. Food for thought.

     I've added a `parents-stack` argument to capture this; I'll first hack
     out a simpler version, then see about adding this. Shouldn't be
     terribly difficult to add back in... (famous last words)
     "
     ;; add a function, dunno what to call it, to iterate over pieces 
     ;; of code and keep the bound-vars & free-vars lists separate...
     (cond
        (null? code)
            free-vars
        (and
            (define-form? code)
            (or (define-lambda-var? code) (define-lambda-pair? obj)))
            ;; add the name to bound-vars, but nothing else, for now.
            ;; I just realized that this interface doesn't really allow
            ;; for adding to the bound-vars terribly easily. Need to return
            ;; (Pair (Pair Symbol+) (Pair Symbol+)) I guess, the first
            ;; being free vars, the second being bound vars...
            (list
                free-vars
                (cons (cadr code) bound-vars))
        (define-form? code)
            ;; ok, check each piece of `code`...
            (let ((new-bounds (cons (cadr code) bound-vars)))
                (flat-map
                    (lambda (x)
                        (collect-free-vars x new-bounds parents-stack free-vars))
                    (caddr code)))
        (begin-form? code)
            ;; iterate over each item in `code`, merging free/bound vars
            (flat-map
                (lambda (x)
                    (collect-free-vars x bound-vars parents-stack free-vars))
                (cdr code))
        (symbol? code)
            ;; need to look it up somewhere and decide...
            (let ((item (memq code bound-vars)))
                (list
                    (if (eq? item #f)
                        (cons code free-vars)
                        free-vars)
                    bound-vars))
        (pair? code)
            ;; generic form; iterate over it & collect free-vars
            ;; this isn't quite right either, as it will collect the
            ;; same vars over and over...
            (flat-map
                (lambda (x)
                    (collect-free-vars x bound-vars parents-stack free-vars))
                code)
        else
            ;; could just return #f for "this is something we don't
            ;; really care about in the free var system..."
            (list #f)))

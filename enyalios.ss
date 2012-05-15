;; A simple compiler for a restricted subset of Digamma, similar in nature to Eprime, but
;; containing fixes for things I've learned since having started Eprime, as well as 
;; some simpler experiments. 

;; Two major things:

;; generate pretty C code, and avoid strings with C code in them 
;; (can be used for generating FFI code too!)
;; remove unecessary walks through functions; just generate code.
;; (meaning, no tail-call?, no hof?, &c.).

;; my idea wrt "just generate it" is to wrap functions in
;; while-blocks, and break on all expressions that would return.
;; Basic testing shows that gcc creates similar code for:

;; int
;; foo()
;; {
;;     while(1) {
;;         if(3 < 4)
;;             return 1;
;;         else
;;             return 0;
;;     }
;; }

;; and

;; int
;; foo()
;; {
;;     if(3 < 4)
;;         return 1;
;;     else
;;         return 0;
;; }

;; I still have to find a nice way of generating HOFs, but I don't
;; think that it will be too big a deal to turn:
;; (define (foo f x) ... (f x 1) ...)
;; into:
;; SExp *
;; foo(SExp *(*f)(SExp *, SExp *), SExp *x)
;; {
;;    ...
;; }

(define (generate-literal lit)
    (cond
        (void? lit) SVOID
        (eof-object? lit) SEOF
        (number? lit) #f
        (boolean? lit) (if lit STRUE SFALSE)
        (string? lit) #f
        (pair? lit) #f ;; used for '(pair)
        (vector? lit) #f
        (dict? lit) #f))

(define (generate-code c)
    #f)

(define (int->spaces lvl out)
    (if (< lvl 1)
        #v
        (begin
            (display "    " out)
            (int->spaces (- lvl 1) out))))

(define (il->c il lvl out)
    (cond
        (null? il) #v
        (void? il) (display "SVOID" out)
        (integer? il) (display (format "makeinteger(~n)" il) out)
        (real? il) (display (format "makereal(~a)" il) out)
        (rational? il) (display (format "makerational(~n,~n)" (numerator il) (denomenator il)) out)
        (complex? il) (display (format "makecomplex(~r,~r)" (real-part il) (imag-part il)) out)
        (string? il) (display (format "makestring(~s)" il) out)
        (char? il) (display (format "makechar(~c)" il) out)
        (symbol? il) (display il out)
        (pair? (car il))
            (foreach-proc (fn (x) (il->c x lvl out)) il)
        (eq? (car il) 'c-if)
            (begin
                (int->spaces lvl out)
                (display "if(" out)
                (il->c (cadr il) 0 out)
                (display "){\n" out)
                (il->c (cddr il) (+ lvl 1) out)
                (int->spaces lvl out)
                (display "}\n" out))
        (eq? (car il) 'c-elif)
            (begin
                (int->spaces lvl out)
                (display "else if(" out)
                (il->c (cadr il) 0 out)
                (display "){\n" out)
                (il->c (cddr il) (+ lvl 1) out)
                (int->spaces lvl out)
                (display "}\n" out))
        (eq? (car il) 'c-else)
            (begin
                (int->spaces lvl out)
                (display "else {\n" out)
                (il->c (cdr il) (+ lvl 1) out)
                (int->spaces lvl out)
                (display "}\n" out))
        (eq? (car il) 'c-return)
            (begin
                (int->spaces lvl out)
                (display "return " out)
                (il->c (cadr il) 0 out)
                (display ";\n" out))
        (eq? (car il) 'c-dec) ;; function declaration
            (begin
                (display "SExp *\n" out)
                (display (cadr il) out) ;; should be a call to MUNG here...
                (display "(" out)
                (display
                    (string-join
                        (map (fn (x) (format "SExp *~a" x))
                            (caddr il))
                        ", ")
                    out)
                (display "){\n" out)
                (foreach-proc (fn (x) (il->c x (+ lvl 1) out)) (cadddr il))
                (display "\n}\n" out))
        (eq? (car il) 'c-eq)
            (begin
                ;; this is a dummy for now; I would like to
                ;; eventually unpack eq?s like so:
                ;; (eq? x 3)
                ;; (TYPE(x) == NUMBER && NTYPE(x) == INTEGER && AINT(x) == 3? STRUE : SFALSE)
                (display "eqp(" out)
                (il->c (cadr il) 0 out)
                (display ", " out)
                (il->c (caddr il) 0 out)
                (display ")" out))
        (eq? (car il) 'c-primitive)
            (display "#primitive#" out)
        else (display "###" out)))

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

(define *procedures* {
    ;; negative numbers mean "N or less"
    :display ["fprinc" -2]
    :newline ["fnewline" -1]
})
(define *primitives* {
    :car ["car" #f 1]
    :cdr ["cdr" #f 1]
    :cons ["cons" #f 2]
    :eq? ["eqp" #f 2] 
    :< ["flt" #f 0]
    :> ["fgt" #f 0]
    :<= ["flte" #f 0]
    :>= ["fgte" #f 0]
    := ["fnumeq" #f 0]
    :+ ["fplus" #f 0]
    :* ["fmult" #f 0]
    :/ ["fdivi" #f 0]
    :- ["fsubt" #f 0]
    :numerator ["fnumerator" #f 1]
    :denomenator ["fdenomenator" #f 1]
})
(define *ulambdas* {})

(define (enyalios@primitive? o)
    (dict-has? *primitives* o))

(define (enyalios@procedure? o)
    (dict-has? *procedures* o))

(define (enyalios@ulambda? o)
    (dict-has? *ulambdas* o))

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

(define (compile-primitive block)
    (let ((prim (nth *primitives* (car block)))
          (args (cdr block)))
        (cond
            (= (nth prim 2) 0)
                (if (= (length args) 0)
                    (list 
                        (list 'c-primitive (nth prim 0) '()) #f)
                    (list
                        (list 'c-primitive (nth prim 0)
                            (map (fn (x) (car (generate-code x '() #f))) args)) #f))
            (= (nth prim 2) (length args))
                (list
                    (list 'c-primitive-fixed (nth prim 0)
                        (map (fn (x) (car (generate-code x '() #f))) args)) #f)
            else
                (error (format "incorrect arity for primitive ~a" (car block))))))

(define (compile-lambda block name tail?)
    #f)

(define (compile-procedure block name tail?)
    " compile a top-level procedure, as opposed to
      closure conversion of compile-lambda
      May need to switch away from using compile-begin,
      because procedures have some special cases, wrt
      lambda lifting (although, thinking about it, so do
      let blocks). Have to masticate on this more, but 
      this is a decent first start.
    "
    (let ((body (compile-begin (cdr block) name #t))
          (params (car block)))
        (if (cadr body) ;; body contains a tail-call
            (list 'c-dec name params
                (list 'c-loop (car body)))
            (list 'c-dec name params (car body)))))

(define (compile-if block name tail?)
    " compiles an if statement into IL.
      PARAMETERS:
      block : scheme code
      name : current function name, to support TCO

      RETURNS:
      (AST RECURSE?)
    "
    (let* ((<cond> (car (generate-code (car block) name #f)))
          (<then> (generate-code (cadr block) name tail?))
          (<else> (generate-code (caddr block) name tail?))
          (<result> (list
                        (list 'c-if <cond> (car <then>))
                        (list 'c-else (car <else>)))))
        (list <result> (and tail? (or (cadr <then>)
                            (cadr <else>))))))

(define (compile-cond block name tail?)
    #f)

(define (compile-begin block name tail?)
    (if tail?
        (if (= (length block) 1)
                (generate-code (car block) name #t)
            (let* ((b (map
                      (fn (x) (car (generate-code x '() #f)))
                      (cslice block 0 (- (length block 1)))))
                   (e (generate-code
                        (cslice block (- (length block) 1) (length block))
                        name
                        tail?)))
                (list
                    (append b (list (car e)))
                    (cadr e))))
        (list
            (map (fn (x) (car (generate-code x '() #f))) block)
            #f)))

(define (compile-let block name tail?)
    #f)

(define (generate-code c name tail?)
    " Generates IL code for a given
      preDigamma code. The tail? parameter
      is used in code that doesn't need to
      tail call optimize, like the <cond>
      portion of an if block, non-tail members
      of a begin, &c.
      @params:
        c : preDigamma code
        name : lambda name for generating tail calls
        tail? : boolean to determine whether or not to tail call optimize
      @returns:
        (PAIR-OF-ATOMS RECURSE?)
    "
    (cond
        (null? c) '()
        (or (vector? c)
            (dict? c)
            (goal? c)
            (bool? c)
            (char? c)
            (string? c)
            (number? c)
            (void? c)
            (symbol? c) ;; not exactly sure about this one...
            (eof-object? c)) 
                (if tail?
                    (list (list 'c-return c) #f)
                    (list c))
        (eq? (car c) 'if) (compile-if (cdr c) name tail?)
        (eq? (car c) 'quote)
            (if (null? (cadr c))
                '((c-nil) #f)
                (list (list 'c-quote (cdr c)) #f))
        (eq? (car c) 'cond) (compile-cond (cdr c) name tail?)
        (eq? (car c) 'define) 
            (cond
                (symbol? (cadr c))
                    (if (and
                            (pair? (caddr c))
                            (or
                                (eq? (car (caddr c)) 'lambda)
                                (eq? (car (caddr c)) 'fn)))
                        (compile-procedure (cdaddr c) (cadr c) #t)
                        (list 'c-dec-var (cadr c) (car (generate-code (caddr c) '() #f))))
                (pair? (cadr c))
                    (compile-procedure (cons (cdadr c) (cddr c)) (caadr c) #t)
                else (error "illegal define form; DEFINE (SYMBOL | PAIR) FORM*"))
        (eq? (car c) 'let) #t
        (eq? (car c) 'let*) #t
        (eq? (car c) 'letrec) #t
        (eq? (car c) 'with) #t ; transform this into let, run same code
        (eq? (car c) 'set!) #t
        (eq? (car c) 'begin) (compile-begin (cdr c) name tail?)
        (eq? (car c) name) ;; tail-call?
            (list
                (list
                    'c-tailcall
                    name
                    (map
                        (fn (x) (car (generate-code x '() #f)))
                        (cdr c)))
                #t)
        (enyalios@primitive? (car c)) (compile-primitive c name tail?) ; all other primitive forms
        (enyalios@procedure? (car c)) #t ; primitive procs, like display
        (enyalios@ulambda? (car c)) ; user-defined lambda?
            (list
                (list
                    'c-call
                    (car c)
                    (map
                        (fn (x) (car (generate-code x '() #f)))
                        (cdr c)))
                #t)
        else (error (format "unknown form: ~a" c))))

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
        (eof-object? il) (display "SEOF" out)
        (bool? il) (if il (display "STRUE" out) (display "SFALSE" out))
        (goal? il) (if il (display "SSUCC" out) (display "SUNSUCC" out))
        (integer? il) (display (format "makeinteger(~n)" il) out)
        (real? il) (display (format "makereal(~a)" il) out)
        (rational? il) (display (format "makerational(~n,~n)" (numerator il) (denomenator il)) out)
        (complex? il) (display (format "makecomplex(~r,~r)" (real-part il) (imag-part il)) out)
        (string? il) (display (format "makestring(~s)" il) out)
        (char? il) (display (format "makechar(~c)" il) out)
        (symbol? il) (display il out)
        (pair? (car il))
            (foreach-proc (fn (x) (il->c x lvl out)) il)
        (eq? (car il) 'c-nil)
            (display "SNIL" out)
        (eq? (car il) 'c-quote)
            #f
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
        (eq? (car il) 'c-loop)
            (begin
                (int->spaces lvl out)
                (display "while(1) {\n" out)
                (il->c (cadr il) (+ lvl 1) out)
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
                (display "}\n" out))
        (eq? (car il) 'c-docstring)
            (begin
                (int->spaces lvl out)
                (display "/* " out)
                (display (cadr il) out)
                (newline out)
                (int->spaces lvl out)
                (display " */\n" out))
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
            (begin
                (display (cadr il) out)
                (if (null? (caddr il))
                    (display "(SNIL)" out)
                    (begin
                        (display "list(" out)
                        (display (length (caddr il)) out)
                        (display ",")
                        (display (string-join
                            (map
                                (fn (x) (il->c x 0 out)) (caddr il))
                                ",")
                            out)
                        (display "))"))))
        (eq? (car il) 'c-primitive-fixed) ;; fixed arity primitive
            (begin
                (display (cadr il) out)
                (display "(" out)
                (display 
                    (string-join
                        (map
                            (fn (x) (il->c x 0 out)) (caddr il))
                        ",") out)
                (display ")" out))
        else
            (display "###" out)))

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
    :display ["f_princ" 1]
    :newline ["newline" 1]
    :read ["f_read" 0]
    :write ["f_write" 0]
    :format ["format" 0]
    :read-char ["fread_char" 0]
    :write-char ["fwrite_char" 0]
    :read-buffer ["fread_buffer" 0]
    :write-buffer #t
    :read-string #t
    :write-string #t
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
    :length ["flength" #f 1] 
    :exact? ["fexactp" #f 1]
    :inexact? ["finexactp" #f 1]
    :real? ["frealp" #f 1]
    :integer? ["fintegerp" #f 1]
    :complex? ["fcomplexp" #f 1]
    :rational? ["frationalp" #f 1]
    :rationalize ["frationalize" #f 2]
    :numerator ["fnum" #f 1]
    :denomenator ["fden" #f 1]
    ;;:type ["ftype"]
    :gcd ["fgcd" #f 2]
    :lcm ["flcm" #f 2]
    :ceil ["fceil" #f 1]
    :floor ["ffloor" #f 1]
    :truncate ["ftruncate" #f 1]
    :round ["fround" #f 1]
    :inexact->exact ["fin2ex" #f 1]
    :quotient ["fquotient" #f 2]
    :modulo ["fmodulo" #f 2]
    :remainder ["fremainder" #f 2]
    :& ["fbitand" #f 2]
    :| ["fbitor" #f 2]
    :^ ["fbitxor" #f 2]
    :~ ["fbitnot" #f 2]
    :<< ["fbitshl" #f 2]
    :>> ["fbitshr" #f 2]
    :make-vector ["fmkvector" #f 0]
    :make-string ["fmakestring" #f 0]
    :append ["fappend" #f 0]
    :first ["ffirst" #f 1]
    :rest ["frest" #f 1]
    :ccons ["fccons" #f 2] 
    :nth ["fnth" #f 0]
    :keys ["fkeys" #f 1] 
    :partial-key? ["fpartial_key" #f 2]
    :cset! ["fcset" #f 2]
    :string ["fstring" #f 0]
    :empty? ["fempty" #f 1]
    :gensym ["gensym" 0] 
    :imag-part ["fimag_part" #f 1]
    :real-part ["freal_part" #f 1] 
    :make-rectangular ["fmake_rect" #f 2]
    :make-polar ["fmake_pole" #f 2]
    :magnitude ["fmag" #f 1]
    :argument ["farg" #f 1]
    :conjugate! ["fconjugate_bang" #f 1] 
    :conjugate ["fconjugate" #f 1]
    :polar->rectangular ["fpol2rect" #f 1]
    :rectangular->polar ["frect2pol" #f 1]
    :sin ["fsin" #f 1]
    :cos ["fcos" #f 1]
    :tan ["ftan" #f 1]
    :asin ["fasin" #f 1]
    :acos ["facos" #f 1]
    :atan ["fatan" #f 1]
    :atan2 ["fatan2" #f 2]
    :cosh ["fcosh" #f 1]
    :sinh ["fsinh" #f 1]
    :tanh ["ftanh" #f 1]
    :exp ["fexp" #f 1]
    :ln ["fln" #f 1]
    :abs ["fnabs" #f 1]
    :sqrt ["fsqrt" #f 1]
    :exp2 ["fexp2" #f 1]
    :expm1 ["fexpm1" #f 1]
    :log2 ["flog2" #f 1]
    :log10 ["flog10" #f 1]
    :string-append ["fstringappend" #f 0]
    ;:apply #t
    :assq ["assq" #f 2]
    :memq ["memq" #f 2]
    ;:defrec #t
    ;:set-rec! #t
    :dict ["fdict" #f 0]
    ;:make-dict #t
    :dict-has? ["fdicthas" #f 2]
    :coerce ["fcoerce" #f 2]
    :error ["ferror" #f 1]
    :cupdate ["fcupdate" #f 3]
    :cslice ["fcslice" #f 3]
    ;:tconc! ["tconc"]
    ;;:make-tconc #t
    ;;:tconc-list #t
    ;;:tconc->pair #t
    ;;:tconc-splice! ["tconc_splice"]
    :eval ["__seval" #t 0]
})

(define *ulambdas* {})

(define (show x)
    (display "x: ")
    (write x)
    (newline)
    x)

(define (enyalios@primitive? o)
    (dict-has? *primitives* o))

(define (enyalios@procedure? o)
    (dict-has? *procedures* o))

(define (enyalios@ulambda? o)
    (dict-has? *ulambdas* o))

(define (count-arities p req opt)
    (cond
        (null? p) (list req opt)
        (symbol? (car p)) (count-arities (cdr p) (+ 1 req) opt)
        (pair? (car p)) (count-arities (cdr p) req (+ 1 opt))))

(define (shadow-params params d)
    (if (null? params)
        d
        (if (pair? (car params))
            (shadow-params
                (cdr params)
                (append
                    d
                    (list (gensym (caar params)))))
            (shadow-params
                (cdr params)
                (append
                    d
                    (list (gensym (car params))))))))

(define (set-arity! name params)
    (let ((arities (count-arities params 0 0)))
        (cset! *ulambdas* name
            (vector
                name
                params
                (car arities)
                (cadr arities)
                (shadow-params params '())))))

(define (compile-primitive block name tail? rewrites)
    (let ((prim (nth *primitives* (car block)))
          (args (cdr block)))
        (cond
            (= (nth prim 2) 0)
                (if (= (length args) 0)
                    (list 
                        #f
                        (list 'c-primitive (nth prim 0) '()))
                    (list
                        #f
                        (list 'c-primitive (nth prim 0)
                            (map (fn (x) (cadr (generate-code x '() #f rewrites))) args))))
            (= (nth prim 2) (length args))
                (list
                    #f
                    (list 'c-primitive-fixed (nth prim 0)
                        (map (fn (x) (cadr (generate-code x '() #f rewrites))) args)))
            else
                (error (format "incorrect arity for primitive ~a" (car block))))))

(define (compile-primitive-procedure block name tail? rewrites)
    (let ((proc (nth *procedures* (car block)))
          (args (cdr block)))
        (if (= (nth proc 1) (length args))
            (list
                #f
                (list 'c-procedure (nth proc 0)
                    (map (fn (x) (cadr (generate-code x '() #f rewrites))) args)))
            (error (format "incorrect arity for primitive-procedure ~a" (car block))))))

(define (compile-lambda block name tail? rewrites)
    #f)

(define (compile-procedure block name tail? rewrites)
    " compile a top-level procedure, as opposed to
      closure conversion of compile-lambda
      May need to switch away from using compile-begin,
      because procedures have some special cases, wrt
      lambda lifting (although, thinking about it, so do
      let blocks). Have to masticate on this more, but 
      this is a decent first start.
    "
    (let ((body (compile-begin (cdr block) name #t rewrites))
          (params (car block)))
        (if (car body) ;; body contains a tail-call
            (list 'c-dec name params
                (list 'c-begin
                    (list 'c-shadow-params name)
                    (list 'c-loop (cadr body))))
            (list 'c-dec name params (cadr body)))))

(define (compile-if block name tail? rewrites)
    " compiles an if statement into IL.
      PARAMETERS:
      block : scheme code
      name : current function name, to support TCO

      RETURNS:
      (RECURSE? AST+)
    "
    (let* ((<cond> (cadr (generate-code (car block) name #f rewrites)))
          (<then> (generate-code (cadr block) name tail? rewrites))
          (<else> (generate-code (caddr block) name tail? rewrites)))
        ;; need to check tail? here, and, if it is true,
        ;; add 'c-returns to each of (<then> <else>)
        (list (and tail? (or (car <then>) (car <else>)))
                    (list 'c-if <cond> (returnable (cdr <then>) tail?))
                    (list 'c-else (returnable (cdr <else>) tail?)))))

(define (il-syntax? c)
    (cond
        (not (pair? c))
            #f
        (pair? (car c)) 
            (il-syntax? (car c))
        (or
            (eq? (car c) 'c-if)
            (eq? (car c) 'c-elif)
            (eq? (car c) 'c-else)
            (eq? (car c) 'c-begin)
            (eq? (car c) 'c-set!)
            (eq? (car c) 'c-return)
            (eq? (car c) 'c-while)
            (eq? (car c) 'c-for)
            (eq? (car c) 'c-shadow-params)
            (eq? (car c) 'c-loop))
            #t
        else
            #f))

(define (returnable c tail?)
    (if (and tail?
            (not (il-syntax? c)))
        (list 'c-return c)
        c))

(define (compile-begin block name tail? rewrites)
    (if tail?
        (if (= (length block) 1)
            (let ((x (generate-code (car block) name #t rewrites)))
                (list (car x)
                    (list 'c-begin (returnable (cdr x) tail?))))
            (let* ((b (map
                      (fn (x) (cdr (generate-code x '() #f rewrites)))
                      (cslice block 0 (- (length block) 1))))
                   (e (generate-code
                        (car (cslice block (- (length block) 1) (length block)))
                        name
                        tail?
                        rewrites)))
                (list
                    (car e)
                    (cons 'c-begin
                        (append b (list (returnable (cadr e) tail?)))))))
        (list
            #f
            (cons 'c-begin
                (map (fn (x) (cadr (generate-code x '() #f rewrites))) block)))))

(define (generate-let-temps names d)
    " generates temporary names for let
      variables:
      (let ((x 10) (y 30) (z 20))
        (+ x y z))

      SExp *tmp0 = 10, *tmp1 = 30, *tmp2 = 20;
      return fplus(list(3,tmp0,tmp1,tmp2));
    "
    (if (null? names)
        d
        (begin
            (cset! d (car names) (gensym 'tmp))
            (generate-let-temps (cdr names) d))))

(define (dict-copy k dict new-dict)
    "shallow copy a dictionary"
    (if (null? k)
        new-dict
        (begin
            (cset! new-dict (car k) (nth dict (car k)))
            (dict-copy (cdr k) dict new-dict))))

(define (compile-let block name tail? rewrites)
    " uses dict-copy to merge rewrites into the new
      generated list of let temporaries; new let bindings
      shadow their higher-level counterparts
    "
    (let* ((vals (unzip (car block)))
           (vars (car vals))
           (data (cadr vals))
           (var-temps (generate-let-temps
                        vars
                        (dict-copy
                            (keys rewrites)
                            rewrites {})))
           (body (cdadr (compile-begin (cdr block) name tail? var-temps))))
        (cons
            'c-begin
            (append
                (map 
                    (fn (x) (list 'c-var
                                (nth var-temps (car x))
                                (cadr (generate-code (cadr x) name #f rewrites))))
                    (car block)) 
                body))))

(define (compile-let* block name tail? rewrites)
    #f)

(define (generate-code c name tail? rewrites)
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
            (eof-object? c)) 
                (if tail?
                    (list #f (list 'c-return c))
                    (list #f c))
        (symbol? c)
            (if (and (dict? rewrites) (dict-has? rewrites c))
                (if tail?
                    (list #f (list 'c-return (nth rewrites c)))
                    (list #f (nth rewrites c)))
                (if tail?
                    (list #f (list 'c-return c))
                    (list #f c)))
        (eq? (car c) 'if) (compile-if (cdr c) name tail? rewrites)
        (eq? (car c) 'quote)
            (if (null? (cadr c))
                '(#f (c-nil))
                (list #f (list 'c-quote (cdr c))))
        (eq? (car c) 'define) 
            (cond
                (symbol? (cadr c))
                    (if (and
                            (pair? (caddr c))
                            (or
                                (eq? (car (caddr c)) 'lambda)
                                (eq? (car (caddr c)) 'fn)))
                        (compile-procedure (cdaddr c) (cadr c) #t rewrites)
                        (list #f (list 'c-var (cadr c) (car (generate-code (caddr c) '() #f rewrites)))))
                (pair? (cadr c))
                    (compile-procedure (cons (cdadr c) (cddr c)) (caadr c) #t rewrites)
                else (error "illegal define form; DEFINE (SYMBOL | PAIR) FORM*"))
        (eq? (car c) 'let) (compile-let (cdr c) name tail? rewrites)
        (eq? (car c) 'let*) (compile-let (cdr c) name tail? rewrites) ;; no difference in PreF
        (eq? (car c) 'letrec) #t
        ; transform this into let, run same code
        (eq? (car c) 'with) 
            (compile-let
                (cons
                    (list (list (cadr c) (caddr c)))
                    (cdddr c)
                    rewrites)
                name
                tail?
                rewrites)
        (eq? (car c) 'set!) #t
        (eq? (car c) 'begin) (compile-begin (cdr c) name tail? rewrites)
        (eq? (car c) name) ;; tail-call?
            (list
                #t
                (list
                    'c-tailcall
                    name
                    (map
                        (fn (x) (cadr (generate-code x '() #f rewrites)))
                        (cdr c))))
        (enyalios@primitive? (car c)) (compile-primitive c name tail? rewrites) ; all other primitive forms
        (enyalios@procedure? (car c)) (compile-primitive-procedure c name tail? rewrites) ; primitive procs, like display
        (enyalios@ulambda? (car c)) ; user-defined lambda?
            (list
                #f
                (list
                    'c-call
                    (car c)
                    (map
                        (fn (x) (cadr (generate-code x '() #f rewrites)))
                        (cdr c))))
        else (error (format "unknown form: ~a" c))))

(define (int->spaces lvl out)
    (if (< lvl 1)
        #v
        (begin
            (display "    " out)
            (int->spaces (- lvl 1) out))))

(define (comma-separated-c ill out)
    (cond
        (null? ill) #v
        (null? (cdr ill))
            (il->c (car ill) 0 out)
        else
            (begin
                (il->c (car ill) 0 out)
                (display ", " out)
                (comma-separated-c (cdr ill) out))))

(define (il->c il lvl out)
    (cond
        (null? il) #v
        (void? il) (display "SVOID" out)
        (eof-object? il) (display "SEOF" out)
        (bool? il) (if il (display "STRUE" out) (display "SFALSE" out))
        (goal? il) (if il (display "SSUCC" out) (display "SUNSUCC" out))
        (integer? il) (display (format "makeinteger(~n)" il) out)
        (real? il) (display (format "makereal(~a)" il) out)
        (rational? il)
            (display
                (format "makerational(~n,~n)"
                    (numerator il)
                    (denomenator il))
                out)
        (complex? il)
            (display
                (format "makecomplex(~r,~r)"
                    (real-part il)
                    (imag-part il))
                out)
        (string? il)
            (begin
                (display "makestring(" out)
                (write il out)
                (display ")" out))
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
                (int->spaces lvl out)
                (display "}\n" out))
        (eq? (car il) 'c-return)
            (begin
                (int->spaces lvl out)
                (display "return " out)
                (il->c (cadr il) 0 out)
                (display ";\n" out))
        (eq? (car il) 'c-var) ;; variable declaration
            (begin
                (int->spaces lvl out)
                (display "SExp *" out)
                (display (cadr il) out)
                (display " = " out)
                (il->c (cadr il))
                (display ";\n" out))
        (eq? (car il) 'c-dec) ;; function declaration
            (begin
                (display "SExp *\n" out)
                (display (cadr il) out) ;; should be a call to MUNG here...
                (display "(" out)
                (if (null? (caddr il))
                    #v
                    (display
                        (string-join
                            (map (fn (x) (format "SExp *~a" x))
                                (caddr il))
                            ", ")
                        out))
                (display "){\n" out)
                (il->c (cadddr il) (+ lvl 1) out)
                (display "}\n" out))
        (eq? (car il) 'c-shadow-params)
            (let ((proc-data (nth *ulambdas* (cadr il))))
                (foreach-proc
                    (fn (x)
                        (int->spaces lvl out)
                        (format "SExp ~a = SNIL;~%" x))
                    (nth proc-data 4)))
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
                        (display "(list(" out)
                        (display (length (caddr il)) out)
                        (display ", " out)
                        (comma-separated-c (caddr il) out)
                        (display "))" out))))
        (eq? (car il) 'c-primitive-fixed) ;; fixed arity primitive
            (begin
                (display (cadr il) out)
                (display "(" out)
                (comma-separated-c (caddr il) out)
                (display ")" out))
        (eq? (car il) 'c-begin)
            (foreach-proc
                (fn (x)
                    (if (il-syntax? x)
                        (il->c x lvl out)
                        (begin
                            (int->spaces lvl out) 
                            (il->c x lvl out)
                            (display ";\n" out))))
                (cdr il))
        (eq? (car il) 'c-tailcall)
            (let ((proc-data (nth *ulambdas* (cadr il))))
                (if (< (length (caddr il)) (nth proc-data 2))
                    (error "Incorrect arity for user-defined lambda")
                    (begin
                        ;; set shadow params to value of each
                        ;; parameters:
                        ;; x1 = (+ x 1)
                        ;; y2 = (* y 3)
                        (foreach-proc
                            (fn (x)
                                (int->spaces lvl out)
                                (display
                                    (format "~a = ~a;~%"
                                        (car x)
                                        (cadr x))
                                    out))
                            (zip 
                                (nth proc-data 4) ;; shadow params
                                (caddr il)))
                        ;; set each parameter to the shadow's value:
                        ;; x = x1
                        ;; y = y2
                        (foreach-proc
                            (fn (x)
                                (int->spaces lvl out)
                                (display
                                    (format "~a = ~a; ~%" 
                                        (car x)
                                        (cadr x))
                                    out))
                            (zip
                                (nth proc-data 1)
                                (nth proc-data 4))))))
        (eq? (car il) 'c-call)
            (let ((proc-data (nth *ulambdas* (cadr il))))
                (if (< (length (caddr il)) (nth proc-data 2))
                    (error "Incorrect arity for user-defined lambda")
                    (begin
                        (display (cadr il) out)
                        (display "(" out)
                        (comma-separated-c (caddr il) out)
                        (display ")" out))))
        (eq? (car il) 'c-procedure)
            (begin
                (display (cadr il) out)
                (display "(" out)
                (comma-separated-c (caddr il) out)
                (display ")" out))
        else
            (display "###" out)))

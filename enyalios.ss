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

;; More cond/if and/or/not related notes:
;; * if Cond used a nested approach, an item that needed vars could easily be supported
;; - support both: if no and/or/not is detected, use linear cond expansion; if and/or/not is detected, use nested cond expansion 
;; - make if support vars first
;; - SSA?
;; * Fix apply:
;; ** don't use list with apply
;; - (+ x y) -> fplus(list(2,x,y));
;; - (apply + (some-fn)) -> fplus(some_fn());

(load "experiments/sr.ss")

(define *procedures* {
    ;; [ c-proc min max ]
    ;; if max != min && max == 0:
    ;;    1 .. N args are accepted
    ;; should write these to a list. The proc
    ;; handles unpacking.
    :display ["f_princ" 1 2]
    :newline ["newline" 0 1]
    :read ["f_read" 0 1]
    :write ["f_write" 1 2]
    :format ["format" 1 0]
    :read-char ["fread_char" 0 1]
    :write-char ["fwrite_char" 0 1]
    :read-buffer ["fread_buffer" 1 2]
    :quit ["fquit" 0 0]
    :write-buffer ["fwrite_buffer" 1 2]
    :read-string #t
    :write-string #t
    :eval ["__seval" 1 1]
    :sys/getpid ["f_sysgetpid" 0 1]
    :sys/getppid ["f_sysgetppid" 0 1]
    :sys/fork ["f_fork" 0 1]
    :sys/time ["f_time" 0 1]
    :announce ["f_announce" 3 0]
    :accept ["f_accept" 1 0]
    :listen ["f_listen" 1 0]
    :hangup ["f_hangup" 1 0]
    :ls ["f_ls" 1 0]
    :close ["f_close" 1 0]
    :open ["f_open" 2 0]
})

(define *varprimitives* {
    :list "list"
    :vector "vector"
    ;:dict "dict"
    ;:string "string"
})

(define *primitives* {
    :car ["car" #f 1 1]
    :cdr ["cdr" #f 1 1]
    :cons ["cons" #f 2 2]
    :eq? ["eqp" #f 2 2] 
    :< ["flt" #f 1 -1]
    :> ["fgt" #f 1 -1]
    :<= ["flte" #f 1 -1]
    :>= ["fgte" #f 1 -1]
    := ["fnumeq" #f 1 -1]
    :+ ["fplus" #f 1 -1]
    :* ["fmult" #f 1 -1]
    :/ ["fdivd" #f 1 -1]
    :- ["fsubt" #f 1 -1]
    :numerator ["fnumerator" #f 1 1]
    :denomenator ["fdenomenator" #f 1 1]
    :length ["flength" #f 1 1] 
    :exact? ["fexactp" #f 1 1]
    :inexact? ["finexactp" #f 1 1]
    :real? ["frealp" #f 1 1]
    :integer? ["fintegerp" #f 1 1]
    :complex? ["fcomplexp" #f 1 1]
    :rational? ["frationalp" #f 1 1]
    :rationalize ["frationalize" #f 2 2]
    :numerator ["fnum" #f 1 1]
    :denomenator ["fden" #f 1 1]
    :type ["ftype" #f 1 1]
    :gcd ["fgcd" #f 1 -1]
    :lcm ["flcm" #f 1 -1]
    :ceil ["fceil" #f 1 1]
    :floor ["ffloor" #f 1 1]
    :truncate ["ftrunc" #f 1 1]
    :round ["fround" #f 1 1]
    :inexact->exact ["fin2ex" #f 1 1]
    :quotient ["fquotient" #f 2 2]
    :modulo ["fmodulo" #f 2 2]
    :remainder ["fremainder" #f 2 2]
    :& ["fbitand" #f 2 2]
    :| ["fbitor" #f 2 2]
    :^ ["fbitxor" #f 2 2]
    :~ ["fbitnot" #f 1 1]
    :<< ["fbitshl" #f 2 2]
    :>> ["fbitshr" #f 2 2]
    :make-vector ["fmkvector" #f 1 -1]
    :make-string ["fmakestring" #f 1 -1]
    :type ["ftype" #f 1 1]
    :append ["fappend" #f 2 -1]
    :first ["ffirst" #f 1 1]
    :rest ["frest" #f 1 1]
    :ccons ["fccons" #f 2 2] 
    :nth ["fnth" #f 2 3]
    :keys ["fkeys" #f 1 1] 
    :partial-key? ["fpartial_key" #f 2 2]
    :cset! ["fcset" #f 3 3]
    :dict-set! ["fdset" #f 3 3] ;; to be optimized away below
    :vector-set! ["fvset" #f 3 3] ;; to be optimized away below
    :vector-ref ["fvref" #f 2 2] ;; to be optimized away below
    :type? ["typep" #f 2 2] ;; to be optimized away below
    :string ["fstring" #f 0 -1]
    :dict ["fdict" #f 0 -1]
    :empty? ["fempty" #f 1 1]
    :gensym ["fgensym" #f 0 1] 
    :imag-part ["fimag_part" #f 1 1]
    :real-part ["freal_part" #f 1 1] 
    :make-rectangular ["fmake_rect" #f 2 2]
    :make-polar ["fmake_pole" #f 2 2]
    :magnitude ["fmag" #f 1 1]
    :argument ["fimag_part" #f 1 1]
    :conjugate! ["fconjugate_bang" #f 1 1] 
    :conjugate ["fconjugate" #f 1 1]
    :polar->rectangular ["fpol2rect" #f 1 1]
    :rectangular->polar ["frect2pol" #f 1 1]
    :sin ["fsin" #f 1 1]
    :cos ["fcos" #f 1 1]
    :tan ["ftan" #f 1 1]
    :asin ["fasin" #f 1 1]
    :acos ["facos" #f 1 1]
    :atan ["fatan" #f 1 1]
    :atan2 ["fatan2" #f 2 2]
    :cosh ["fcosh" #f 1 1]
    :sinh ["fsinh" #f 1 1]
    :tanh ["ftanh" #f 1 1]
    :exp ["fexp" #f 1 1]
    :ln ["fln" #f 1 1]
    :abs ["fnabs" #f 1 1]
    :sqrt ["fsqrt" #f 1 1]
    :exp2 ["fexp2" #f 1 1]
    :expm1 ["fexpm1" #f 1 1]
    :log2 ["flog2" #f 1 1]
    :log10 ["flog10" #f 1 1]
    :string-append ["fstringappend" #f 0 -1]
    ;:apply #t
    :assq ["assq" #f 2 2]
    :memq ["memq" #f 2 2]
    ;:defrec #t
    ;:set-rec! #t
    ;:dict ["fdict" #f 0 -1]
    :make-dict ["makedict" #f 0 0]
    :dict-has? ["fdicthas" #f 2 2]
    :coerce ["fcoerce" #f 2 2]
    :error ["f_error" #f 1 1]
    :cupdate ["fcupdate" #f 3 3]
    :cslice ["fcslice" #f 3 3]
    :not ["fnot" #f 1 1] ;; to be optimized away
    ;:tconc! ["tconc"]
    ;;:make-tconc #t
    ;;:tconc-list #t
    ;;:tconc->pair #t
    ;;:tconc-splice! ["tconc_splice"]
})

(define *profiling* #f)
(define *ulambdas* {})
(define *usyntax* {})

;; Out-of-Bounds lambdas
;; these are lambdas that are defined
;; somewhere in the code, and need to be lifted.
(define *ooblambdas* '())

;; Includes; these are handled by their respective IL systems
;; Not checked in any way shape or form; SRFI-0 would help
;; here. A user can use the config language to select which
;; includes they want to use, based on the output IL
(define *includes* '())
(define *debug* #f)

(define (show x (prefix "x: "))
    (if *debug*
        (begin
            (display prefix)
        (write x)
        (newline))
        #v)
    x)

(define (enyalios@primitive? o)
    (dict-has? *primitives* o))

(define (enyalios@procedure? o)
    (dict-has? *procedures* o))

(define (enyalios@ulambda? o)
    (dict-has? *ulambdas* o))

(define (enyalios@usyntax? o)
    (dict-has? *usyntax* o))

(define (enyalios@var-prim? o)
    (or
        (eq? o 'list)
        (eq? o 'vector)
        (eq? o 'dict)
        (eq? o 'string)))

(define (enyalios@parameter-call? o lparams)
    (not (eq? (memq o (nth lparams "parameters" '())) #f)))

(define (member*? x l)    
    (cond
        (null? l) #f
        (pair? (car l)) (or (member*? x (car l)) (member*? x (cdr l)))
        (eq? (car l) x) #t
        else (member*? x (cdr l))))

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
                (shadow-params params '())
                '()))))

(define (fixed-helper args idx max-len rewrites lparams)
    (cond
        (>= idx max-len)
        '()
        (null? args)
        (cons '(c-nil) (fixed-helper args (+ idx 1) max-len rewrites lparams))
        else
        (cons
            (cadr (generate-code (car args) '() #f rewrites lparams))
            (fixed-helper (cdr args) (+ idx 1) max-len rewrites lparams))))

(define (compile-primitive block name tail? rewrites lparams)
    (let ((prim (nth *primitives* (car block)))
          (args (cdr block)))
        ;(display "compile-primitive ")
        ;(display (car block))
        ;(display " ; len(args) == ")
        ;(display (length args))
        ;(display "and primitive lens == ")
        ;(display (format "~a, ~a~%" (nth prim 2) (nth prim 3)))
        (cond
            (and
                (= (nth prim 2) 0)
                (= (nth prim 3) 0)
                (= (length args) 0))
                (list 
                    #f
                    (list 'c-primitive-null (nth prim 0)))
            (and
                (> (nth prim 2) 0)
                (>= (length args) (nth prim 2))
                (= (nth prim 3) -1))
                (list
                    #f
                    (list 'c-primitive (nth prim 0)
                        (map (fn (x) (cadr (generate-code x '() #f rewrites lparams))) args)))
            (and
                (>= (length args) (nth prim 2))
                (<= (length args) (nth prim 3)))
                (list
                    #f
                    (list 'c-primitive-fixed (nth prim 0)
                        (fixed-helper args 0 (nth prim 3) rewrites lparams)))
            else
                (error (format "incorrect arity for primitive ~a" (car block))))))

(define (compile-primitive-procedure block name tail? rewrites lparams)
    (let ((proc (nth *procedures* (car block)))
          (args (cdr block))
          (env  (nth lparams "env" "tlenv")))
        ;(display "keys in lparams: ")
        ;(write (keys lparams))
        ;(newline)
        (cond
            (and
                (= (nth proc 1) 0)
                (= (nth proc 2) 0)
                (= (length args) 0))
                (list
                    #f
                    (list 'c-procedure (nth proc 0) '(c-nil) env))
            (and
                (> (nth proc 1) 0)
                (>= (length args) (nth proc 1))
                (= (nth proc 2) 0))
                (list
                    #f
                    (list 'c-procedure (nth proc 0)
                        (map (fn (x) (cadr (generate-code x '() #f rewrites lparams))) args)
                        env))
            (and
                (>= (length args) (nth proc 1))
                (<= (length args) (nth proc 2)))
                (list
                    #f
                    (list 'c-procedure (nth proc 0)
                        (map (fn (x) (cadr (generate-code x '() #f rewrites lparams))) args)
                        env))
            else (error (format "incorrect arity for primitive-procedure ~a" (car block))))))

(define (compile-variable-primitive block name tail? rewrites lparams)
    (let ((hd (car block))
          (args (cdr block)))
        (list
            #f
            (list 'c-variable-primitive hd
                (map (fn (x) (cadr (generate-code  x '() #f rewrites lparams))) args)))))

(define (compile-lambda block name tail? rewrites lparams)
    (let* ((params (car block))
           (name (gensym 'fun_))
           (nulparams (dict "params" params "name" name "env" (nth lparams "env" "tlenv"))) ;; should probably merge dicts here...
           (body (compile-begin (cdr block) name #t rewrites nulparams)))
        (set! *ooblambdas*
            (cons 
                (list 'c-dec name params (cadr body))
                *ooblambdas*))
        (set-arity! name params)
        (if tail?
            (list #f (list 'c-function-reference name (length params)))
            (list #f name))))

(define (merge-parameters lparams params)
    (with ret (dict-copy (keys lparams) lparams (dict))
        (cset! ret "parameters"
            (append params (nth lparams "parameters" '())))
        ret))
                
(define (compile-procedure block name tail? rewrites lparams)
    " compile a top-level procedure, as opposed to
      closure conversion of compile-lambda
      May need to switch away from using compile-begin,
      because procedures have some special cases, wrt
      lambda lifting (although, thinking about it, so do
      let blocks). Have to masticate on this more, but 
      this is a decent first start.

      - The above actually looks pretty good after testing. One
        other thing now though is if function params that are them
        selves functions (HOF) should be analyzed here or in the 
        code generator. If it was handled here in some way, it would
        make life a bit easier in the Code generator...
    "
    ;(display (format "in compile-procedure; name == ~a, block == \n" name))
    ;(write block)
    ;(newline)
    (let* ((params (car block))
          (nulparams (merge-parameters lparams params)))
        (cset! nulparams "parameters" (append params (nth lparams "parameters" '())))
        (cset! nulparams "name" name)
        ;(display "block.rest == ")
        ;(write (cdr block))
        ;(newline)
        (let ((body (compile-begin (cdr block) name #t rewrites nulparams))) 
            ;(display "body == ")
            ;(write body)
            ;(newline)
            (if (car body) ;; body contains a tail-call
                (list 
                    #f
                    (list 'c-dec name params
                        (list 'c-begin
                            (list 'c-shadow-params name)
                            (list 'c-loop (cadr body)))))
                (list
                    #f
                    (list 'c-dec name params (cadr body)))))))

(define (compile-if block name tail? rewrites lparams)
    " compiles an if statement into IL.
      PARAMETERS:
      block : scheme code
      name : current function name, to support TCO

      RETURNS:
      (RECURSE? AST+)
    "
    (let* ((<cond> (cadr (generate-code (car block) name #f rewrites lparams)))
          (<then> (generate-code (cadr block) name tail? rewrites lparams))
          (<else> (generate-code (caddr block) name tail? rewrites lparams))
          (<tail-check> (and tail? (or (car <then>) (car <else>)))))
        ;; need to check tail? here, and, if it is true,
        ;; add 'c-returns to each of (<then> <else>)
        ;; this check for #v should be extended to all non-function calls...
        ;; all literals?
        ;(display "\n<then> == ")
        ;(write <then>)
        ;(newline)
        ;(display "\n<else> == ")
        ;(write <else>)
        ;(newline)
        (if (eq? (cadr <else>) #v)
            (list
                <tail-check>
                (list 'c-if <cond> (returnable (cadr <then>) tail? #t)))
            (list 
                <tail-check>
                (list
                    'c-begin
                        (list 'c-if <cond> (returnable (cadr <then>) tail? #t))
                        (list 'c-else (returnable (cadr <else>) tail? #t)))))))

(define (cond-unzip block conds thens)
    (cond
        (null? block) (list (tconc->pair conds) (tconc->pair thens))
        (null? (cdr block)) (error "incorrectly formated COND/CASE block")
        else
            (begin
                (tconc! conds (car block))
                (tconc! thens (cadr block))
                (cond-unzip (cddr block) conds thens))))

(define (compile-cond block name tail? rewrites lparams)
    " compiles a cond statement into IL.
      PARAMETERS:
      block: scheme code
      name : current function name in TCO
      tail? : boolean for tail calls
      rewrites : any let renames.
      lparams : dict containing function information (used outside of compile-cond)

      RETURNS : 
      (RECURSE? AST+)
    "
    ;; this is pretty low-level; should clean this up a bit.
    ;; especially the cond-list/then-list stuff. Actually, the 
    ;; whole thing. Clean it up.
    ;; possiblity: pass (cddr block) into a "helper" lambda that
    ;; does the jobs of the below with less mess?
    ;; save on interation too; a helper could iterate through once,
    ;; whereas here we're iterating through several times...
    (let* ((seps (cond-unzip block (make-tconc '()) (make-tconc '())))
           (init-cond (generate-code (caar seps) name #f rewrites lparams))
           (init-then (generate-code (caadr seps) name tail? rewrites lparams))
           (cond-list (cdar seps))
           (then-list (cdadr seps))
           (tail-rec? #f))
       (if (car init-then)
            (set! tail-rec? #t)
            #v)
       (set! cond-list
            (map
                (fn (x)
                    (if (eq? x 'else)
                        x
                        (cadr (generate-code x name #f rewrites lparams))))
                cond-list))
        (set! then-list
            (map
                (fn (x)
                    (with res (generate-code x name tail? rewrites lparams)
                        (if (car res)
                            (set! tail-rec? #t)
                            #v)
                        (returnable (cadr res) tail? #t)))
                then-list))
        (list
            tail-rec?
            (list
                'c-begin
                (list
                    (cons 
                        (cons 'c-if (list (cadr init-cond) (returnable (cadr init-then) tail? #t)))
                        (map
                            (fn (x1)
                                (if (eq? (car x1) 'else)
                                    (list 'c-else (cadr x1))
                                    (cons 'c-elif x1 )))
                            (zip cond-list then-list))))))))
                    
(define (compile-case block name tail? rewrites lparams)
    " compiles a CASE form into IL.
      PARAMETERS:
      block: scheme code
      name : current function name in TCO
      tail? : boolean for tail calls
      rewrites : any let renames.
      lparams : dict containing function information (used outside of compile-cond)

      RETURNS : 
      (RECURSE? AST+)
    "
    (let* ((seps (cond-unzip (cdr block) (make-tconc '()) (make-tconc '())))
           (cond-list (car seps))
           (then-list (cadr seps))
           (tail-rec? #f))
        (set! then-list
            (map
                (fn (x)
                    (with res (generate-code x name tail? rewrites lparams)
                        (if (car res)
                            (set! tail-rec? #t)
                            #v)
                        (returnable (cadr res) tail?)))
                then-list))
        (list
            tail-rec?
            (cons
                'c-case
                    (cons
                        (cadr (generate-code (car block) name #f rewrites lparams))
                        (zip cond-list then-list))))))

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
            (eq? (car c) 'c-case)
            (eq? (car c) 'c-begin)
            (eq? (car c) 'c-set!)
            (eq? (car c) 'c-return)
            (eq? (car c) 'c-while)
            (eq? (car c) 'c-for)
            (eq? (car c) 'c-shadow-params)
            (eq? (car c) 'c-var)
            (eq? (car c) 'c-dec)
            (eq? (car c) 'c-tailcall)
            (eq? (car c) 'c-docstring)
            (eq? (car c) 'c-%prim)
            (eq? (car c) 'c-loop))
            #t
        else
            #f))

;; perhaps this should return a var assignment here...
;; ActionScript does something like that; it creates a lexically-scoped
;; dummy var for such purposes. Could be a per-scope gensym'd var...
(define (returnable c tail? (last? #f))
    (cond
        (and tail? (not (il-syntax? c))) (list 'c-return c)
        (and last? (not (il-syntax? c))) (list 'c-no-return c)
        else c))

(define (compile-begin block name tail? rewrites lparams)
    (if tail?
        (if (= (length block) 1)
            (let ((x (generate-code (car block) name #t rewrites lparams)))
                (list (car x)
                    (list 'c-begin (returnable (cdr x) tail?))))
            (let ((b (map
                      (fn (x)
                        (if (string? x) ;; just a random string?
                            (list 'c-docstring x)
                            (cadr (generate-code x '() #f rewrites lparams))))
                      (cslice block 0 (- (length block) 1))))
                   (e (generate-code
                        (car (cslice block (- (length block) 1) (length block)))
                        name
                        tail?
                        rewrites lparams)))
                ;(display "\n\nb == ")
                ;(write b)
                ;(display "\n\ne == ")
                ;(write e)
                ;(display "\n\n")
                (list
                    (car e)
                    (cons 'c-begin
                        (append b (list (returnable (cadr e) tail?)))))))
        (list
            #f
            (cons 'c-begin
                (map (fn (x) (cadr (generate-code x '() #f rewrites lparams))) block)))))

(define (logic-type t)
    (cond
        (eq? t 'and) 'c-and
        (eq? t 'or) 'c-or
        else (error "invalid logic operation lookup attempt")))

(define (compile-logic block name tail? rewrites lparams type)
    " compile-logic: handle compilation of and/or/not syntax.
      this is a somewhat-complicated generation scheme here, and I'm
      not sure I like it. What's happening is that it binds the 
      individual operations of the form to temporaries, and then
      returns a c-begin block with c-var declarations for each,
      with a c-and/c-or/c-not IR. so:
      (and (> x y) (< x z))
      becomes:
      (c-begin
        (c-var it2 (c-primitive \"flt\" ( x y )))
        (c-var it3 (c-primitive \"fgt\" (x z)))
        (c-and it2 it3))
      which makes if/cond kinda complicated (if less-so than
      instances c-elif instances for cond). I'm thinking that something
      like just returning (c-and form0 form1 ... formN) and having the
      IR output:
      if(((it = flt(x,y)) && it->type == BOOL && it->object.c) && ...)
      which is much less complicated than the previous scheme, but relies
      on destructive update within an if form, which is kinda yucky.
    (let ((vals (map
                    (fn (x)
                        (list
                            'c-var
                            (gen-sym 'it)
                            (generate-code x '() #f rewrites lparams)))
                    block))
          (ir-type (logic-type type)))
        (list
            #f
            (cons
                'c-begin
                    (list
                        (cons
                            ir-type
                            vals))))))
    actually, thinking about it further, I do like the method expounded
    previously, so I'm commenting out the above & just running with it."
    ;; need to do a "tail?" check here, and if so, check if the last
    ;; member is a tail call. Would be weird in an and/or block, but 
    ;; kinda-sorta makes sense. Also, needs to use "returnable" anyway...
    (let ((ir-type (logic-type type)))
        (list #f
            (cons
                ir-type
                (map
                    (fn (x) (show (cadr (generate-code x '() #f rewrites lparams)) "inside compile-logic: "))
                    block)))))

(define (compile-apply block name tail? rewrites lparams)
    (cond
            (enyalios@primitive? (car block))
            (list
                #f
                (cons
                    'c-apply-primitive
                    (cons
                        (first (nth *primitives* (car block)))
                        (map
                            (fn (x)
                                (cadr
                                    (generate-code x '() #f rewrites lparams)))
                            (cdr block)))))
            (enyalios@var-prim? (car block))
            (list
                #f
                (cons
                    'c-apply-variable-primitive
                    (cons
                        (nth *varprimitives* (car block))
                        (map
                            (fn (x)
                                (cadr
                                    (generate-code x '() #f rewrites lparams)))
                            (cdr block)))))
            else (error "enyalios does not support this type for use in APPLY")))

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

(define (compile-let block name tail? rewrites lparams)
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
                            rewrites (dict))))
           (body (compile-begin (cdr block) name tail? var-temps lparams))
           (nulparams (dict-copy (keys lparams) lparams (dict))))
        (cset! nulparams "letvals" (cons vars (nth lparams "letvals" '())))
        ;(display "\n\nlet.body == ")
        ;(write body)
        ;(newline)
        ;(display "\n\nvars == ")
        ;(write vars)
        ;(display "\n")
        (list
            (car body)
            (cons
                'c-begin
                (append
                    (map 
                        (fn (x2) (list 'c-var
                                (nth var-temps (car x2))
                                (cadr (generate-code (cadr x2) name #f var-temps nulparams))))
                        (car block)) 
                    (cdr body))))))

(define (compile-let* block name tail? rewrites lparams)
    #f)

(define (generate-code c name tail? rewrites lparams)
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
            (key? c)
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
        (pair? (car c)) ;; lambda or the like?
            (let ((proc-name (generate-code (car c) name #f rewrites lparams)))
                (if (or (not (string? proc-name)) (not (symbol? proc-name)))
                    (error "Enyalios currently only supports lambda's in CAR position")
                    (list
                        #f
                        (list
                            'c-call
                            proc-name
                            (map
                                (fn (x) (cadr (generate-code x '() #f rewrites lparams)))
                                (cdr c))))))
        (eq? (car c) '%prim) (list #f (list 'c-%prim (cadr c)))
        (eq? (car c) '%include)
            (begin
                (set! *includes* (append *includes* (list (cdr c))))
                (list #f (list 'c-nop)))
        (eq? (car c) 'define-syntax)
            (begin
                (cset! *usyntax* (cadr c) (show (cddr c) "define-syntax capture: "))
                (list #f (list 'c-nop)))
        (eq? (car c) 'load) 
            (let* ((fh (open (coerce (cadr c) :string) :read))
                   (lines (enyalios@load fh))
                   (load-obj-code (map
                        (fn (line)
                            (cadr (generate-code line '() #f {} lparams)))
                        lines)))
                (close fh)
                (set! *ooblambdas* (append *ooblambdas* load-obj-code))
                (list #f (list 'c-nop)))
        (eq? (car c) 'if) (compile-if (cdr c) name tail? rewrites lparams)
        (eq? (car c) 'cond) (compile-cond (cdr c) name tail? rewrites lparams)
        (eq? (car c) 'case) (compile-case (cdr c) name tail? rewrites lparams)
        (eq? (car c) 'quote)
            (if (null? (cadr c))
                '(#f (c-nil))
                (list #f (list 'c-quote (cdr c))))
        (or
            (eq? (car c) 'define) 
            (eq? (car c) 'def)) ;; quite a bit of legacy F code uses def
            (cond
                (symbol? (cadr c))
                    (if (and
                            (pair? (caddr c))
                            (or
                                (eq? (car (caddr c)) 'lambda)
                                (eq? (car (caddr c)) 'fn)))
                        (compile-procedure (cdaddr c) (cadr c) #t rewrites lparams)
                        (list #f (list 'c-var (cadr c) (cadr (generate-code (caddr c) '() #f rewrites lparams)))))
                (pair? (cadr c))
                    (compile-procedure (cons (cdadr c) (cddr c)) (caadr c) #t rewrites lparams)
                else (error "illegal define form; DEFINE (SYMBOL | PAIR) FORM*"))
        (or (eq? (car c) 'lambda)
            (eq? (car c) 'fn))
            (compile-lambda (cdr c) name tail? rewrites lparams)
        (eq? (car c) 'let) (compile-let (cdr c) name tail? rewrites lparams)
        (eq? (car c) 'let*) (compile-let (cdr c) name tail? rewrites lparams) ;; no difference in PreF
        (eq? (car c) 'letrec) #t
        ; transform this into let, run same code
        (eq? (car c) 'with) 
            (compile-let
                (cons
                    (list (list (cadr c) (caddr c)))
                    (cdddr c))
                name
                tail?
                rewrites lparams)
        (eq? (car c) 'set!)
            (list
                #f
                (list
                    'c-set!
                    (if (dict-has? rewrites (cadr c))
                        (nth rewrites (cadr c))
                        (cadr c))
                    (cadr (generate-code (caddr c) name #f rewrites lparams))))
        (eq? (car c) 'apply)
            (compile-apply (cdr c) name tail? rewrites lparams)
        (eq? (car c) 'begin) (compile-begin (cdr c) name tail? rewrites lparams)
        (and tail? ;; we don't want to check for tail-call in non-tail position
            (eq? (car c) name)) ;; tail-call?
            (list
                #t
                (list
                    'c-tailcall
                    name
                    (map
                        (fn (x)
                            (if (and (pair? x) (eq? (car x) '+) (symbol? (cadr x)) (integer? (caddr x)))
                                (list 'c-primitive-fixed "inc" (list (cadr x) (caddr x))) ;; hmm (+ ip 1)
                                (cadr (generate-code x '() #f rewrites lparams))))
                        (cdr c))))
        (or (eq? (car c) 'or)
            (eq? (car c) 'and))
            (compile-logic (cdr c) name tail? rewrites lparams (car c))
        (enyalios@usyntax? (car c)) ;; user-defined syntax?
            (generate-code
                (show (show (syntax-expand1 (show (nth *usyntax* (car c)) "syntax: ") c)) "result of syntax: ")
                name
                tail?
                rewrites
                lparams)
        (enyalios@primitive? (car c)) (compile-primitive c name tail? rewrites lparams) ; all other primitive forms
        (enyalios@procedure? (car c)) (compile-primitive-procedure c name tail? rewrites lparams) ; primitive procs, like display
        (enyalios@var-prim? (car c)) (compile-variable-primitive c name tail? rewrites lparams) ; list & friends
        (enyalios@parameter-call? (car c) lparams) ;; are we attempting to call a paramter?
            (let* ((name (nth lparams "name"))
                   (data (nth *ulambdas* name)))
                (cset!
                    data
                    5
                    (cons
                        (list (car c) (- (length c) 1))
                        (nth data 5)))
                (list
                    #f
                    (list
                        'c-call-variable
                        (car c)
                        (map
                            (fn (x) (cadr (generate-code x '() #f rewrites lparams)))
                            (cdr c)))))
        (enyalios@ulambda? (car c)) ; user-defined lambda?
            (list
                #f
                (list
                    'c-call
                    (car c)
                    (map
                        (fn (x) (cadr (generate-code x '() #f rewrites lparams)))
                        (cdr c))))
        else (error (format "unknown form: ~a" c))))

;; END il-compiler
;; BEGIN il->c

(define (ascii-acceptable? c)
    (or
        (and (char>=? c #\a) (char<=? c #\z))
        (and (char>=? c #\A) (char<=? c #\Z))
        (and (char>=? c #\0) (char<=? c #\9))
        (eq? c #\_)))

(define (cmung o)
    ;(show o "%%CMUNG-I-VALUE: ")
    (map
        (fn (x)
            (if (ascii-acceptable? x)
                x
                (if (eq? x #\?)
                    #\p
                    #\_)))
            (coerce o 'string)))

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

(define (generate-sexps n)
    ;(show n "generate-sexps: ")
    (if (= n 0)
        '()
        (cons "SExp *" (generate-sexps (- n 1)))))

(define (params->c params param-data)
    ;(show param-data "param->c param-data: ")
    (if (null? params)
        '()
        (let* ((cur (car params))
               (data (nth param-data 5)))
            ;(show data "params->c::data ")
            (if (not (eq? (assq cur data) #f))
                (cons
                    (format
                        "SExp *(*~a)(~s)"
                        cur
                        (string-join 
                            (generate-sexps (cadar data))
                            ", "))
                    (params->c (cdr params) param-data))
                (cons
                    (format "SExp *~a" (cmung cur))
                    (params->c (cdr params) param-data))))))
                    
(define (generate-vector x)
    (let ((n (length x)) (p (coerce x 'pair)))
        (string-append (format "vector(~n," n) (string-join (map generate-quoted-literal p) ",") ")")))

(define (generate-pair x)
    (string-append
        "cons("
        (generate-quoted-literal (car x))
        ", "
        (generate-quoted-literal (cdr x))
        ")"))

(define (generate-list x)
    ;(display "%%GENERATE-LIST: x ->")
    ;(write x)
    ;(newline)
    (let ((n (length x)))
        (string-append (format "list(~n," n) (string-join (map generate-quoted-literal x) ",") ")")))

(define (generate-dict d)
    ;(display "%%I-GENERATE-DICT: (keys d) -> ")
    ;(write (keys d))
    ;(newline)
    (if (empty? (keys d)) ; have to update empty? to check keys automagically...
        "makedict()"
        (format "fdict(~s)" 
            (string-join
                (map
                    (fn (x) (format "\"~s\", ~s" x (generate-quoted-literal (nth d x))))
                    (keys d))
                ", "))))

(define (generate-number x)
    (cond
        (integer? x) (format "makeinteger(~n)" x)
        (rational? x) (format "makerational(~n,~n)" (numerator x) (denomenator x))
        (real? x) (format "makereal(~n)" x)
        (complex? x) (format "makecomplex(~n,~n)" (real-part x) (imag-part x))
        else (error "NaN")))

(define (generate-quoted-literal x)
    (cond
        (vector? x) (generate-vector x)
        (list? x) (generate-list x) ; proper, non-dotted list
        (pair? x) (generate-pair x) ; improper, dotted list
        (dict? x) (generate-dict x)
        (symbol? x) (format "makeatom(\"~a\")" x)
        (number? x) (generate-number x)
        (goal? x) (if (eq? x #s) "SSUCC" "SUNSUCC")
        (bool? x) (if x "STRUE" "SFALSE")
        (string? x) (format "makestring(~S)" x)
        (char? x) (format "makechar('~c')" x)
        (key? x) (format "makekey(\"~a\")" x)
        (null? x) "SNIL"))

(define (condition-connector c connector out)
    " used by if-condition to map a connector
      over a set of statements. so
      (c-and (flt 3 4) (fgt 6 5))
      should be joined as 
      (flt(3,4) == STRUE) && (fgt(6,5) == STRUE)"
    (if (null? (cdr c))
        (if-condition (car c) out)
        (begin
            (if-condition (car c) out)
            (display (format " ~s " connector) out)
            (condition-connector (cdr c) connector out))))

(define (optimizable-primitive? c)
    (show c "optimizable-primitive? c == ")
    (or
        (and
            (eq? (car c) 'c-primitive-fixed)
            (or
                (eq? (cadr c) "eqp")
                (eq? (cadr c) "fdset")
                (eq? (cadr c) "fvset")
                (eq? (cadr c) "fvref")
                (eq? (cadr c) "inc")
                (eq? (cadr c) "typep")
                (eq? (cadr c) "fnot")))
        (and
            (eq? (car c) 'c-primitive)
            (or
                (eq? (cadr c) "fplus")
                (eq? (cadr c) "fnumeq")
                (eq? (cadr c) "flt")
                (eq? (cadr c) "flte")
                (eq? (cadr c) "fgt")
                (eq? (cadr c) "fgte")))
        #f))

(define (optimize-logical code out status)
    "optimize a large swath of logical operations: <, >, <=, >=, =. Handle cases similar to
     optimize +. Probably can be used for most operators (+,/,*,%); would need a NEST-OR-JOIN
     flag that could be used to either nest or join the operations with && or the like."
    (let* ((args (caddr code))
           (a0 (car args))
           (a1 (cadr args))
           (lp (length args))
           (proc (cadr code)))  
        (if status
            (display "(" out)
            #v)
        (cond
            (> lp 2)
                ;; these could be decomposed down properly, but I'm lazy atm
                (begin
                    (display proc out)
                    (display (format "(list(~n," lp) out)
                    (comma-separated-c args out)
                    (display ")" out))
            (and
                (or
                    (symbol? a0)
                    (pair? a0))
                (or
                    (symbol? a1)
                    (pair? a1)))
                (begin
                    (display (format "~a_nn(" proc) out)
                    (comma-separated-c args out)
                    (display ")" out))
            ;; this whole thing could be DRYed out a bit, but
            ;; for now it's fine
            (or
                (symbol? a0)
                (pair? a0))
                (cond
                    (integer? a1)
                        (begin
                            (display (format "~a_ni(" proc) out)
                            (il->c a0 0 out)
                            (display ", " out)
                            (display a1 out)
                            (display ")" out))
                    (real? a1)
                        (begin
                            (display (format "~a_nr(" proc) out)
                            (il->c a0 0 out)
                            (display ", " out)
                            (display a1 out)
                            (display ")" out))
                    (rationalq? a1)
                        (begin
                            (display (format "~a_nq(" proc) out)
                            (il->c a0 0 out)
                            (display ", " out)
                            (display (numerator a1) out)
                            (display ", " out)
                            (display (denomenator a1) out)
                            (display ")" out))
                    (complex? a1)
                        (begin
                            (display (format "~a_nc(" proc) out)
                            (il->c a0 0 out)
                            (display ", " out)
                            (display (real-part a1) out)
                            (display ", " out)
                            (display (imag-part a1) out)
                            (display ")" out))
                    else
                        (error "Incorrect argument for logical procedure"))
            (or 
                (symbol? a1)
                (pair? a1))
                (cond
                    (integer? a0)
                        (begin
                            (display (format "~a_in(" proc) out)
                            (display a0 out)
                            (display ", " out)
                            (il->c a1 0 out)
                            (display ")" out))
                    (real? a0)
                        (begin
                            (display (format "~a_rn(" proc) out)
                            (display a0 out)
                            (display ", " out)
                            (il->c a1 0 out)
                            (display ")" out))
                    (rational? a0)
                        (begin
                            (display (format "~a_qn(" proc) out)
                            (display (numerator a0) out)
                            (display ", " out)
                            (display (denomenator a0) out)
                            (display ", " out)
                            (il->c a1 0 out)
                            (display ")" out))
                    (complex? a0)
                        (begin
                            (display (format "~a_cn(" proc) out)
                            (display (real-part a0) out)
                            (display ", " out)
                            (display (imag-part a0) out)
                            (display ", " out)
                            (il->c a1 0 out)
                            (display ")" out))
                    else
                        (error "Incorrect argument for logical procedure"))
            else
                (error "Unable to optimize logical procedure; incorrect arguments provided."))
            (if status
                (display ") == STRUE" out)
                #v)))

(define (optimize-eq code out status)
    (show status "optimize-eq stats == ")
    (show code "optimize-eq code == ")
    (let* ((args (caddr code)) ; destructuring bind would be nice here...
           (a0 (car args))
           (a1 (cadr args)))
        (cond
            (not status) ;; even here, could be a0 == a1 ? STRUE : SFALSE
                (begin
                    (display "eqp(" out)
                    (il->c a0 0 out)
                    (display ", " out)
                    (il->c a1 0 out)
                    (display ")" out))
            (and ;; two symbols? fall back on eqp
                (symbol? a0) ;; might be able to do some sort of
                (symbol? a1)) ;; type test, but still...
                (begin
                    (display "eqp(" out)
                    (il->c a0 0 out)
                    (display ", " out)
                    (il->c a1 0 out)
                    (display ")" out))
            (and
                (symbol? a0)
                (integer? a1))
                (display
                    (format
                        "(~a->type == NUMBER && NTYPE(~a) == INTEGER && AINT(~a) == ~a)"
                        a0 a0 a0 a1)
                    out)
            (and
                (symbol? a0)
                (real? a1))
                (display
                    (format
                        "(~a->type == NUMBER && NTYPE(~a) == REAL && AREAL(~a) == ~a)"
                        a0 a0 a0 a1)
                    out)
            (and
                (symbol? a0)
                (string? a1))
                (display 
                    (format
                        "(~a->type == STRING && !strncmp(~a->object.str, \"~s\", ~a->length))"
                        a0 a0 a1 a0)
                    out)
            (and ;; (eq? x 'ATOM) optimization...
                (symbol? a0)
                (pair? a1)
                (eq? (car a1) 'c-quote)
                (eq? (type (caadr a1)) "Symbol"))
                (display
                    (format
                        "(~a->type == STRING && !strncasecmp(~a->object.str,\"~a\", ~a->length))"
                        a0 a0 (caadr a1) a0)
                    out)
            (and
                (pair? a1)
                (eq? (car a1) 'c-quote)
                (eq? (type (caadr a1)) "Symbol"))
                (begin
                    (display "(eqp_atom(" out)
                    (il->c a0 0 out)
                    (display (format ", \"~a\") == STRUE)" (caadr a1)) out))
            else
                (begin
                    (display "(eqp(" out)
                    (il->c a0 0 out)
                    (display ", " out)
                    (il->c a1 0 out)
                    (display ") == STRUE)" out)))))

(define *tower-lookup* {
    :Integer 0
    :Rational 1
    :Real 2
    :Complex 3
})

(define (tower-order? o0 o1)
    (cond
        (and (number? o0) (number? o1) (eq? (numeric-type o0) (numeric-type o1)))
            #t
        (and (integer? o0) (rational? o1)) #t
        (and (integer? o0) (real? o1)) #t
        (and (integer? o0) (complex? o1)) #t
        (and (rational? o0) (real? o1)) #t
        (and (rational? o0) (complex? o1)) #t
        (and (real? o0) (complex? o1)) #t
        (and (number? o0) (not (number? o1))) #t
        else #f))

(define (optimize-add o out)
    " optimize the primitive \"fplus\" to be a bit more
      performance-friendly. Things to look into:
      - constant folding: (+ x 3 4 5) should just be (+ x 10)
      - turning (+ x y z) into fplus_nn(x, fplus_nn(y, z)) or the like (no intermediate list creation)
      - look into other optimizations, like in-place addition (something like (add! x 10) or incf from CL)
    "
    (let* ((args (caddr o))
          (arg-len (length (caddr o)))
          (a0 (nth args 0 #f)))
        (show args "optimize-add args == ")
        (show arg-len "optimize-add arg-len == ")
        (cond
            (> arg-len 2)
                (begin
                    (display "fplus(list(" out)
                    (display arg-len out)
                    (display ", " out)
                    (comma-separated-c args out)
                    (display "))" out))
            (= arg-len 0) 
                (display "makeintger(0)" out)
            (= arg-len 1)
                (cond
                    (integer? a0)
                        (display (format "makeinteger(~a)" a0) out)
                    (rational? a0)
                        (display
                            (format
                                "makerational(~a, ~a)"
                                (numerator a0)
                                (denomenator a0))
                            out)
                    (real? a0)
                        (display (format "makereal(~a)" a0) out)
                    (complex? a0)
                        (display
                            (format
                                "makecomplex(~a, ~a)"
                                (real-part a0)
                                (imag-part a0))
                            out)
                    else
                        (il->c a0 0 out))
            (= arg-len 2)
                (let ((a1 (nth args 1)))
                    " we can cheat here; since multiplication & addition are commutative, I can change the operands' order
                      and make optimizations easier here than requiring a full table of numeric type heirarchies.
                    "
                    (show "" "in arg-len == 2\n")
                    (if (tower-order? a0 a1)
                        #v
                        (begin
                            (set! a0 (nth args 1))
                            (set! a1 (nth args 0))))
                    (cond
                        (and (integer? a0) (integer? a1))
                            (display (format "makeinteger(~a)" (+ a0 a1)) out)
                        (and (integer? a0) (rational? a1))
                            (display (format "makerational(~a, ~a)" (+ a0 (numerator a1)) (denomenator a1)) out)
                        (and (integer? a0) (real? a1))
                            (display (format "makereal(~a)" (+ a0 a1)) out)
                        (and (integer? a0) (complex? a1))
                            (display (format "makecomplex(~a, ~a)" (+ a0 (real-part a1)) (imag-part a1)) out)
                        (and (rational? a0) (real? a1))
                            (display (format "makereal(~a)" (+ a0 a1)) out)
                        (and (rational? a0) (complex? a1))
                            (display (format "makecomplex(~a, ~a)" (+ a0 (real-part a1)) (image-part a1)) out)
                        (and (complex? a0) (complex? a1))
                            (display
                                (format
                                    "makecomplex(~a, ~a)"
                                    (+ (real-part a0) (real-part a1)) 
                                    (+ (imag-part a0) (imag-part a1)))
                                out)
                        (integer? a0)
                            (begin
                                (display (format "fplus_in(~a, " a0) out)
                                (il->c a1 0 out)
                                (display ")" out))
                        (rational? a0)
                            (begin
                                (display (format "fplus_qn(~a, ~a, " (numerator a0) (denomenator a0)) out)
                                (il->c a1 0 out)
                                (display ")" out))
                        (real? a0)
                            (begin
                                (display (format "fplus_rn(~a, " a0) out)
                                (il->c a1 0 out)
                                (display ")" out))
                        (complex? a0)
                            (begin
                                (display (format "fplus_cn(~a, ~a, " (real-part a0) (imag-part a0)) out)
                                (il->c a1 0 out)
                                (display ")" out))
                        else
                            (begin
                                (display "fplus_nn(" out)
                                (il->c a0 0 out)
                                (display ", " out)
                                (il->c a1 0 out)
                                (display ")" out)))))))

(define (optimize-typep code out status?)
    (let* ((args (caddr code)) ; destructuring bind would be nice here...
           (obj (car args))
           (type (cadr args)))
        (display "TYPEP(" out)
        (il->c obj 0 out)
        (display ", " out)
        (display type out)
        (display ")" out)))

(define (optimize-fnot code out status?)
    (let* ((args (caddr code)) 
           (obj (car args)))
        (if (optimizable-primitive? obj)
            (begin
                (display "!(" out)
                (optimize-primitive obj out status?)
                (if status?
                    (display ")" out)
                    (display ") == STRUE ? STRUE : SFALSE" out)))
            (begin
                (display "fnot(" out)
                (il->c obj 0 out)
                (if status?
                    (display ") == STRUE" out) 
                    (display ")" out))))))

(define (optimize-dset o out)
    (let* ((vals (caddr o))
          (d (car vals))
          (key (cadr vals))
          (obj (caddr vals)))
        (if (not (symbol? d))
            (begin
                (display "fcset(" out)
                (il->c d 0 out)
                (display ", " out)
                (il->c key 0 out)
                (display ", " out)
                (il->c obj 0 out)
                (display ")" out))
            (begin
                (display "trie_put(" out)
                (cond
                    (symbol? key) (display (format "ASTRING(~s)" (cmung key)) out)
                    (or (key? key) (string? key)) (display (format "\"~a\"" key) out)
                    else (il->c key 0 out))
                (display ", " out)
                (il->c obj 0 out)
                (display ", " out)
                (display d out)
                (display "->object.dict)" out)))))

(define (optimize-vset o out)
    (let* ((vals (caddr o))
          (d (car vals))
          (key (cadr vals))
          (obj (caddr vals)))
        (if (not (symbol? d))
            (begin
                (display "fcset(" out)
                (il->c d 0 out)
                (display ", " out)
                (il->c key 0 out)
                (display ", " out)
                (il->c obj 0 out)
                (display ")" out))
            (begin
                (display (cmung d) out)
                (display "->object.vec[" out)
                (cond
                    (symbol? key) (display (format "AINT(~s)" (cmung key)) out)
                    (integer? key) (display key out)
                    else 
                        (begin
                            (display "AINT(" out) 
                            (il->c key 0 out)
                            (display ")" out)))
                (display "] = " out)
                (il->c obj 0 out)))))

(define (optimize-vref o out)
    (let* ((vals (caddr o))
          (d (car vals))
          (key (cadr vals)))
        (if (not (symbol? d))
            (begin
                (display "fnth(" out)
                (il->c d 0 out)
                (display ", " out)
                (il->c key 0 out)
                (display ", SNIL)" out))
            (begin
                (display (cmung d) out)
                (display "->object.vec[" out)
                (cond
                    (symbol? key) (display (format "AINT(~s)" (cmung key)) out)
                    (integer? key) (display key out)
                    else 
                        (begin
                            (display "AINT(" out) 
                            (il->c key 0 out)
                            (display ")" out)))
                (display "]" out)))))

(define (optimize-primitive o out (status #f))
    ;; should probably be a case on (cadr o)
    (cond
        (eq? (cadr o) "eqp")
            (optimize-eq o out status)
        (eq? (cadr o) "fplus")
            (optimize-add o out)
        (eq? (cadr o) "fdset")
            (optimize-dset o out)
        (eq? (cadr o) "fvset")
            (optimize-vset o out)
        (eq? (cadr o) "fvref")
            (optimize-vref o out)
        (eq? (cadr o) "typep")
            (optimize-typep o out status)
        (eq? (cadr o) "fnot")
            (optimize-fnot o out status)
        (eq? (cadr o) "inc")
            (let ((first-arg (caaddr o))
                  (second-arg (cadr (caddr o))))
                (display "inc_i(" out) ;; FIXME: do actual type dispatch here
                (display (cmung first-arg) out)
                (display ", " out)
                (display second-arg out)
                (display ")" out))
        (or
            (eq? (cadr o) "flt")
            (eq? (cadr o) "flte")
            (eq? (cadr o) "fgt")
            (eq? (cadr o) "fgte")
            (eq? (cadr o) "fnumeq"))
            (optimize-logical o out status)
        else
            (error "unable to optimize primitive form")))

(define (if-condition <cond> out)
    " if-condition handles processing of the <cond> portion of
      c-if/c-elif. It handles c-and & c-or, and potentially handle
      optimizations to if statements (like rewriting flt to flt_XX)"
    (show <cond> "in if-condition: ")
    (cond
        (eq? (car <cond>) 'c-and) 
            (condition-connector
                (cdr <cond>)
                "&&"
                out)
        (eq? (car <cond>) 'c-or)
            (condition-connector
                (cdr <cond>)
                "||"
                out)
        (optimizable-primitive? <cond>)
            (optimize-primitive <cond> out #t)
        else
            (begin
                (display "(" out)
                (il->c <cond> 0 out)
                (display " == STRUE)" out))))

;; this could be made generic with another param
;; this param would point to the generator to call...
;; not bad...
(define (stand-alone-logic->c il lvl out connector)
    (int->spaces lvl out)
    (if (null? (cdr il))
        (il->c (car il) 0 out)
        (begin
            (display "(" out)
            ;(if-condition (car il) out)
            (il->c (car il) 0 out)
            (display " == STRUE) ? " out)
            (if connector
                (begin
                    (stand-alone-logic->c (cdr il) lvl out connector)
                    (display " : SFALSE" out))
                (begin
                    (display "STRUE : " out)
                    (stand-alone-logic->c (cdr il) lvl out connector))))))

(define (goto-labels n l base)
    (cond
        (empty? n)
            (tconc->pair l)
        (eq? (car n) 'else)
            (begin
                (tconc! l base)
                (tconc->pair l))
        else
        (begin
            (tconc! l (gensym 'labl))
            (goto-labels (cdr n) l base))))

(define (ir->c-case il lvl out)
    "outputs a set of GOTOs and a hashtable to contain the jump table.
     still need to look into memoizing this so that it is only built
     the first time it is run."
    (let* ((table-name (gensym 'jmptab))
           (base (gensym 'base))
           (offset (gensym 'offset))
           (tmp (gensym 'tmp))
           (seps (unzip (show (cdr il) "ir->c-case::unzip ")))
           (states (car seps))
           (labels (goto-labels states (make-tconc '()) base))
           (codes (cadr seps))
           (init (car il)))
        (int->spaces lvl out)
        (display (format "static AVLNode *~a = nil;\n" table-name) out)
        (int->spaces lvl out)
        (display (format "void *~a = nil;\n" offset) out)
        (int->spaces lvl out)
        (display (format "if(~a == nil){\n" table-name) out)
        (int->spaces (+ lvl 1) out)
        (display (format "~a = makeavlnode(0);\n" table-name) out)
        ;; need to do two things:
        ;; - iterate over each item in states (which could be (1 2 3))
        ;; - figure out how to type a set of states to a GOTO table... 
        (foreach*
            (fn (state label)
                (if (eq? state 'else)
                    #v
                    (foreach (fn (z)
                        (int->spaces (+ lvl 1) out)
                        (display (format "avl_insert(~a, ~a, &&~a);~%" table-name z label) out))
                        state)))
            (list states labels))
        (int->spaces lvl out)
        (display "}\n" out)
        (int->spaces lvl out)
        (display (format "SExp *~a = " tmp) out)
        (il->c init 0 out)
        (display ";\n" out)
        (int->spaces lvl out)
        (display (format "~a = (void *)avl_get(~a, AINT(~a));\n" offset table-name tmp) out)
        (int->spaces (+ lvl 1) out)
        (display (format "if(~a != nil){\n" offset tmp) out)
        (int->spaces (+ lvl 1) out)
        (display (format "goto *~a;~%" offset) out)
        (int->spaces lvl out)
        (display "}\n" out)
        ;; and here, need to tie code together with GOTO states.
        (foreach*
            (fn (label code)
               (display (format "~a:\n" label) out)
               (int->spaces lvl out)
               (display "{\n" out)
               (il->c code lvl out)
               (int->spaces lvl out)
               (display "}\n" out))
            (list labels codes))))

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
        (char? il) (display (format "makechar('~c')" il) out)
        (symbol? il) (display (cmung il) out)
        (key? il) (display (format "makekey(\"~a\")" il) out)
        (dict? il) (display (generate-dict il) out)
        (pair? (car il))
            (foreach-proc (fn (x) (il->c x lvl out)) il)
        (eq? (car il) 'c-%prim)
            (display (cadr il) out)
        (eq? (car il) 'c-nop) ;; do nothing :D
            #v
        (eq? (car il) 'c-nil)
            (display "SNIL" out)
        (eq? (car il) 'c-quote)
            (display (generate-quoted-literal (caadr il)) out)
        (eq? (car il) 'c-if)
            (begin
                (int->spaces lvl out)
                (display "if(" out)
                (if-condition (cadr (show il "about to call if-condition: ")) out)
                (display "){\n" out)
                (il->c (cddr il) (+ lvl 1) out)
                (int->spaces lvl out)
                (display "}\n" out))
        (eq? (car il) 'c-elif)
            (begin
                (int->spaces lvl out)
                (display "else if(" out)
                (if-condition (cadr il) out)
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
        (eq? (car il) 'c-case)
            (ir->c-case (cdr il) lvl out) 
        (eq? (car il) 'c-and)
            (stand-alone-logic->c (cdr il) lvl out #t)
        (eq? (car il) 'c-or)
            (stand-alone-logic->c (cdr il) lvl out #f)
        (eq? (car il) 'c-loop)
            (begin
                (int->spaces lvl out)
                (display "while(1) {\n" out)
                (il->c (cadr il) (+ lvl 1) out)
                (int->spaces lvl out)
                (display "}\n" out))
        (eq? (car il) 'c-return)
            (begin
                (if *profiling*
                    (begin
                        (int->spaces lvl out)
                        (display "gettimeofday(&time, NULL);\n" out)
                        (int->spaces lvl out)
                        (display "printf(\"end %s::%d%d\\n\", __FUNCTION__, time.tv_sec, time.tv_usec);\n" out))
                    #v)
                (int->spaces lvl out)
                (display "return " out)
                (if (and
                        (pair? (caddr il))
                        (optimizable-primitive? (cadr il)))
                    (optimize-primitive (cadr il) out #t)
                    (il->c (cadr il) 0 out))
                (display ";\n" out))
        (eq? (car il) 'c-no-return)
            (begin
                (int->spaces lvl out)
                (if (or
                        (eof-object? (cadr il))
                        (vector? (cadr il))
                        (number? (cadr il))
                        (char? (cadr il))
                        (void? (cadr il))
                        (bool? (cadr il))
                        (goal? (cadr il))
                        (string? (cadr il))
                        (key? (cadr il)))
                    (display "1" out)
                    (il->c (cadr il) 0 out))
                (display ";\n" out))
        (eq? (car il) 'c-var) ;; variable declaration
            (begin
                (int->spaces lvl out)
                (display "SExp *" out)
                (display (cmung (cadr il)) out)
                (display " = " out)
                (il->c (caddr il) 0 out)
                (display ";\n" out))
        (eq? (car il) 'c-set!) 
            (begin
                (int->spaces lvl out)
                (display (cmung (cadr il)) out)
                (display " = " out)
                (il->c (caddr il) 0 out)
                (display ";\n" out))
        (eq? (car il) 'c-dec) ;; function declaration
            (begin
                (display "SExp *\n" out)
                (display (cmung (cadr il)) out) ;; should be a call to MUNG here...
                (display "(" out)
                (if (null? (caddr il))
                    #v
                    (display ;; can soon switch this to params->c
                        (string-join
                            (params->c (caddr il) (nth *ulambdas* (cadr il)))
                            ", ")
                        out))
                (display "){\n" out)
                (if *profiling*
                    (begin
                        (int->spaces lvl out)
                        (display "struct timeval time;\n" out)
                        (int->spaces lvl out)
                        (display "gettimeofday(&time, NULL);\n" out)
                        (int->spaces lvl out)
                        (display "printf(\"start %s::%d%d\\n\", __FUNCTION__, time.tv_sec, time.tv_usec);\n" out))
                    #v)
                (il->c (cadddr il) (+ lvl 1) out)
                (display "}\n" out))
        (eq? (car il) 'c-shadow-params)
            (let ((proc-data (nth *ulambdas* (cadr il))))
                (foreach-proc
                    (fn (x)
                        (int->spaces lvl out)
                        (display (format "SExp *~a = SNIL;~%" (cmung x)) out))
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
        (eq? (car il) 'c-primitive-null)
            (begin
                (display (cmung (cadr il)) out)
                (display "()" out))
        (eq? (car il) 'c-primitive)
            (cond
                (null? (caddr il)) (display (format "~a(SNIL)" (cadr il) out))
                (optimizable-primitive? il) (optimize-primitive il out)
                else
                (begin
                    (display (cadr il) out)
                    (display "(list(" out)
                    (display (length (caddr il)) out)
                    (display ", " out)
                    (comma-separated-c (caddr il) out)
                    (display "))" out)))
        (eq? (car il) 'c-primitive-fixed) ;; fixed arity primitive
            (if (optimizable-primitive? il)
                (optimize-primitive il out)
                (begin
                    (display (cadr il) out)
                    (display "(" out)
                    (comma-separated-c (caddr il) out)
                    (display ")" out)))
        (eq? (car il) 'c-begin)
            (foreach-proc
                (fn (x)
                    ;(write x)
                    ;(display "\n")
                    (if (il-syntax? x)
                        (il->c x lvl out)
                        (begin
                            (int->spaces lvl out) 
                            (il->c x lvl out)
                            (display ";\n" out))))
                (cdr il))
        (eq? (car il) 'c-tailcall)
            (let ((proc-data (nth *ulambdas* (cadr il))))
                ;; This section can be rewritten:
                ;; use a map*/foreach*, which is just normal map in Scheme,
                ;; to accept the three lists (params, shadows, code), then
                ;; using this extra information, decide if code really needs
                ;; to be shadowed or even addressed:
                ;; (define (foo code ip i j)
                ;; (foo code (+ ip 1) (+ i j) i)
                ;; code -> no output
                ;; ip -> fplus_in(1, ip)
                ;; i, j shadowed output.
                ;; TODO: check if this can be cleaned up; don't need to necessarily
                ;; shadow parameters that need not be.
                (cond
                    (< (length (caddr il)) (nth proc-data 2))
                        (error (format "Incorrect arity for user-defined lambda: ~a" (cadr il)))
                    (= (length (caddr il)) 0)
                        (begin
                            (int->spaces lvl out)
                            (display "continue;\n" out))
                    else
                    (begin
                        ;; set shadow params to value of each
                        ;; parameters:
                        ;; x1 = (+ x 1)
                        ;; y2 = (* y 3)
                        (foreach-proc
                            (fn (x3)
                                (int->spaces lvl out)
                                (display
                                    (format "~a = "
                                        (cmung (car x3)))
                                    out)
                                (il->c (cadr x3) 0 out)
                                (display ";\n" out))
                            (zip 
                                (nth proc-data 4) ;; shadow params
                                (caddr il)))
                        ;; set each parameter to the shadow's value:
                        ;; x = x1
                        ;; y = y2
                        (foreach-proc
                            (fn (x4)
                                (int->spaces lvl out)
                                (display
                                    (format "~a = ~a; ~%" 
                                        (cmung (car x4))
                                        (cmung (cadr x4)))
                                    out))
                            (zip
                                (nth proc-data 1)
                                (nth proc-data 4)))
                        (int->spaces lvl out)
                        (display "continue;\n" out))))
        (eq? (car il) 'c-call)
            (let ((proc-data (nth *ulambdas* (cadr il))))
                (if (< (length (caddr il)) (nth proc-data 2))
                    (error (format "Incorrect arity for user-defined lambda ~a" (cadr il)))
                    (begin
                        (display (cmung (cadr il)) out)
                        (display "(" out)
                        (comma-separated-c (caddr il) out)
                        (display ")" out))))
        (eq? (car il) 'c-call-variable)
            (begin
                (display (cmung (cadr il)) out)
                (display "(" out)
                (comma-separated-c (caddr il) out)
                (display ")" out))
        (eq? (car il) 'c-variable-primitive)
            (let ((name (nth *varprimitives* (cadr il))))
                (display (format "~a(~n, " name (length (caddr il))) out)
                (comma-separated-c (caddr il) out)
                (display ")" out))
        (eq? (car il) 'c-procedure)
            (begin
                (display (cadr il) out)
                (if (or (eq? (caddr il) 'c-nil) (null? (caddr il)))
                    (display "(SNIL, " out)
                    (begin
                        (display "(list(" out)
                        (display (length (caddr il)) out)
                        (display ", " out)
                        (comma-separated-c (caddr il) out)
                        (display "), " out)))
                (display (cadddr il) out)
                (display ")" out))
        (eq? (car il) 'c-apply-primitive)
            (begin
                (display (cadr il) out)
                (display "(" out)
                (il->c (caddr il) 0 out)
                (display ")" out))
        (eq? (car il) 'c-apply-variable-primitive)
            (let ((name (nth *varprimitives* (cadr il))))
                (display "il => ")
                (write il)
                (newline)
                (display (format "~a(~n, " name (length (caddr il))) out)
                (il->c (caddr il) 0 out)
                (display ")" out))
        else
            (display "###" out)))

;; END il->c
;; BEGIN main-driver

(define (enyalios@load in)
    (with r (read in)
        (if (eof-object? r)
            '()
            (begin
                (if (and
                        (pair? r)
                        (or
                            (eq? (car r) 'define)
                            (eq? (car r) 'def)))
                    (cond
                        (symbol? (cadr r))
                            (if (and
                                (pair? (caddr r))
                                (or
                                    (eq? (car (caddr r)) 'lambda)
                                    (eq? (car (caddr r)) 'fn)))
                                (set-arity! (cadr r) (car (cdaddr r)))
                                #v)
                        (pair? (cadr r))
                            (set-arity! (caadr r) (cdadr r))
                        else #v)
                    #v)
                (cons
                    r
                    (enyalios@load in))))))

(define (enyalios@dump-lambdas lams out)
    (if (null? lams)
        #v
        (begin
            (il->c (car lams) 0 out)
            (enyalios@dump-lambdas (cdr lams) out))))

(define (enyalios@compile-loop code env-name)
    (if (null? code)
        (begin
            (display "\nfinished compiling\n")
            '())
        (let ((o (car code)))
            (if (and
                    (pair? o)
                    (or (eq? (car o) 'define)
                        (eq? (car o) 'def)))
                (display (format "COMPILING: ~a~%" (cadr o)))
                #v)
            (cons 
                (cadr (generate-code o '() #f (dict) (dict "env" env-name)))
                (enyalios@compile-loop (cdr code) env-name)))))

(define (enyalios@dump-prototypes names d out) 
    (if (null? names)
        #v
        (let* ((name (car names))
               (data (nth d name)))
            (display "SExp *" out)
            (display (cmung (car names)) out)
            (display "(" out)
            (if (null? (nth data 1))
                #v
                (display
                    (string-join 
                        (params->c (nth data 1) data)
                        ", ")
                    out))
            (display ");\n" out)
            (enyalios@dump-prototypes (cdr names) d out))))

(define (enyalios in-file out-file init)
    (let* ((inf (open in-file :read))
          (outf (open out-file :write))
          (code (enyalios@load inf))
          (env-name (gensym "enyalios"))
          (obj-code (enyalios@compile-loop code env-name)))
        (display (format "LOADED: ~a~%GENERATING INCLUDES... " in-file))
        (if (empty? *includes*)
            #v  
            (foreach-proc
                (fn (include)
                    (if (cadr include)
                        (display (format "#include <~a>~%" (car include)) outf)
                        (display (format "#include \"~a\"~%" (car include)) outf)))
                *includes*))
        (display "DONE.\nGENERATING C PROTOTYPES... ")
        (enyalios@dump-prototypes (keys *ulambdas*) *ulambdas* outf)
        (display "Symbol *" outf)
        (display env-name outf)
        (display ";\n\n" outf)
        (display "DONE.\nGENERATING C ANONYMOUS LAMBDAS... ")
        (enyalios@dump-lambdas *ooblambdas* outf)
        (display "DONE.\nGENERATING C LAMBDAS... ")
        (enyalios@dump-lambdas obj-code outf)
        (display "DONE\n")
        (close inf)
        (close outf)))
;; END main-driver

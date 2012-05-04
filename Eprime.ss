; A restricted Digamma->C compiler that supports a subset of the Digamma spec.
; it's probably a crappy method of compilation, but it eases transition, since it
; reuses Vesta's runtime. The Best method would be to have a nice Type inference system
; that uses something similar to a tagged pointer for SExprs, rather than the heavy struct/union
; I use now. On top of that, I could unbox many things; this is definitely the tact I will take
; with future releases, but for now I'm just looking to speed up Digamma development & run times 
; :D
; zlib/png licensed (c) 2010 Stefan Edwards

; E' generates human-readable C code from a strict Digamma subset. It focuses on
; self-tail call optimization & research. Eventually, with the addition of 
; contracts, I hope to make this a decent compiler, and grow it into the full
; Enyo, which should support inference, whole program analysis & unboxing

; E's main purpose is to act as an extension compiler for Vesta; it could be used as a stand
; alone compiler (if you wanted to link against the Vesta runtime), but the main focus is three
; fold:
; -- research techniques for compiling human-readable code
; -- an extension compiler for Vesta
; -- test out new ideas related to compilers quickly

(def *fnarit* {}) ; this is a dict of the various functions' arity
(def *fnmung* {}) ; maps the program's lambda's name to the munged version
(def *ooblam* {}) ; lambdas that are lifted can be placed here; for lift-map & friends
(def *primitives* {
:car [1 #f "car"]
:cdr [1 #f "cdr"] 
:cons [2 #f "cons"] 
:length [1 #f "flength"] 
:+ [0 #f "fplus"]
:exact? [1 #f "fexactp"]
:inexact? [1 #f "finexactp"]
:real? [1 #f "frealp"]
:integer? [1 #f "fintegerp"]
:complex? [1 #f "fcomplexp"]
:rational? [1 #f "frationalp"]
:numerator [1 #f "fnum"]
:denomenator [1 #f "fden"]
:* [0 #f "fmult"]
:type [1 #f "ftype"]
:- [0 #f "fsubt"]
:/ [0 #f "fdivd"]
:gcd [0 #f "fgcd"]
:lcm [0 #f "flcm"]
:ceil [1 #f "fceil"]
:floor [1 #f "ffloor"]
:truncate [1 #f "ftruncate"]
:round [1 #f "fround"]
:inexact->exact [1 #f "fin2ex"]
:eq? [2 #f "eqp"]
:< [0 #f "flt"] 
:> [0 #f "fgt"] 
:<= [0 #f "flte"]
:>= [0 #f "fgte"]
:= [0 #f "fnumeq"]
:quotient [2 #f "fquotient"]
:modulo [2 #f "fmodulo"]
:remainder [2 #f "fremainder"]
:& [2 #f "fbitand"]
:| [2 #f "fbitor"]
:^ [2 #f "fbitxor"]
:~ [2 #f "fbitnot"]
:make-vector [0 #f "fmkvector"]
:make-string [0 #f "fmakestring"]
:append [0 #f "fappend"]
:first [0 #f "ffirst"]
:rest [0 #f "frest"]
:ccons [0 #f "fccons"] 
:nth [0 #f "fnth"]
:keys #t 
:partial-key? #t
:cset! [0 #f "fcset"]
:string [0 #f "fstring"]
:empty? [1 #f "fempty"] 
:gensym [0 #f "gensym"] 
:imag-part [1 #f "fimag_part"]
:real-part [1 #f "freal_part"] 
:make-rectangular [2 #f "fmake_rect"]
:make-polar [2 #f "fmake_pole"]
:magnitude [1 #f "fmag"]
:argument [1 #f "fimag_part"]
:conjugate! [1 #f "fconjugate_bang"] 
:conjugate [1 #f "fconjugate"]
:polar->rectangular [1 #f "fpol2rect"]
:rectangular->polar [1 #f "frect2pol"]
:sin [1 #f "fsin"]
:cos [1 #f "fcos"]
:tan [1 #f "ftan"]
:asin [1 #f "fasin"]
:acos [1 #f "facos"]
:atan [1 #f "fatan"]
:atan2 [2 #f "fatan2"]
:cosh [1 #f "fcosh"]
:sinh [1 #f "fsinh"]
:tanh [1 #f "ftanh"]
:exp [1 #f "fexp"]
:ln [1 #f "fln"]
:abs [1 #f "fnabs"]
:sqrt [1 #f "fsqrt"]
:exp2 [1 #f "fexp2"]
:expm1 [1 #f "fexpm1"]
:log2 [1 #f "flog2"]
:log10 [1 #f "flog10"]
:<< [2 #f "fbitshl"]
:>> [2 #f "fbitshr"]
:string-append [0 #f "fstringappend"]
;:apply #t
:assq [2 #f "assq"]
;:defrec #t
;:set-rec! #t
:dict [0 #f "fdict"]
:make-dict #t
:dict-has? [2 #f "fdicthas"]
:coerce [2 #f "fcoerce"]
:error [1 #f "ferror"]
:cupdate [3 #f "fcupdate"]
:cslice [3 #f "fcslice"]
:tconc! [2 #f "tconc"]
:make-tconc #t
:tconc-list #t
:tconc->pair #t
:tconc-splice! [2 #f "tconc_splice"]
:eval [1 #f "__seval"]
;:meta! #t
})

(def *prim-proc* {
 :display [0 "f_princ"]
 :newline [0 "newline"]
 :read [0 "f_read"]
 :write [0 "f_write"]
 :format [0 "format"]
 :read-char #t
 :write-char #t
 :read-buffer #t
 :write-buffer #t
 :read-string #t
 :write-string #t
 })

(def *prim-syntax* {
 :or #t
 :and #t
 :not #t
 :set! #t
 :if #t
 :cond #t
 :begin #t
 :let #t
 })

(define (show x) 
    (display "show: ")
    (display x)
    (newline)
    (display (type x))
    (newline) 
    x)

(def (string-join l ij)
  (if (null? (cdr l))
        (car l)
        (string-append (car l) ij (string-join (cdr l) ij))))

(def (gen-number x)
    (cond
        (integer? x) (format "makeinteger(~n)" x)
        (rational? x) (format "makerational(~n,~n)" (numerator x) (denomenator x))
        (real? x) (format "makereal(~n)" x)
        (complex? x) (format "makecomplex(~n,~n)" (real-part x) (imag-part x))
        else (error "NaN")))

(def (gen-string x)
    (format "makestring(\"~s\")" x))

(def (gen-symbol x)
    (format "makeatom(\"~s\")" x))

(def (gen-key x)
    (format "makekey(\"~s\")" x))

(def (gen-vector x)
    (let ((n (length x)) (p (coerce x 'pair)))
        (string-append (format "vector(~n," n) (string-join (map gen-literal p) ",") ")")))

(def (gen-pair x)
    (let ((n (length x)))
        (string-append (format "list(~n," n) (string-join (map gen-literal x) ",") ")")))

(def (gen-bool x)
    (if x
        "STRUE"
        "SFALSE"))

(def (gen-goal x)
    (if (eq? x #s)
        "SSUCC"
        "SUNSUCC"))

(def (gen-literal x)
    (cond
        (number? x) (gen-number x)
        (string? x) (gen-string x)
        (vector? x) (gen-vector x)
        (pair? x) (gen-pair x) ; really, need to tell what type of code to generate here...
        (dict? x) (gen-dict x)
        (eq? x '()) "SNIL"
        (symbol? x) (gen-symbol x)
        (bool? x) (gen-bool x)
        (goal? x) (gen-goal x)
        (key? x) (gen-key x)
        (void? x) "SVOID"
        else (error (format "unsupported data type for code generation: ~s" (type x)))))

(def (gen-dict d)
    (if (empty? (keys d)) ; have to update empty? to check keys automagically...
        "makedict()"
        (format "dict(~s)" (string-join)))) ; ... has to be the normal map dance

(def (cmung-name s)
    (def (imung s i thusfar)
        (cond
            (>= i (length s)) thusfar 
            (ascii-acceptable? (nth s i))  (imung s (+ i 1) (append thusfar (list (nth s i))))
            (mungable? (nth s i)) (imung s (+ i 1) (append thusfar (char-mung (nth s i))))
            else (imung s (+ i 1) thusfar)))
    (apply string (imung (coerce s 'string) 0 '())))

(def (char-mung c)
    (cond
        (eq? c #\:) (list #\_)
        (eq? c #\@) (list #\_ #\a #\t #\_)
        (eq? c #\%) (list #\_ #\p #\e #\r #\c #\e #\n #\t #\_)
        (eq? c #\=) (list #\_ #\e #\q #\u #\a #\l #\_)
        (eq? c #\>) (list #\_ #\m #\o #\r #\e #\_)
        (eq? c #\<) (list #\_ #\l #\e #\s #\s #\_)
        (eq? c #\.) (list #\_)
        (eq? c #\-) (list #\_)
        (eq? c #\?) (list #\_ #\p)))

(def (mungable? c)
    (or
        (eq? c #\:)
        (eq? c #\%)
        (eq? c #\@)
        (eq? c #\=)
        (eq? c #\?)
        (eq? c #\-)
        (eq? c #\>)
        (eq? c #\<)
        (eq? c #\.)))

(def (ascii-acceptable? c)
    (or
        (and (char->=? c #\a) (char-<=? c #\z))
        (and (char->=? c #\A) (char-<=? c #\Z))
        (and (char->=? c #\0) (char-<=? c #\9))
        (eq? c #\_)))

(def (tail-call? name code)
    "walk through the code of proc, and check if it calls itself; return #t if:
    - a bottom if has a call in either it's <then> or <else> suite
    - a bottom begin has a self-call in the tail\n"
    (if (pair? code)
        (cond
            (eq? (car code) 'if)
                (if (tail-call? name (caddr code))
                    #t
                    (tail-call? name (cadddr code)))
            (eq? (car code) 'begin)
                (tail-call? name (nth code (- (length code) 1)))
            (eq? (car code) 'let)
                (tail-call? name (nth code (- (length code) 1)))
            (eq? (car code) 'with)
                (tail-call? name (nth code (- (length code) 1)))
            (eq? (car code) 'fn)
                (tail-call? name (nth code (- (length code) 1)))
            (eq? (car code) 'cond) 
                (if (tail-call? name (caddr code))
                    #t
                    (if (eq? (cdr code) '())
                        #f
                        (tail-call? name (cons 'cond (cdddr code)))))
            else (eq? (car code) name))
        #f))

(def (rewrite-tail-cond name params lstate state code auxvs)
    "rewrite a cond form in the tail position, using inline-if (rather than nested ones!)
     parameters:
     - name : the function name we're looking for
     - params: the parameters to this funciton
     - lstate: the variable name used in if blocks
     - state: the variable used as while-loop sentry
     - code: the code of this variable
     - auxvs: auxillary variables
    "
    (let ((<cond> (car code))
          (<then> (cadr code))
          (<else> (cddr code)))
        (if (eq? lstate '()) ; should be initial state
            (with nlstate (gensym 'condit)
                (string-append 
                    (format "SExp *~a = nil;~%" nlstate)
                    (rewrite-tail-cond name params nlstate state code auxvs)))
            (if (eq? code '())
                (string-append
                    (format "~s = 0;" state)
                    (gen-code '(set! ret #f)))
                (if (eq? <cond> 'else)
                    (if (tail-call? name <then>)
                        (rewrite-tail-call name params state <then> auxvs)
                        (string-append 
                            (format "~s = 0;" state)
                            (wrap-gen-code (cadr code))))
                    (if (tail-call? name <then>)
                        (format "~s = ~a;
    if(~s == nil || ~s->type == NIL || ((~s->type == BOOL || ~s->type == GOAL) && ~s->object.c))
    {
        ~s
    }
    else
    {
        ~s
    }~%" lstate (gen-code <cond>) lstate lstate lstate lstate lstate
       (rewrite-tail-call name params state <then> auxvs)  ;; this is the tail call; call rewrite-tail-call here!
       (rewrite-tail-cond name params lstate state <else> auxvs))
                        (format "~s = ~a;
    if(~s == nil || ~s->TYPE == NIL || ((~s->type == BOOL || ~s->type == GOAL) && ~s->object.c))
    {
        ~s = 0;
        ~s
    }
    else
    {
        ~s
    }~%" lstate (gen-code <cond>) lstate lstate lstate lstate lstate state 
         (wrap-gen-code <then>)
         (rewrite-tail-cond name params lstate state <else> auxvs))))))))
        ; actually, I need to call tail-call? here for each <else> datum, since if it
        ; isn't a tail call, we want to set the state to 0
        ; rewrite-tail-call falls into a simple gen-code if no rewrite rules match;
        ; maybe this can be used? It's a bit expensive to rewrite something if it isn't
        ; a tail call. Need to see what can be done here...

(def (generate-aux-vars l)
    " generate auxillary variable names from a list of parameters.
      Parameters:
       - l: list containing name of parameters to be used as argument to gensym
    "
    (display "l == ")
    (display l)
    (newline)
    (map (fn (x) (coerce (gensym x) 'string)) l))

(def (rewrite-tail-call name params state code auxvs )
    "rewrite-tail-call: take code in the tail position, and rewrite it to be a simple jump, walking
     through syntax (cond,if,begin,let,with) to find the final call. 
     Parameters:
      - name: the function name we're looking for
      - params: parameters to the function
      - state: the state variable used in rewriting
      - code: the body of the function
      - auxvs: auxillary variables, used to avoid clobbering our parameters on assignment
     "
    (cond
        (not (pair? code)) (gen-code code)
        (eq? (car code) 'cond) (rewrite-tail-cond name params '() state (cdr code) auxvs)
        (eq? (car code) 'if)
            (with <cond> (gen-code (cadr code))
                (if (tail-call? name (caddr code)) ; does the tail call happen in the <then> portion or the <else> portion?
                    (let ((<then> (rewrite-tail-call name params state (caddr code) auxvs))
                        (<else> (rewrite-tail-call name params state (cadddr code) auxvs))
						(<it> (gensym 'it)))
                        (format "SExp *~s = ~s;~%
		if(~s == nil || ~s->type == NIL || ((~s->type == BOOL || ~s->type == GOAL) && ~s->object.c))
        {
                ~s
        }
        else
        {
                ~s = 0;
                ret = ~s;
        }~%" <it> <cond> <it> <it> <it> <it> <it> <then> state <else>))
					 (let ((<then> (rewrite-tail-call name params state (caddr code) auxvs))
						(<else> (rewrite-tail-call name params state (cadddr code) auxvs))
						(<it> (gensym 'it)))
                        (format "SExp *~s = ~s;~%
        if(~s == nil || ~s->type == NIL || ((~s->type == BOOL || ~s->type == GOAL) && ~s->object.c))
        {
                ~s = 0;
                ret = ~s;
        }
        else
        {
                ~s
        }~%" <it> <cond> <it> <it> <it> <it> <it> state <then> <else>))))
                (eq? (car code) 'begin)
                        (rewrite-tail-call name params (nth code (- (length code) 1)) auxvs)
                (eq? (car code) name)
                     (string-append 
                       (string-join ; assign auxillary variables to avoid clobbering params
                         (map 
                           (fn (x) 
                               (format "~s = ~s" (car x) (gen-code (cadr x)))) 
                           (zip auxvs (cdr code))) 
                         ";\n") 
                       ";\n"
                       (string-join ; assing params the values of their respective auxillaries
                         (map 
                           (fn (x) 
                               (format "~s = ~s" (coerce (car x) 'string) (cadr x))) 
                           (zip params auxvs)) 
                         ";\n") 
                       ";\n")
                else (gen-code code)))

(def (lift-lambda name code)
    " creates a function in C with C-safe name and generates code for the lambda's body.
      Parameters:
       - name: the function's name, which will be fed to cmung-name
       - code: the rest of the lambda
    "
	(let ((fixname (cmung-name name)))
        (cset! *fnmung* name fixname)
        (cset! *fnarit* name (length (car code)))
        (let ((body (gen-begin (cdr code))))
            (if (= (length (car code)) 0)
                (format "SExp *~%~s()~%{~%\tSExp *ret = nil;\n\t~s\n\treturn ret;\n}\n"
                fixname body)
	            (format "SExp *~%~s(~s)\n{\n\tSExp *ret = nil;\n\t~s \n\treturn ret;\n}\n" 
                    fixname 
                    (string-join 
                        (map 
                            (fn (x) (format "SExp *~a" x)) 
                            (car code)) 
                        ",") 
             body)))))

(def (lift-tail-lambda name code)
	"lift-tail-lambda is for when check-tail-call returns #t; basically, this generates a while loop version of the same lambda"
	(let ((state (gensym 's))
	      (fixname (cmung-name name))
	      (auxvs (generate-aux-vars (car code))))
	 (cset! *fnmung* name fixname)
	 (cset! *fnarit* name (length (car code)))
	 (format "SExp *~%~s(~s)\n{\n\tSExp *ret = nil, ~s;\n\tint ~s = 1;\n\twhile(~s)\n\t{\n\t\t\n~s\n\t}\n\treturn ret;\n}\n" 
	  	fixname 
		(string-join (map (fn (x) (format "SExp *~a" x)) (car code)) ",") 
        (string-join (map (fn (x) (format "*~a = nil" x)) auxvs) ",")
		state 
		state 
		(string-join (list 
                       (gen-begin (cslice code 1 (- (length code) 1))) ;; start at one to slice off the args
		               (rewrite-tail-call name (car code) state (nth code (- (length code) 1)) auxvs)) 
                     "\n"))))

; rather than lift each lambda passed to primitive HOFs like map, we should lift the entire
; HOF form, remove the anonymous lambda, and rewrite the whole thing to be a tail-recursive fn.
; if C supported block expressions in toto (not in specific compilers), I could just rewrite this
; to a "block while", and rely on lexical scope in whiles + returning a result
; Also, I should look into how to make this more general: reduce, filter, &c are going to
; be pretty damn similar, as is map-vector, map-string, map-apply, &c.
; It would be great if this were actually just syntax; need to implement syntax-rules stat,
; to simplify these types of tasks, since this could be rewritten to a simple tail-recursive
; lambda & lifted with the normal tail-lambda, rather than hoisted in this fashion (and that
; goes for a good portion of code here). 
(def (lift-map code)
	(let ((name (coerce (gensym 'map) 'string))
          (header '())
          (footer (format "\nreturn mret;\n}\n"))
          (body '()))
     (set! header (format "SExp *\n~s(SExp *lst)\n{\n" name))
     ;; generate the C function skeleton
     ;; this should be placed in *ooblam*
	 (if (eq? (caadr code) 'fn) ; anonymous lambda or not
        ;; insert the lambda's body directly into the while loop
        (set! body (string-append
            "SExp *mret = SNIL, *ret = SNIL, *head = SNIL,"
            (coerce (car (cadadr x)) 'string)
            " = nil;\n\tif(lst == nil || lst == SNIL)\n\t\treturn SNIL;"
            "\nmret = cons(SNIL,SNIL); head = mret;\n"
            "while(lst != SNIL)\n{\n"
            (coerce (car (cadadr x)) 'string)
            " = car(lst);\n"
            (gen-begin (cddadr x))
            "\nmcar(head) = ret;\n"
            "lst = cdr(lst);\n"
            "if(lst == SNIL)\n\tbreak;\n"
            "mcdr(head) = cons(SNIL,SNIL);\n"
            "head = mcdr(head);\n}\n"
            "return mret;\n}"))
	  	 ;; just place a call to ret for each proc iteration
         (with tmp (coerce (gensym 'tmp) 'string)
            (set! body (string-append
               "SExp *mret = SNIL, *head = SNIL, *" 
               tmp
               " = SNIL;\n"
               "if(lst == nil || lst == SNIL)\n return mret;\n"
               "mret = cons(SNIL,SNIL);\nhead = mret;\n"
               "while(lst != SNIL)\n{\n"
               tmp
               " = car(lst); mcar(head) = "
               (cmung proc) "(" tmp ");"
               "lst = cdr(lst);"
               "if(lst == SNIL)\nbreak;\n"
               "mcdr(head) = cons(SNIL,SNIL);\nhead = mcdr(head);\n")))
     (cset! *ooblam* name (string-append header body footer)) ; add the definition to ooblam
     (format "~s(~s)" name (gen-code (caddr code))))))

; from TSPL:
; for-each is similar to map except that for-each does not create
; and return a list of the resulting values, and for-each
; guarantees to perform the applications in sequence over the
; lists from left to right.
; -> while loop :D
; Doesn't process more than one list at a time, which should be 
; fixed
(def (gen-foreach code)
    "generates a while-loop from a foreach, using empty? as the 
     loop test" 
#f)

(define (gen-foreach-proc code)
    " generates a while-loop from a foreach-proc loop, using empty?
      as the loop test, but this will convert loops such as:
        (foreach-proc (fn (x) (display x) (newline)) '(1 2 3 4 5))
      into a normal foreach loop:
        SExp *v123 = list(5,makeinteger(1)...);
        while(emptyp(v123) != env->sfalse){
            x = f_first(v123);
            v123 = f_rest(v123);
            f_display(x);
            f_newline();
        }
      (i.e. it will not generate a lifted anonymous function, and 
       will be nearly identical to the normal foreach loop)
      whereas a loop such as
        (foreach-proc myfun '(1 2 3 4 5))
      will be translated into:
        SExp *v124 = list(5,...);
        SExp *v125 = env->snil;
        while(emptyp(v124) != env->sfalse) {
           v125 = f_first(v124);
            v124 = f_rest(v124);
            myfun(v125);
        }
    "
    (display "within gen-foreach\n")
    (display "code == ")
    (display code)
    (newline) 
    (let ((proc (cadr code))
          (collection (caddr code))
          (col-var (coerce (gensym) 'string))
          (anon-var (coerce (gensym) 'string)))

        (if (and (pair? proc) (or (eq? (car proc) 'fn) (eq? (car proc) 'lambda)))
            (string-append
                "SExp *" (show col-var) " = " (show (gen-code collection)) ";\n"
                "while(f_emptyp(" col-var ") != e->snil) {\n"
                "SExp *" (coerce (caadr proc) 'string) " = f_first(" col-var ");\n"
                col-var " = f_rest(" col-var ");\n"
                (show (gen-begin (cddr proc)))
                "\n}\n")
            (string-append
                "SExp *" col-var " = " (gen-code collection) ","
                " *" anon-var " = e->snil;\n"
                "while(f_emptyp(" col-var ") != e->snil) {\n"
                anon-var " = f_first(" col-var ");\n"
                col-var " = f_rest(" col-var ");\n"
                proc "(" anon-var ");\n"
                "\n}\n"))))

(def (defined-lambda? name)
	(dict-has? *fnmung* name))

(def (call-lambda name args)
 (if (= (length args) (nth *fnarit* name))
    (if (= (length args) 0)
        (format "~s()" (nth *fnmung* name))
        (format "~s(~s)" (nth *fnmung* name) (string-join (map (fn (x) (gen-code x)) args) ",")))
    (error (format "incorrect arity for ~S~%" (coerce name 'string)))))

; need to change this:
;  - check if <then> or <else> is a (begin ...)
;  - if not, say ret = (gen-code ...)
;  - if so, do nothing (and make gen-begin set ret = final code...)
(def (gen-if args)
	     (let ((<cond> (gen-code (car args)))
		   (<then> (wrap-gen-code (cadr args))) 
		   (<else> (wrap-gen-code (caddr args))) 
		   (<it> (gensym 'it)))
	      (format "SExp *~s = ~s;~%
	       if(~s == nil || ~s->type == NIL || ((~s->type == BOOL || ~s->type == GOAL) && ~s->object.c))
	       {
	       	 ~s
		}
		else
		{
			~s
		}~%" <it> <cond> <it> <it> <it> <it> <it> <then> <else>)))

(def (gen-cond args base)
    (let ((<cond> (car args))
          (<then> (cadr args))
          (<else> (cddr args)))
     (if (eq? <cond> 'else)
      (wrap-gen-code <then>)
      (if (eq? base '())
       (with <it> (gensym 'it)
        (format "SExp *~s = ~s;~%
       if(~s == nil || ~s->type == NIL || ((~s->type == BOOL || ~s->type == GOAL) && ~s->object.c))
       {
        ~s
       }
       else
       {
        ~s
       }~%" <it> (gen-code <cond>) <it> <it> <it> <it> <it> (wrap-gen-code <then>) (gen-cond <else> <it>)))
       (format "~s = ~s;~%
        if(~s == nil || ~s->type == NIL || ((~s->type == BOOL || ~s->type == GOAL) && ~s->object.c))
        {
         ~s
         }
         else
         {
          ~s
          }~%" base (gen-code <cond>) base base base base base (wrap-gen-code <then>) (gen-cond <else> base))))))

(def (gen-set! sym code)
    (format "~a = ~s" sym (gen-code code)))

(def (gen-let params code)
    "Generate C-code equivalent to a let; I wonder if we need to support lifting let blocks into
     top-level functions, so as to support (set! c (let ...)). Also, (let loop ...) should
     be converted into a tail while loop, but I don't know if it should do so as a simple
     loop rewrite or as a call to tail-lambda or the like.
     "
    (string-append
        "SExp "
        (string-join
            (map
                (fn (x)
                    (string-append
                        "*"
                        (coerce (gensym (car x)) 'string)
                        " = "
                        (gen-code (cadr x))))
                params)
            ", ")
        ";\n"
        (gen-begin code)))

(def (ep-syntax-expand synobj) #f) ; E' syntax expansion. Use this instead of Vesta's, since Vesta's in currently incomplete

(def (primitive-syntax? o)
	(dict-has? *prim-syntax* o))

(def (wrap-gen-code body)
    "a simple wrapper around gen-code, that also helps to alleviate the mess of inline-if's all over\n"
    (if (pair? body)
     (if (primitive-syntax? (car body))
      (gen-code body)
      (format "ret = ~s;~%" (gen-code body)))
     (format "ret = ~s;~%" (gen-code body))))

(def (gen-code x)
	(if (pair? x) 
		(cond
			(or (eq? (car x) 'def) 
                (eq? (car x) 'define))
                (cond
                    (symbol? (car (cdr x)))
					    (if (not (pair? (car (cdr (cdr x)))))
						    (format "SExp *~s = ~s;" (cmung-name (car (cdr x))) (gen-literal (car (cdr (cdr x)))))
						    (if (not (eq? (caaddr x) 'fn))
							      (format "SExp *~s = ~s;" (cmung-name (car (cdr x))) (gen-code (car (cdr (cdr x)))))
                                  (if (tail-call? (cadr x) (caddr x))
							        (lift-tail-lambda (cadr x) (cdaddr x))
							        (lift-lambda (cadr x) (cdaddr x)))))
                     (pair? (car (cdr x))) ; (def (foo x) ...)
                        (if (tail-call? (caadr x) (nth x (- (length x) 1)))
                            (lift-tail-lambda (caadr x) (cons (cdadr x) (cddr x)))
                            (lift-lambda (caadr x) (cons (cdadr x) (cddr x))))
                     else (error "def's first argument *must* be SYMBOL | PAIR"))
            (eq? (car x) 'set!) (gen-set! (cadr x) (caddr x))
			(eq? (car x) 'load) #t
			(eq? (car x) 'import) #t
			(eq? (car x) 'use) #t
			(eq? (car x) 'from) #t
			(eq? (car x) 'let) (if (symbol? (cadr x))
                                    (lift-named-let (cdr x))
                                    (gen-let (cadr x) (cddr x)))  ; let should be a top-level form, rather than expand to lambda(s)
            (eq? (car x) 'let*) (gen-let (cadr x) (cddr x)) ; same here
            (eq? (car x) 'letrec) (gen-letrec (cadr x) (cddr x)) ; letrecs should be lifted with generated names 
			(eq? (car x) 'with) 
                (format "SExp *~s = ~s;\n~s" (coerce (car (cdr x)) 'string) (gen-code (caddr x)) (gen-begin (cdddr x)))
            (eq? (car x) 'map)
                (lift-map x)
            (eq? (car x) 'map-apply)
                (lift-map-apply x)
            (eq? (car x) 'append-map)
                (lift-map-append x)
            (eq? (car x) 'foreach)
                (gen-foreach x)
            (eq? (car x) 'foreach-proc)
                (gen-foreach-proc x)
			(eq? (car x) 'quote) (gen-literal (cadr x))
			(eq? (car x) 'module) 'MODULE
			(eq? (car x) 'if) (gen-if (cdr x)) ; if & other primitive syntax needs to be handled here
			(eq? (car x) 'cond) (gen-cond (cdr x) '()) ; should be nearly identical to if, but with more else if's 
			(eq? (car x) 'begin) (gen-begin (cdr x))
			(eq? (car x) 'list) (format "list(~n,~s)" (length (cdr x)) (string-join (map gen-code (cdr x)) ","))
			(eq? (car x) 'vector) (format "vector(~n,~s)" (length (cdr x)) (string-join (map gen-code (cdr x)) ","))
			(eq? (car x) 'string) (format "list(~n,~s)" (length (cdr x)) (string-join (map gen-code (cdr x)) ","))
            (eq? (car x) 'apply) ;; basically, calling apply means don't build a list, just run the fn on the operand
                (if (primitive-form? (cadr x))
                  (format "~s(~s,tl_env)" (cadr x)  (gen-code (caddr x)))
                  (format "~s(~s)" (cadr x)  (gen-code (caddr x))))
			(pair? (car x)) #t
			(defined-lambda? (car x)) (call-lambda (car x) (cdr x))
			(primitive-form? (car x)) (gen-primitive x) 
			(primitive-proc? (car x)) (call-prim-proc x) ;display & friends
			(primitive-syntax? (car x)) (gen-code (ep-syntax-expand x))
            else                (call-lambda (car x) (cdr x)))
		(if (symbol? x)
		 (coerce x 'string)
		 (gen-literal x))))

(def (foreach-expression proc in)
	(with r (read in)
	 (if (eq? r #e)
		#v
	 	(begin
			(proc r)
		    (foreach-expression proc in)))))

; Awesome things to do:
; - call graphs
; - function instrumentation (for debugging)
; - static checks of availability
; - useful lambda lifting 
; - inclusion of types & typed-syntax expansion
; - c-lambdas, c-macros, c-syntax
(def (header-out p) 
		 "output C headers & any top-level structure to output file"
		 (def BASE (gensym 'BASE))
		 ; would be nice to store which of these headers is needed by which
		 ; primitives, and only include accordingly.
		 (display "/* this code was generated by E', the restricted Digamma compiler */\n" p)
		 (foreach-proc (fn (x) (display x p) (newline p)) '("#include <stdio.h>"
"#include <stdlib.h>"
"#include <string.h>"
"#include <unistd.h>"
"#include <gc.h>"
"#include <math.h>"
"#include <sys/param.h>"
"#include <fcntl.h>"
"#include <sys/time.h>"
"#include <sys/types.h>"
"#include <sys/stat.h>"
"#include <sys/wait.h>"
"#include <sys/socket.h>"
"#include <netdb.h>"
"#include <netinet/in.h>"
"#include <arpa/inet.h>"
"#include <dirent.h>"
"#include <signal.h>"
"#include <errno.h>"
"#include <stdarg.h>"
"#include \"vesta.h\""
"#define nil NULL"
"#define STRUE tl_env->strue"
"#define SFALSE tl_env->sfalse"
"#define SSUCC tl_env->ssucc"
"#define SUNSUCC tl_env->sunsucc"
"#define SNIL tl_env->snil"
"static Symbol *tl_env = nil;")))

(def (footer-out p)
		 "finalize C code to output file"
		 (display "\n}\n" p))

(def (gen-arity arity)
     "Generates a list of SExp parameters to a prototype"
     (if (<= arity 0)
       '()
       (cons "SExp *" (gen-arity (- arity 1)))))

(def (write-prototypes out)
     " iterate over *fnarit*, writing the corresponding *fnmung* name
       and prototype to out"
    (foreach-proc
      (fn (k)
          (with name (nth *fnmung* k)
                (if (= (nth *fnarit* k) 0)
                  (display (format "SExp *~s();~%" name) out)
                  (display (format "SExp *~s(~s);~%" name 
                                 (string-join (gen-arity (nth *fnarit* k)) ", ")) 
                         out))))
      (keys *fnarit*)))

(define (set-arity! code)
    (if (and (pair? code) (or (eq? (car code) 'def) (eq? (car code) 'define)))
        (if (pair? (cadr code))
            (begin
                (cset! *fnarit* (caadr code) (length (cdadr code)))
                (cset! *fnmung* (caadr code) (cmung-name (caadr code))))
            (if (and (pair? (caddr code)) (or (eq? (caaddr code) 'fn) (eq? (caaddr code) 'lambda)))
                (begin
                    (cset! *fnarit* (cadr code) (length (cadr (caddr code))))
                    (cset! *fnmung* (cadr code) (cmung-name (cadr code))))
                #v))
        #v))
        
(def (eprime i o name)
	   "Main code output"
	   (let ((in (open i :read)) 
		    (out (open o :write))
            (defs '())
            (codes '()))
	    (header-out out)
        ;; add another level of indirection:
        ;; foreach expression, collect all 
        ;; functions *before* attempting to 
        ;; generate C code, so that we don't have
        ;; the issues with missing functions:
        ;; (define (a x) (b (- x 1)))
        ;; (define (b z) (if (> z 15) (a z) z))
        ;; this *should* work, but it currently does not,
        ;; because a has no notion that b exists afterward.
        ;; collecting all definitions first, *then* processing
        ;; them would ease this (as did the generation of C
        ;; function prototypes helped not having to explicitly
        ;; order code so that the generated code matched). 
         
	    (foreach-expression (fn (e)
                                (set-arity! e)
                                (set! codes (append codes (list e))))
                            in)
        (foreach-proc (fn (c)
                        (with cde (gen-code c)
                            (set! defs (append defs (list cde)))))
                    codes)
        (newline out)
        (write-prototypes out)
        (newline out)
        (foreach-proc (fn (d) (display d out)) defs) ; write actual definitions to file
	    (display (format "void~%~s()~%{~%\ttl_env = init_env(0);~%" name) out)
	    (footer-out out)
	    (close in)
	    (close out)))

; The basic system is this:
; - read form from file
; - call syntax-expand
; - call macro-expand
; - call gen-code on what's left
; - loop to the first item until #e
; Primitive handling functions for Eris
; also defines *primitives* a global dict of all internal forms
; TODO:
;  - make *primitives* result a vector: [arity syntax? internal-c-function]
;    + arity is the number of parameters to the C function [0 means just pass a list]
;    + syntax? means if this form should have it's arguments eval'd before applying it
;    + internal-c-function is the low-level C function that backs this primitive in Vesta's runtime
; zlib/png licensed Copyright 2010 Stefan Edwards 

(def (primitive-form? x)
	(dict-has? *primitives* x))

(def (syntax-form? f)
	(if (pair? f)
	 (cond
	  (eq? (car f) 'if) #t
	  (eq? (car f) 'cond) #t
	  (eq? (car f) 'def) #t
	  (eq? (car f) 'set!) #t
	  else #f)
	 #f))

(def (gen-begin l)
		(if (eq? (cdr l) '())
		 (if (syntax-form? (car l))
		  (string-append (gen-code (car l)) ";\n") 
		  (string-append "ret = " (show (gen-code (show (car l)))) ";\n"))
		 (string-append (show (gen-code (show (car l)))) ";\n" (show (gen-begin (cdr l))))))

(def (gen-primitive x)
    (display "within gen-primitive\n")
    (display x)
    (newline)
	(let ((f (nth *primitives* (car x))) (args (cdr x)))
	 (if (= (nth f 0) 0) ; arity
        (if (= (length args) 0)
          (format "~s(snil)" (nth f 2))
	      (format "~s(list(~n,~s))" (nth f 2) (length args) (string-join (map gen-code args) ",")))
	  (if (= (length args) (nth f 0))
	   (format "~s(~s)" (nth f 2) (string-join (map gen-code args) ","))
	   (error (format "enyalios: incorrect number of arguments to ~s" (nth f 2)))))))

(def (primitive-proc? x)
	(dict-has? *prim-proc* x))

(def (call-prim-proc x)
    (let ((f (nth *prim-proc* (car x))) (args (cdr x)))
        (if (= (length args) 0)
            (format "~s(SNIL,tl_env)" (nth f 1)) 
            (format "~s(list(~n,~s),tl_env)" (nth f 1) (length args) (string-join (map gen-code args) ",")))))

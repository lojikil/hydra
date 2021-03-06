;; A simple stack-based VM for Digamma
;; Meant to support both interpretation in Vesta as
;; well as compilation with E'
;; copyright 2011 Stefan Edwards; please see the LICENSE
;; file for details

;; TODO:
;; - variable arity functions should have some easier method for generating
;;   their code, so as to reduce boiler plate. List, <, +, &c. have a lot of 
;;   repeated code in the code-generation side, that could be handled if they
;;   could simply be marked as "variable arity" to the compiler. This arity marker
;;   could be Z+ (integer >= 1) or -1 for true "variable arity", and they could be 
;;   given handlers for it. Simple with a syntax for "generate-variable-arity-primitive"
;; - DONE: good compilation mechanism for typhon@eval
;; - DONE: method for typhon@vm to manage things like (cons (car (cons 1 2)) (cdr (1 2)))
;;   which it cannot currently do because we need to rotate the stack (wait, do we?)
;;   (cons (car (cons 1 '())) (cdr 4 '())):
;;   (4)   ;; nil
;;   (3 4) ;; load 4
;;   (2)   ;; cons
;;   (1)   ;; cdr
;;   (4)   ;; nil
;;   (3 1) ;; load 1
;;   (2)   ;; cons
;;   (0)   ;; car
;;   (2)   ;; cons
;;   works, so this isn't blocked. \o/
;; - lambda lifting: it would be nice if lambdas were lifted to avoid allocation as
;;   much as possible
;; - define-instruction syntax that can be used to populate typhon@vm as well as 
;;   clean up redundancies in code (like manual calls to typhon@vm in each instruction)
;; - define a clean method of boxed representations of types, one that can be used from
;;   Vesta or E'. Not sure if this is a decent use of time here, in a VM, since that 
;;   should be a function of the run time, not the interpreter (we wouldn't want Ceres
;;   & Hydra to have different representations, for instance). Still, it would be nice
;;   if errors & other types can be simply encoded '(error "error"); SRFI-9, esp. if it
;;   has E' support, might be a good option (need to unbox types in E' though, for the
;;   most efficient representation, as well as unions).
;; - DONE (via structs): Make all instructions support (type . value) pairs, so as to avoid a situation where
;;   the user enters the code (0 (list 1 2 3)) and recieves the value 1 back, because:
;;   (load 0)
;;   (call) ;; call integer 0, since integers -> primitives, this can add some weird behavior.
;; - DONE: Just noticed that the way the CALL operand is implemented, the stack will no longer
;;   hold the parameters. Need to walk over the params to a lambda & bind variables from the
;;   stack before moving to running the code... 
;; - add a debug variable, so that you could ,debug-engine at the REPL, and I wouldn't have to comment/uncomment
;;   debug lines every time I wanted to check out what's going on underneath the hood
;; - DONE fix this:
;;      h; 0
;;      #<primitive-procedure 0>
;;   it should be:
;;      h; 0
;;      0
;;      h; car
;;      #<primitive-procedure 0>
;;
;; - DONE: I wonder if this should re-write to %define, so that I don't have
;; to do anything fancy with eval... There are three cases:
;; (define f literal)
;; (define f (fn (x) (+ x x)))
;; (define f (car '(1 2 3 4 5)))
;; the first two *can* be handled OOB by typhon@eval, but the third
;; cannot really be handled properly. It should be re-written to 
;; (load (1 2 3 4 5))
;; (car)
;; (load f)
;; (%define)
;; - SRFIs to be added: 22, 34, 35, 36, 57, 60, 89, 88 (already done, via Vesta's run time)
;; - SRFI list:
;; --  9: needs to be handled in a fashion similar to how we handle it in Enyalios...
;; --  6: String IO ports are already kinda sorta supported...
;; -- 22: handled by vesta's reader
;; -- 23: Need to have an error function that handles error generation
;; -- 34: Need to add watch list, even in face of continuations
;; -- 35: is this distinct from 23 really? do I want to generate conditions too? can include more info...
;; -- 36: IO conditions as distinct from SRFI-35?
;; -- 9/57: already support Racket/Bigloo style structs, no need to add others.
;; -- 60: just need to define the names I think instead of C-style operators
;; -- 88: handled via Vesta's reader
;; -- 89: need to handle positional arguments gracefully...
;; - Non-SRFI stuffs:
;; -- current-X-port, set-current-X-port!
;; -- Delimited continuations, and a fibre library atop them

;; Include files for C output
;; once SRFI-0 support is here, use that to make things a bit
;; nicer

(%include "murt.h" #f)

;; mini-prelude
;; should be removed once Enyalios supports load better...

(define-struct typhon-primitive (value min-arity max-arity))

;; just like primitives, but don't call reverse-append on them...
;; should just be a handful of primitives, but still
(define-struct typhon-antipole-primitive (value min-arity max-arity))
(define-struct typhon-syntax (name))

;; Add: fixed arity, N-arity, Nullary, & ranged primitives
;; remove the %-prefixed items, and tag structs w/ their operation
;; should include arity too, but for now...
(define-struct typhon-procedure (name min-arity max-arity)) 
(define-struct typhon-error (message))
;; keys & vals are just vectors with linear scan, like Clojure
(define-struct typhon-struct (name keys vals))
(define-struct typhon-dump (offset dump)) ;; almost a singleton...
(define-struct typhon-lambda (env code params))

(define-syntax caar () ((caar x) (car (car x))))
(define-syntax cadr () ((cadr x) (car (cdr x))))
(define-syntax cdar () ((cdar x) (cdr (car x))))
(define-syntax cddr () ((cddr x) (cdr (cdr x))))
(define-syntax caaar () ((caaar x) (car (car (car x)))))
(define-syntax caadr () ((caadr x) (car (car (cdr x)))))
(define-syntax cadar () ((cadar x) (car (cdr (car x)))))
(define-syntax caddr () ((caddr x) (car (cdr (cdr x)))))
(define-syntax cdaar () ((cdaar x) (cdr (car (car x)))))
(define-syntax cdadr () ((cdadr x) (cdr (car (cdr x)))))
(define-syntax cddar () ((cddar x) (cdr (cdr (car x)))))
(define-syntax cdddr () ((cdddr x) (cdr (cdr (cdr x)))))
(define-syntax caaaar () ((caaaar x) (car (car (car (car x))))))
(define-syntax caaadr () ((caaadr x) (car (car (car (cdr x))))))
(define-syntax caadar () ((caadar x) (car (car (cdr (car x))))))
(define-syntax caaddr () ((caaddr x) (car (car (cdr (cdr x))))))
(define-syntax cadaar () ((cadaar x) (car (cdr (car (car x))))))
(define-syntax cadadr () ((cadadr x) (car (cdr (car (cdr x))))))
(define-syntax caddar () ((caddar x) (car (cdr (cdr (car x))))))
(define-syntax cadddr () ((cadddr x) (car (cdr (cdr (cdr x))))))
(define-syntax cdaaar () ((cdaaar x) (cdr (car (car (car x))))))
(define-syntax cdaadr () ((cdaadr x) (cdr (car (car (cdr x))))))
(define-syntax cdadar () ((cdadar x) (cdr (car (cdr (car x))))))
(define-syntax cdaddr () ((cdaddr x) (cdr (car (cdr (cdr x))))))
(define-syntax cddaar () ((cddaar x) (cdr (cdr (car (car x))))))
(define-syntax cddadr () ((cddadr x) (cdr (cdr (car (cdr x))))))
(define-syntax cdddar () ((cdddar x) (cdr (cdr (cdr (car x))))))
(define-syntax cddddr () ((cddddr x) (cdr (cdr (cdr (cdr x))))))

(define-syntax null? () ((null? n) (eq? n '())))
(define-syntax pair? () ((pair? n) (type? n "PAIR")))
(define-syntax vector? () ((vector? n) (type? n "VECTOR")))
(define-syntax dict? () ((dict? n) (type? n "DICT")))
(define-syntax symbol? () ((symbol? n) (type? n "ATOM")))
(define-syntax key? () ((key? n) (type? n "KEY")))
(define-syntax number? () ((number? n) ((type? n "NUMBER"))))
(define-syntax string? () ((string? n) (type? n "STRING")))
(define-syntax bool? () ((bool? n) (type? n "BOOL")))
(define-syntax goal? () ((goal? n) (type? n "GOAL")))
(define-syntax zero? () ((zero? n) (= n 0)))
(define-syntax eof-object? () ((eof-object? n) (eq? n #e)))
(define-syntax void? () ((void? x) (eq? x #v)))

(define (zip xs ys)
	(if (null? xs)
		'()
		(cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys)))))

(define (foldl proc val lst)
    (cond
        (empty? lst) val
        else (foldl proc (proc val (first lst)) (rest lst))))

(define (foldr proc val lst)
    (cond
        (empty? lst) val
        else (proc (first lst) (foldr proc val (rest lst)))))

;; hacking around some Runtime stuff.
;; really could use a "define-inline" here for Enyalios.
;; would almost be an FExpr...

(define (flt lst)
    (cond
        (null? (cdr lst)) #t
        (< (car lst) (cadr lst)) (flt (cdr lst))
        else #f))

(define (flte lst)
    (cond
        (null? (cdr lst)) #t
        (<= (car lst) (cadr lst)) (flte (cdr lst))
        else #f))

(define (fgt lst)
    (cond
        (null? (cdr lst)) #t
        (> (car lst) (cadr lst)) (fgt (cdr lst))
        else #f))

(define (fgte lst)
    (cond
        (null? (cdr lst)) #t
        (>= (car lst) (cadr lst)) (fgte (cdr lst))
        else #f))

(define (fnumeq lst)
    (cond
        (null? (cdr lst)) #t
        (= (car lst) (cadr lst)) (fnumeq (cdr lst))
        else #f))

(define (filter proc coll) ; generic now, for map-able collections (string, vector, list)
    (cond
        (empty? coll) coll
        (proc (first coll)) (ccons (first coll) (filter proc (rest coll)))
        else (filter proc (rest coll))))

(define (srfi1-list-copy l)
    " really, should be included from SRFI-1, but this simply makes a copy
      of the spine of a pair
      "
    (if (null? l)
        l
        (cons (car l) (srfi1-list-copy (cdr l)))))

(define-syntax cadddar () ((cadddar x) (car (cdddar x))))

;; cheat: implement "frationalize" within hydra
;; work around until I get it working within C
(define (frationalize x y)
    #f)

;; naieve map, foreach, append-map
(define (map proc c)
    (if (empty? c)
        c
        (ccons (proc (first c)) (map proc (rest c)))))

(define (foreach proc c)
    (if (empty? c)
        #v
        (begin
            (proc (first c))
            (foreach proc (rest c)))))

(define (append-map f x)
    (if (null? x)
        x
        (append (f (car x)) (append-map f (cdr x)))))

(define (vector-equal? x y offset)
	(cond
		(>= offset (length x)) #t
		(equal? (nth x offset) (nth y offset)) (vector-equal? x y (+ offset 1))
		else #f))

(define (equal? x y)
	(cond
		(and (type? x "PAIR") (type? y "PAIR"))
			(if (equal? (car x) (car y))
				(equal? (cdr x) (cdr y))
				#f)
		(and (type? x "VECTOR") (type? y "VECTOR"))
			(if (= (length x) (length y))
				(vector-equal? x y 0)
				#f)
		(and (type? x "NUMBER") (type? y "NUMBER"))
			(= x y)
		else
			(eq? x y)))

(load "./experiments/sr.ss")
;; end mini-prelude.

;; get rid of the below; use the proper typed versions from
;; the SRFI-9 declarations above

(define-syntax typhon@instruction () 
    ((typhon@instruction c) (car c)))

(define-syntax typhon@operand ()
    ((typhon@operand c) (cadr c)))

(define-syntax typhon@lambda? ()  ((typhon@lambda? x)
    (and (pair? x) (eq? (car x) 'compiled-lambda))))

(define-syntax typhon@primitive? () ((typhon@primitive? x)
    (and (dict? x) (eq? (nth x 'type '()) 'primitive))))

(define-syntax typhon@syntax? () ((typhon@syntax? x)
    (and (dict? x) (eq? (nth x 'type '()) 'syntax))))

(define-syntax typhon@error? () ((typhon@error? x)
    (and (pair? x) (eq? (car x) 'error))))

(define-syntax typhon@continuation? () ((typhon@continuation? x)
    (and (pair? x) (eq? (car x) 'continuation))))

(define-syntax typhon@procedure? () ((typhon@procedure? x)
    (and (dict? x) (eq? (nth x 'type '()) 'procedure))))

(define-syntax typhon@usyntax? () ((typhon@usyntax? x)
    (and (pair? x) (eq? (car x) 'user-syntax)))) 

(define (typhon@umacro? x)
    #f)

;(define (primitive-value obj)
;    (if (dict? obj)
;        (nth obj 'value '())
;        obj))

(define (loop-set-env! env params vals locals lidx)
    (if (null? params)
        vals
        (let ((name (car params)))
            (dict-set! env name (car vals))
            (vector-set! locals lidx (car vals))
            (loop-set-env! env (cdr params) (cdr vals) locals (+ lidx 1)))))

(define (build-environment environment stack params locals)
    "Adds a new window to the environment, removes |params| items from the stack
     and binds those values in the new window. It returns a list of environment and
     the new stack."
    ;; rough match; doesn't take into account optional parameters.
    ;; would probably be better to have an inner function that iterates over
    ;; the parameters & returns those that match. It would then be easier to 
    ;; have optional parameters...
    ;(display "in build-environment; environment == ")
    ;(write environment)
    ;(display "\nstack == ")
    ;(write stack)
    ;(display "\nparams == ")
    ;(write params)
    ;(newline)
    (let ((ls (length stack)) (lp (length params)) (nu-env (make-dict)))
        (if (< ls lp)
            (error "non-optional parameters are not statisfied by stack items in build-environment")
            (if (= lp 0)
                (list (cons nu-env environment) (cdr stack) '())
                (let* ((nu-locals (make-vector lp '()))
                       (new-stack
                            (loop-set-env!
                                nu-env
                                params stack nu-locals 0)))
                    (list (cons nu-env environment) new-stack nu-locals))))))

(define (copy-code code ip offset)
    " copies the spine of code, but at ip & ip+1, insert %nop instructions
      instead, over-writing the call/cc & load-lambda instructions therein.
      "
    (cond
        (null? code) '()
        (= offset (- ip 1)) (append (list '(107) '(107)) (copy-code (cddr code) ip (+ offset 2)))
        else (append (list (car code)) (copy-code (cdr code) ip (+ offset 1)))))

(define (procedure-runner proc arity args env dump)
    (cond
        (eq? proc "display")
            (cond
                (= arity 2)
                    (display (car args) (cadr args))
                (= arity 1)
                    (display (car args))
                else
                    (error "Incorrect arity for procedure: display"))
        (eq? proc "write")
            (cond
                (= arity 2)
                    (write (car args) (cadr args))
                (= arity 1)
                    (write (car args))
                else
                    (error "Incorrect arity for procedure: write"))
        (eq? proc "newline")
            (cond
                (= arity 0)
                    (newline)
                (= arity 1)
                    (newline (car args))
                else
                    (error "Incorrect arity for procedure: newline"))
        (eq? proc "load")
            (cond
                (= arity 1)
                    (typhon@load (car args) env dump)
                (= arity 2)
                    (typhon@load (car args) (cadr args) dump)
                else
                    (error "Incorrect arity for procedure: load"))
        (eq? proc "read")
            (cond
                (= arity 0)
                    (read)
                (= arity 1)
                    (read (car args))
                else
                    (error "Incorrect arity for procedure: read"))
        (eq? proc "digamma-implementation")
            'typhon
        (eq? proc "sys/stat")
            (if (= arity 1)
                (sys/stat (car args))
                (error "Incorrect arity for procedure: sys/stat"))
        (eq? proc "sys/getenv")
            (if (= arity 1)
                (sys/getenv (car args))
                (error "Incorrect arity for procedure: sys/getenv"))
        (eq? proc "format")
            (if (= arity 1)
                (car args) ;; no need to call format for a 1-ary call...
                (cdr args)) ;; garbage for now
        else
            (error (format "unknown procedure \"~a\"" proc))))

(define (typhon@vm code code-len env ip stack locals dump offset)
     " process the actual instructions of a code object; the basic idea is that
       the user enters:
       h; (car (cdr (cons 1 (cons 2 '()))))
       which is compiled by typhon@eval into:
       (4)   ;; nil
       (3 2) ;; load 2
       (2)   ;; cons
       (3 1) ;; load 1
       (2)   ;; cons
       (1)   ;; cdr
       (0)   ;; car
       
       which typhon@vm can then interpret in a tail-call fashion.
       There might be better ways to store the actual VM codes themselves
       other than pairs of pairs (even vector of pairs would be more efficient really)
       and it might be worth it to add two collexion-neutral primitives, cappend & 
       cappend!, to the collexion API. Also, adding named-enumerations to the language,
       even if at the syntactic level, would be helpful. If (enumerate OPCAR OPCDR ...)
       even compiled to
       #define OPCAR 0
       #define OPCDR 1
       // ...
       
       It would be more useful, and the names could be used throughout (and checked!)."
     ;; if this is moved to an IP-based (instruction pointer)
     ;; system, it might be easier to do this using a named-let rather than iterating 
     ;; at the top level. E' should be able to lift named-lets pretty easily into whiles
     ;; and that would fit pretty well here. Also, moving to an inner-loop with named-let
     ;; means I can alliviate some of the duplication here; code & env are rarely modified
     ;; so it would also make sense to not have to pass them on every call. A lot of
     ;; duplication...
     ;; also, I would really like to have these all defined using a Scheme48-style
     ;; define-operator, since that would be much cleaner than what is seen below.
     ;; Syntax could expand the full list of operators in place here, and it would make
     ;; expanding the set of operators *much* easier than it currently is.
     ;(display "stack: ")
     ;(display stack)
     ;(newline)
     ;(display "code: ") 
     ;(display code)
     ;(newline)
     ;(display "ip: ")
     ;(display ip)
     ;(newline)
     ;(display "offset: ")
     ;(display offset)
     ;(newline)
     ;(display "====================\n")
     (cond
        (or (type? (car stack) "ERROR")
            (typhon-error? (car stack)))
            (car stack)
        (typhon-error? code)
            code
        (>= ip code-len)
            (if (= offset 0) ;; should switch dump to a struct...
                (car stack)
                (typhon@vm
                    (vector-ref dump (- offset 1))
                    (vector-ref dump (- offset 2))
                    (vector-ref dump (- offset 3))
                    (+ (vector-ref dump (- offset 4)) 1)
                    (cons (car stack) (vector-ref dump (- offset 5)))
                    (vector-ref dump (- offset 6))
                    dump
                    (- offset 6)))
        (typhon-error? (vector-ref code ip))
            (vector-ref code ip)
        else
         (let* ((c (vector-ref code ip))
                (instr (typhon@instruction c)))
              ;(display (format "current ip: ~n~%current instruction: " ip))
              ;(write (nth code ip))
              ;(display "\ncurrent stack: ")
              ;(write stack)
              ;(display "\ncurrent dump: ")
              ;(write dump)
              ;(display "\ncurrent code: ")
              ;(write code)
              ;(display "\ncurrent env: ")
              ;(write env)
              ;(display "\n=====\n")
              (case instr 
                    (0) ;; car
                        (typhon@vm
                                code
                                code-len
                                env
                                (+ ip 1)
                                (cons (car (car stack)) (cdr stack)) locals dump offset)
                    (1) ;; cdr
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (cdr (car stack)) (cdr stack)) locals dump offset)
                    (2) ;; cons
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (cons (car stack)
                                                (cadr stack))
                                       (cddr stack))
                                 locals
                                 dump offset)
                    (3) ;; load
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (typhon@operand c) stack)
                                 locals
                                 dump offset)
                    (4) ;; nil
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons '() stack)
                                 locals
                                 dump offset)
                    (5) ;; -
                        (let* ((top-of-stack (typhon@operand c))
                               (stack-offset top-of-stack)
                               (bottom-of-stack '())
                               (portion '())
                               (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret 0)
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret (- 0 (car stack)))
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (- (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin 
                                        (set! portion (cslice stack 0 top-of-stack))
                                        (set! ret
                                            (foldl (fn (x y) (- x y)) (car portion) (cdr portion)))
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                            (typhon@vm code code-len
                                     env
                                     (+ ip 1)
                                     (cons ret bottom-of-stack)
                                     locals
                                     dump offset))
                    (6) ;; +
                        ;; this would be perfect as:
                        ;; (cut
                        ;;      (cond
                        ;;          (= top-of-stack 0) 0
                        ;;          (= top-of-stack 1) (cadr stack)
                        ;;          (= top-of-stack 2) (+ (cadr stack) (caddr stack))
                        ;;          else (foldl + 0 (cslice stack 1 top-of-stack)))
                        ;;      (typhon@vm ...))
                        (let* ((top-of-stack (typhon@operand c))
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret 0)
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret (car stack))
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (+ (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin 
                                        (set! ret
                                            (foldl (fn (x y) (+ x y)) 0 (cslice stack 0 top-of-stack))) ;; optional len param to foldl?
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                            (typhon@vm code code-len
                                     env
                                     (+ ip 1)
                                     (cons ret bottom-of-stack)
                                     locals
                                     dump offset))
                    (7) ;; * 
                        (let ((top-of-stack (typhon@operand c))
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret 1)
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret (car stack))
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! bottom-of-stack (cddr stack))
                                        (set! ret (* (car stack) (cadr stack))))
                                else
                                    (begin 
                                        (set! ret
                                            (foldl (fn (x y) (* x y)) 1 (cslice stack 0 top-of-stack))) ;; optional len param to foldl?
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                            (typhon@vm code code-len
                                     env
                                     (+ ip 1)
                                     (cons ret bottom-of-stack)
                                     locals
                                     dump offset))
                    (8) ;; / 
                        (let ((top-of-stack (typhon@operand c))
                              (first-operand 0)
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret 1)
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret (car stack))
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (/ (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin
                                        (set! first-operand (car stack))
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))
                                        (set! ret
                                            (foldl (fn (x y) (/ x y)) first-operand (cslice stack 1 top-of-stack)))))
                            (typhon@vm code code-len
                                     env
                                     (+ ip 1)
                                     (cons ret bottom-of-stack)
                                     locals
                                     dump offset))
                    (9) ;;  < 
                        (let* ((top-of-stack (typhon@operand c))
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret (make-typhon-error "in correct arity for <"))
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret #t)
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (< (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin
                                        (set! ret (apply < (cslice stack 0 top-of-stack)))
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                                (typhon@vm code code-len
                                    env
                                    (+ ip 1)
                                    (cons ret bottom-of-stack)
                                    locals
                                    dump offset))
                    (10) ;; >
                        (let* ((top-of-stack (typhon@operand c))
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret (make-typhon-error "in correct arity for >"))
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret #t)
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (> (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin
                                        (set! ret (apply > (cslice stack 0 top-of-stack)))
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                                (typhon@vm code code-len
                                    env
                                    (+ ip 1)
                                    (cons ret bottom-of-stack)
                                    locals
                                    dump offset))
                    (11) ;; <= 
                        (let* ((top-of-stack (typhon@operand c))
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret (make-typhon-error "in correct arity for <="))
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret #t)
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (<= (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin
                                        (set! ret (apply <= (cslice stack 0 top-of-stack)))
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                                (typhon@vm code code-len
                                    env
                                    (+ ip 1)
                                    (cons ret bottom-of-stack)
                                    locals
                                    dump offset))
                    (12) ;; >= 
                        (let* ((top-of-stack (typhon@operand c))
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret (make-typhon-error "in correct arity for >="))
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret #t)
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (>= (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin
                                        (set! ret (apply >= (cslice stack 0 top-of-stack)))
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                                (typhon@vm code code-len
                                    env
                                    (+ ip 1)
                                    (cons ret bottom-of-stack)
                                    locals
                                    dump offset))
                    (13) ;; length
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (length (car stack)) (cdr stack)) locals dump offset)
                    (14) ;; exact?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (exact? (car stack)) (cdr stack)) locals dump offset)
                    (15) ;; inexact?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (inexact? (car stack)) (cdr stack)) locals dump offset)
                    (16) ;; procedure call
                    (let* ((arity (caddr c))
                           (args (cslice stack 0 arity))
                           (ret (procedure-runner (typhon@operand c) arity args env dump)))
                        (if (or (empty? stack) (= arity 0))
                           (typhon@vm
                                code code-len
                                env
                                (+ ip 1)
                                (cons ret stack)
                                locals
                                dump offset)
                           (typhon@vm
                                code code-len
                                env
                                (+ ip 1)
                                (cons ret (cslice stack (- arity 1) (length (cdr stack))))
                                locals
                                dump offset)))
                    (18) ;; real?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (real? (car stack)) (cdr stack)) locals dump offset)
                    (19) ;; integer?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (integer? (car stack)) (cdr stack)) locals dump offset)
                    (20) ;; complex?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (complex? (car stack)) (cdr stack)) locals dump offset)
                    (21) ;; rational?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (rational? (car stack)) (cdr stack)) locals dump offset)
                    (22) ;; gcd
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (gcd (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (23) ;; lcm
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (lcm (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (24) ;; numerator 
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (numerator (car stack)) (cdr stack)) locals dump offset)
                    (25) ;; denomenator
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (denomenator (car stack)) (cdr stack)) locals dump offset)
                    (26) ;; = 
                        (let* ((top-of-stack (typhon@operand c))
                              (bottom-of-stack '())
                              (ret 0))
                            (cond
                                (= top-of-stack 0)
                                    (begin
                                        (set! ret (make-typhon-error "in correct arity for ="))
                                        (set! bottom-of-stack stack))
                                (= top-of-stack 1)
                                    (begin
                                        (set! ret #t)
                                        (set! bottom-of-stack (cdr stack)))
                                (= top-of-stack 2)
                                    (begin
                                        (set! ret (= (car stack) (cadr stack)))
                                        (set! bottom-of-stack (cddr stack)))
                                else
                                    (begin
                                        (set! ret (apply = (cslice stack 0 top-of-stack)))
                                        (set! bottom-of-stack (cslice stack top-of-stack -1))))
                                (typhon@vm code code-len
                                    env
                                    (+ ip 1)
                                    (cons ret bottom-of-stack)
                                    locals
                                    dump offset))
                    (27) ;; eq?
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (eq? (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (28) ;; jump
                        (typhon@vm code code-len
                                 env
                                 (+ ip (typhon@operand c))
                                 stack locals dump offset)
                    (29) ;; cmp
                        (if (car stack) ;; if the top of the stack is true
                            (typhon@vm code code-len env (+ ip 1) (cdr stack) locals dump offset) ;; jump to the <then> portion
                            (typhon@vm code code-len env (+ ip (typhon@operand c)) (cdr stack) locals dump offset))
                    (30) ;; call
                        ;; need to make call check it's operand now...
                        (let ((call-proc (typhon@operand c)))
                            ;(display "in <else> of CALL\n")
                            ;(display "c == ")
                            ;(write c)
                            ;(newline)
                            ;(display "(car stack) == ")
                            ;(write (car stack))
                            ;(display "\n")
                            (if (symbol? call-proc)
                                (set! call-proc (typhon@lookup call-proc env))
                                #v)
                            (cond
                                (and
                                    (eq? (cdr stack) '())
                                    (not (eq? (nth (cadr call-proc) 2) '())))
                                    (make-typhon-error "incorrect arity for user procedure")
                                (typhon-error? call-proc)
                                    call-proc
                                (typhon@lambda? call-proc)
                                    ;; create a list from the current registers, cons this to dump, and 
                                    ;; recurse over typhon@vm. 
                                    ;; need to support CALLing primitives too, since they could be passed
                                    ;; in to HOFs...
                                    (if (> offset (length dump))
                                        (error "Dump stack overflow")
                                        (let ((env-and-stack (build-environment (vector-ref (cadr call-proc) 1) stack (vector-ref (cadr call-proc) 2) locals)))
                                              ;(v-dump (typhon-dump-dump dump offset))
                                              ;(offset (typhon-dump-offset dump offset)))
                                            (vector-set! dump offset locals)
                                            (vector-set! dump (+ offset 1) (cadr env-and-stack))
                                            (vector-set! dump (+ offset 2) ip)
                                            (vector-set! dump (+ offset 3) env)
                                            (vector-set! dump (+ offset 4) code-len)
                                            (vector-set! dump (+ offset 5) code)
                                            ;(typhon-dump-set-offset! dump (+ offset 6))
                                            (typhon@vm
                                                (vector-ref (cadr call-proc) 1)
                                                (length (vector-ref (cadr call-proc) 1))
                                                (car env-and-stack)
                                                0 '() 
                                                (caddr env-and-stack)
                                                dump (+ offset 6))))
                                (typhon-primitive? (car stack)) ;; if primitives stored arity, slicing would be easy...
                                    (begin
                                        ;(display "in typhon@primitive\n\t")
                                        ;(display (car stack))
                                        ;(display "\n")
                                        #t)
                                ;;(typhon@procedure? (car stack))
                                ;;    #t
                                else
                                (begin
                                    (display "in <else> of CALL\n")
                                    (display "c == ")
                                    (write c)
                                    (newline)
                                    (display "(car stack) == ")
                                    (write (car stack))
                                    (display "\n")
                                #f)))
                    (31) ;; environment-load; there is never a raw #f, so this is safe
                        (with r (typhon@lookup (typhon@operand c) env)
                            (if (typhon-error? r)
                                r
                                (typhon@vm
                                    code
                                    code-len
                                    env
                                    (+ ip 1) 
                                    (cons r stack)
                                    locals
                                    dump offset)))
                    (32) ;; tail-call 
                        (if (and (not (null? stack)) (eq? (caar stack) 'compiled-lambda))
                            (typhon@vm
                                (nth (cdar stack) 0)
                                (length (nth (cdar stack) 0))
                                (nth (cdar stack) 1)
                                0 '() 
                                locals
                                dump offset)
                            #f)
                    (33) ;; %define
                        (begin
                            (typhon@add-env! (car stack) (cadr stack) env)
                            (typhon@vm
                                code code-len env (+ ip 1)
                                (cons #v stack)
                                locals
                                dump offset))
                    (34) ;; %set!
                        (begin
                            (typhon@set-env! (car stack) (cadr stack) env)
                            (typhon@vm
                                code code-len env (+ ip 1)
                                (cons #v stack)
                                locals
                                dump offset))
                    (35) ;; ceil
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (ceil (car stack)) (cdr stack)) locals dump offset)
                    (36) ;; floor
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (floor (car stack)) (cdr stack)) locals dump offset)
                    (37) ;; truncate
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (truncate (car stack)) (cdr stack)) locals dump offset)
                    (38) ;; round
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (round (car stack)) (cdr stack)) locals dump offset)
                    (39) ;; inexact->exact
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (inexact->exact (car stack)) (cdr stack)) locals dump offset)
                    (40) ;; quotient
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (quotient (cadr stack) (car stack)) (cddr stack)) locals dump offset)
                    (41) ;; modulo
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (modulo (cadr stack) (car stack)) (cddr stack)) locals dump offset)
                    (42) ;; &
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (& (cadr stack) (car stack)) (cddr stack)) locals dump offset)
                    (43) ;; |
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (| (cadr stack) (car stack)) (cddr stack)) locals dump offset)
                    (44) ;; ^
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (^ (cadr stack) (car stack)) (cddr stack)) locals dump offset)
                    (45) ;; ~
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (~ (car stack)) (cdr stack)) locals dump offset)
                    (46) ;; %list
                        ;; take N items off the stack, create a list, and return it
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (cslice (cdr stack) 0 (car stack))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump offset)
                    (47) ;; %vector
                        ;; take N items off the stack, create a list, and return it
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (coerce (cslice (cdr stack) 0 (car stack)) 'vector)
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump offset)
                    (48) ;; %make-vector
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-vector (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (49) ;; %make-string
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-string (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (50) ;; %string
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (apply string (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump offset)
                    (51) ;; %append
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (apply append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump offset)
                    (52) ;; first
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (first (car stack)) (cdr stack)) locals dump offset)
                    (53) ;; rest
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (rest (car stack)) (cdr stack)) locals dump offset)
                    (54) ;; ccons
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (ccons (cadr stack) (car stack)) (cddr stack)) locals dump offset)
                    (55) ;; %nth
                            (typhon@vm code code-len
                                env
                                (+ ip 1)
                                (cons (nth (car stack) (cadr stack) (caddr stack)) (cdddr stack))
                                locals dump offset)
                    (56) ;; keys
                            (typhon@vm code code-len
                                env
                                (+ ip 1)
                                (cons (keys (car stack)) (cdr stack)) locals dump offset)
                    (57) ;; partial-key?
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (partial-key? (car stack) (cadr stack)) (cddr stack))
                            locals
                            dump offset)
                    (58) ;; cset!
                        (begin
                            (cset! (car stack) (cadr stack) (caddr stack))
                            (typhon@vm code code-len
                                env
                                (+ ip 1)
                                (cons #v (cdddr stack)) locals dump offset))
                    (59) ;; empty?
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (empty? (car stack)) (cdr stack))
                            locals
                            dump offset)
                    (60) ;; gensym
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (gensym (car stack)) (cdr stack)) locals dump offset)
                    (61) ;; imag-part
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (imag-part (car stack)) (cdr stack)) locals dump offset)
                    (62) ;; real-part
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (real-part (car stack)) (cdr stack)) locals dump offset)
                    (63) ;; make-rectangular
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-rectangular (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (64) ;; make-polar
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-polar (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (65) ;; magnitude
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (magnitude (car stack)) (cdr stack)) locals dump offset)
                    (66) ;; argument
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (argument (car stack)) (cdr stack)) locals dump offset)
                    (67) ;; conjugate
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (conjugate (car stack)) (cdr stack)) locals dump offset)
                    (68) ;; conjugate
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (conjugate! (car stack)) (cdr stack)) locals dump offset)
                    (69) ;; polar->rectangular
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (polar->rectangular (car stack)) (cdr stack)) locals dump offset)
                    (70) ;; rectangular->polar
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (rectangular->polar (car stack)) (cdr stack)) locals dump offset)
                    (71) ;; sin
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (sin (car stack)) (cdr stack)) locals dump offset)
                    (72) ;; cos
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cos (car stack)) (cdr stack)) locals dump offset)
                    (73) ;; tan
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (tan (car stack)) (cdr stack)) locals dump offset)
                    (74) ;; asin
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (asin (car stack)) (cdr stack)) locals dump offset)
                    (75) ;; acos
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (acos (car stack)) (cdr stack)) locals dump offset)
                    (76) ;; atan
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (atan (car stack)) (cdr stack)) locals dump offset)
                    (77) ;; atan2
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (atan2 (cadr stack) (car stack)) (cddr stack)) locals dump offset)
                    (78) ;; sinh
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (sinh (car stack)) (cdr stack)) locals dump offset)
                    (79) ;; cosh
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cosh (car stack)) (cdr stack)) locals dump offset)
                    (80) ;; tanh
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (tanh (car stack)) (cdr stack)) locals dump offset)
                    (81) ;; exp
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp (car stack)) (cdr stack)) locals dump offset)
                    (82) ;; ln
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (ln (car stack)) (cdr stack)) locals dump offset)
                    (83) ;; abs
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (abs (car stack)) (cdr stack)) locals dump offset)
                    (84) ;; sqrt
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (sqrt (car stack)) (cdr stack)) locals dump offset)
                    (85) ;; exp2
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump offset)
                    (86) ;; expm1
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (expm1 (car stack)) (cdr stack)) locals dump offset)
                    (87) ;; log2
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (log2 (car stack)) (cdr stack)) locals dump offset)
                    (88) ;; log10
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (log10 (car stack)) (cdr stack)) locals dump offset)
                    (89) ;; <<
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (<< (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (90) ;; >>
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (>> (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (91) ;; %string-append
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (apply string-append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1)))
                            locals
                            dump offset)
                    (92) ;; assq
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (assq (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (93) ;; memq
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (memq (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (94) ;; %dict, need to wrap these...
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (dict type: 'dict value: (apply dict (cslice (cdr stack) 0 (car stack))))
                                (cslice (cdr stack) (car stack) (- (length stack) 1)))
                            locals
                            dump offset)
                    (95) ;; make-dict
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (dict type: 'dict value: (make-dict)) stack) locals dump offset)
                    (96) ;; dict-has? need to unwrap this...
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (dict-has? (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (97) ;; coerce, check for sanbox escape too...
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (coerce (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (98) ;; cupdate, unwrap dicts, also, need to update cset!
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cupdate (car stack) (cadr stack) (caddr stack)) (cdddr stack)) locals dump offset)
                    (99) ;; cslice
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cslice (car stack) (cadr stack) (caddr stack)) (cdddr stack)) locals dump offset)
                    (100) ;; tconc!
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump offset)
                    (101) ;; make-tconc
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump offset)
                    (102) ;; tconc-list
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump offset)
                    (103) ;; tconc->pair
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump offset)
                    (104) ;; tconc-splice
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump offset)
                    (105) ;; rationalize
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (rationalize (car stack) (cadr stack)) (cddr stack)) locals dump offset)
                    (106) ;; call/cc
                        (let ((retcode (typhon@vm (cons (list 3 (car stack)) (list (list 30))) 2 ;; there's only two instructions here ...
                                        env
                                        0
                                        (cons (list 'continuation (copy-code code ip 0) ip env stack locals dump offset) '())
                                        locals
                                        '() 0)))
                         (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons retcode (cdr stack))
                            locals
                            dump offset))
                    (107) ;; %nop
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            stack
                            locals
                            dump offset)
                    (108) ;; %ap
                        (let ((cont-code (car stack))
                              (v (cadr stack)))
                         (typhon@vm 
                            (nth cont-code 1)
                            (length (nth cont-code 1))
                            (nth cont-code 3)
                            0
                            (cons
                                v
                                (nth cont-code 4))
                            (nth cont-code 5)
                            (nth cont-code 6) 0))
                    (109) ;; %makeclosure
                        ;; makeclosure should rebuild the lambda that it is "enclosing"
                        ;; to ensure clean enclosing, rather than using cset, which just
                        ;; gives us the same problem we have now... duh (well, when you
                        ;; think about it, sure, but the naive approach? sure).
                        (begin
                            ;;(cset! (cadar stack) 0 env)
                            (typhon@vm
                                code code-len
                                env
                                (+ ip 1)
                                (cons
                                    (list 'compiled-lambda
                                        (vector
                                            (srfi1-list-copy env)
                                            (nth (cadar stack) 1)
                                            (nth (cadar stack) 2)))
                                    (cdr stack))
                                locals
                                dump offset))
                    (110) ;; call from stack
                        (let ((call-proc (car stack)))
                            ;(display "call-proc == ")
                            ;(write call-proc)
                            ;(display "\n")
                            (cond
                                (and
                                    (eq? (cdr stack) '())
                                    (not (eq? (nth (cadr call-proc) 2) '())))
                                    (make-typhon-error "incorrect arity for user procedure")
                                (typhon-error? call-proc)
                                    (begin
                                        ;(display "error: ")
                                        ;(write call-proc)
                                        ;(newline)
                                        call-proc)
                                (typhon@lambda? call-proc)
                                    ;; create a list from the current registers, cons this to dump, and 
                                    ;; recurse over typhon@vm. 
                                    ;; need to support CALLing primitives too, since they could be passed
                                    ;; in to HOFs...
                                    ;; nth (on vectors) => vector-ref, cset! (on vectors) => vector-set!
                                    (if (> offset (length dump))
                                        (error "Dump stack overflow")
                                        (let ((env-and-stack (build-environment (vector-ref (cadr call-proc) 0) (cdr stack) (vector-ref (cadr call-proc) 2) locals)))
                                              ;(v-dump (typhon-dump-dump dump offset))
                                              ;(offset (typhon-dump-offset dump offset)))
                                            ;(display "in let; (car env-and-stack) == ")
                                            ;(write (car env-and-stack))
                                            ;(newline)
                                            (vector-set! dump offset locals) ;; can these be made into vector-set! calls?
                                            (vector-set! dump (+ offset 1) (cadr env-and-stack))
                                            (vector-set! dump (+ offset 2) ip)
                                            (vector-set! dump (+ offset 3) env)
                                            (vector-set! dump (+ offset 4) code-len)
                                            (vector-set! dump (+ offset 5) code)
                                            ;(typhon-dump-set-offset! dump (+ offset 6))
                                            (typhon@vm
                                                (vector-ref (cadr call-proc) 1)
                                                (length (vector-ref (cadr call-proc) 1))
                                                (car env-and-stack)
                                                0 '() 
                                                (caddr env-and-stack)
                                                dump (+ offset 6))))
                                (typhon-primitive? (car stack)) ;; if primitives stored arity, slicing would be easy...
                                    #t
                                else
                                    (error "non-applicable CALL-STACK argument")))
                    (111) ;; type
                        (typhon@vm
                            code
                            code-len
                            env
                            (+ ip 1)
                            (cons (type (car stack)) (cdr stack))
                            locals
                            dump offset)
                    (112) ;; fetch local
                        (typhon@vm
                            code
                            code-len
                            env
                            (+ ip 1)
                            (cons
                                (vector-ref locals (typhon@operand c))
                                stack)
                            locals
                            dump offset)
                    (113) ;; set local
                        (let ((lidx (nth c 1))
                              (envobj (nth c 2 #f)))
                            (vector-set! locals lidx (car stack))
                            (if (not (eq? envobj #f))
                                (typhon@add-env! envobj (car stack) env)
                                #v)
                            (typhon@vm
                                code
                                code-len
                                env
                                (+ ip 1)
                                (cdr stack)
                                locals
                                dump offset))
                    (114) ;; return
                        (if (= offset 0) ;; should switch dump to a struct...
                            (car stack)
                            (let ((top-dump (cadr dump))
                                  (offset (car dump)))

                                (typhon@vm
                                    (vector-ref top-dump (- offset 1))
                                    (vector-ref top-dump (- offset 2))
                                    (vector-ref top-dump (- offset 3))
                                    (+ (vector-ref top-dump (- offset 4)) 1)
                                    (cons (car stack) (vector-ref top-dump (- offset 5)))
                                    (vector-ref top-dump (- offset 6))
                                    (list (- offset 6) top-dump) offset)))
                    (115) ;; return from literal
                        (if (= (car dump) 0) ;; should switch dump to a struct...
                            (typhon@operand c)
                            (let ((top-dump (cadr dump))
                                  (offset (car dump)))

                                (typhon@vm
                                    (vector-ref top-dump (- offset 1))
                                    (vector-ref top-dump (- offset 2))
                                    (vector-ref top-dump (- offset 3))
                                    (+ (vector-ref top-dump (- offset 4)) 1)
                                    (cons (typhon@operand c) (vector-ref top-dump (- offset 5)))
                                    (vector-ref top-dump (- offset 6))
                                    (list (- offset 6) top-dump) offset)))
                    (116) ;; return from environment/locals
                          ;; need to wedge the above items into here some how...
                        (let ((element (typhon@operand c)))
                            (cond
                                (integer? element) (vector-ref locals element)
                                (symbol? element) ;; environment return
                                    (typhon@lookup element env)
                                else (make-typhon-error "stack underflow in return")))))))

; syntax to make the above nicer:
; (define-instruction := "numeq" '() '() (+ ip 1) (cons (= (car stack) (cadr stack)) (cddr stack)))
; (define-instruction '() "jump" '() '() '() (if (car stack) ...))
; this should also fill in the top-level environment for those that have non-null name
; (define-instruction name short-description code-manip stack-manip IP-manip resulting code) 

; (primitive 0) can still be defeated, I think:
; (define foo '(primitive 0))
; (foo '(1 2 3 4))
; Definitely can be defeated. Really, need to move to some SRFI-9-ish system that
; users cannot create their own versions of.

; there are two ways of dealing with arity, both have upsides & downsides:

; 0 - encode arity of primitives here, in the actual primitive notation.
;     + this allows typhon@compile to know the proper arity, and signal an error.
;     + it also means that typhon@vm has to suddenly change: it must now unpack the actual opcode
;       before running (this might not be too bad...)
; 1 - encode the arity of primitives in a separate array.
;     + this allows typhon@compile to remain unchanged, and only minimal changes to typhon@vm
;     + this does not confer the benefits that the above does (that typhon@compile can
;       know about the arity of primitives & signal failure during code generation).

(define (typhon@init-env env)
    ;; Once Enyalios supports dicts, these will be even smaller
    ;; *However*, this isn't a bad method for dealing with structs
    ;; in Typhon, so long as user's can create them in a
    ;; Typhon-in-Typhon use case :D
    ;; all of these make me feel like some syntax for fixed-arity
    ;; primitives wouldn't be terrible... or a separate type?
    (dict-set! env "car" (make-typhon-primitive 0 1 1))
    (dict-set! env "call/cc" (make-typhon-primitive 106 1 1))
    (dict-set! env "cdr" (make-typhon-primitive 1 1 1))
    (dict-set! env "cons" (make-typhon-primitive 2 2 2))
    (dict-set! env "conjugate" (make-typhon-primitive 67 1 1))
    (dict-set! env "conjugate!" (make-typhon-primitive 68 1 1))
    (dict-set! env "complex?" (make-typhon-primitive 20 1 1))
    (dict-set! env "cos" (make-typhon-primitive 72 1 1))
    (dict-set! env "cosh" (make-typhon-primitive 79 1 1))
    (dict-set! env "coerce" (make-typhon-primitive 97 2 2))
    (dict-set! env "ceil" (make-typhon-primitive 35 1 1))
    (dict-set! env "ccons" (make-typhon-primitive 54 2 2))
    (dict-set! env "cset!" (make-typhon-primitive 58 3 3))
    (dict-set! env "cslice" (make-typhon-primitive 99 3 3))
    (dict-set! env "cupdate" (make-typhon-primitive 98 3 3))
    (dict-set! env "load" (make-typhon-primitive 3 1 2))
    (dict-set! env "list" (make-typhon-primitive 46 0 -1)) ;; N
    (dict-set! env "nil" (make-typhon-primitive 4 0 0))
    (dict-set! env "nop" (make-typhon-primitive 107 0 0))
    (dict-set! env "-" (make-typhon-primitive 5 1 -1))
    (dict-set! env "+" (make-typhon-primitive 6 0 -1))
    (dict-set! env "*" (make-typhon-primitive 7 0 -1))
    (dict-set! env "/" (make-typhon-primitive 8 1 -1))
    (dict-set! env "<" (make-typhon-primitive 9 2 -1))
    (dict-set! env "<=" (make-typhon-primitive 11 2 -1))
    (dict-set! env ">" (make-typhon-primitive 10 2 -1))
    (dict-set! env ">=" (make-typhon-primitive 12 2 -1))
    (dict-set! env "=" (make-typhon-primitive 26 1 -1))
    (dict-set! env "%jmp" (make-typhon-primitive 28 1 1))
    (dict-set! env "%cmp" (make-typhon-primitive 29 0 0))
    (dict-set! env "%call" (make-typhon-primitive 30 1 1))
    (dict-set! env "%env-load" (make-typhon-primitive 31 1 1))
    (dict-set! env "%tail-call" (make-typhon-primitive 32 1 1))
    (dict-set! env "%define" (make-typhon-primitive 33 1 1))
    (dict-set! env "%set!" (make-typhon-primitive 34 2 2))
    (dict-set! env "string" (make-typhon-primitive 50 0 -1))
    (dict-set! env "vector" (make-typhon-primitive 47 0 -1))
    (dict-set! env "make-vector" (make-typhon-primitive 48 1 2))
    (dict-set! env "make-string" (make-typhon-primitive 49 1 2))
    (dict-set! env "%ap" (make-typhon-primitive 108 1 1))
    (dict-set! env "%makeclosure" (make-typhon-primitive 109 1 1))
    (dict-set! env "append" (make-typhon-primitive 51 0 -1))
    (dict-set! env "inexact?" (make-typhon-primitive 15 1 1))
    (dict-set! env "inexact->exact" (make-typhon-primitive 39 1 1))
    (dict-set! env "integer?" (make-typhon-primitive 19 1 1))
    (dict-set! env "imag-part" (make-typhon-primitive 61 1 1))
    (dict-set! env "floor" (make-typhon-primitive 36 1 1))
    (dict-set! env "first" (make-typhon-primitive 52 1 1))
    (dict-set! env "length" (make-typhon-primitive 13 1 1))
    (dict-set! env "lcm" (make-typhon-primitive 23 2 -1))
    (dict-set! env "ln" (make-typhon-primitive 82 1 1))
    (dict-set! env "log2" (make-typhon-primitive 87 1 1))
    (dict-set! env "log10" (make-typhon-primitive 88 1 1))
    (dict-set! env "exact?" (make-typhon-primitive 14 1 1))
    (dict-set! env "exp" (make-typhon-primitive 81 1 1))
    (dict-set! env "exp2" (make-typhon-primitive 85 1 1))
    (dict-set! env "expm1" (make-typhon-primitive 86 1 1))
    (dict-set! env "eq?" (make-typhon-primitive 27 2 2))
    (dict-set! env "empty?" (make-typhon-primitive 59 1 1))
    (dict-set! env "quotient" (make-typhon-primitive 40 2 2))
    (dict-set! env "dict" (make-typhon-primitive 94 0 -1))
    (dict-set! env "dict-has?" (make-typhon-primitive 96 1 1))
    (dict-set! env "denomenator" (make-typhon-primitive 25 1 1))
    (dict-set! env "apply" (make-typhon-primitive 17 2 2))
    (dict-set! env "argument" (make-typhon-primitive 66 1 1))
    (dict-set! env "asin" (make-typhon-primitive 74 1 1))
    (dict-set! env "assq" (make-typhon-primitive 92 2 2))
    (dict-set! env "acos" (make-typhon-primitive 75 1 1))
    (dict-set! env "atan" (make-typhon-primitive 76 1 1))
    (dict-set! env "atan2" (make-typhon-primitive 77 2 2))
    (dict-set! env "abs" (make-typhon-primitive 83 1 1))
    (dict-set! env "real?" (make-typhon-primitive 18 1 1))
    (dict-set! env "real-part" (make-typhon-primitive 62 1 1))
    (dict-set! env "rest" (make-typhon-primitive 53 1 1))
    (dict-set! env "rectangular->polar" (make-typhon-primitive 70 1 1))
    (dict-set! env "rational?" (make-typhon-primitive 21 1 1))
    (dict-set! env "rationalize" (make-typhon-primitive 105 1 2))
    (dict-set! env "round" (make-typhon-primitive 38 1 1))
    (dict-set! env "gcd" (make-typhon-primitive 22 2 2))
    (dict-set! env "gensym" (make-typhon-primitive 60 0 1))
    (dict-set! env "numerator" (make-typhon-primitive 24 1 1))
    (dict-set! env "nth" (make-typhon-primitive 55 2 3))
    (dict-set! env "<<" (make-typhon-primitive 89 2 2))
    (dict-set! env ">>" (make-typhon-primitive 90 2 2))
    (dict-set! env "string-append" (make-typhon-primitive 91 0 -1))
    (dict-set! env "sin" (make-typhon-primitive 71 1 1))
    (dict-set! env "sinh" (make-typhon-primitive 78 1 1))
    (dict-set! env "sqrt" (make-typhon-primitive 84 1 1))
    (dict-set! env "truncate" (make-typhon-primitive 37 1 1))
    (dict-set! env "tan" (make-typhon-primitive 73 1 1))
    (dict-set! env "tanh" (make-typhon-primitive 80 1 1))
    (dict-set! env "tconc!" (make-typhon-primitive 100 1 1))
    (dict-set! env "tconc-list" (make-typhon-primitive 102 1 1))
    (dict-set! env "tconc->pair" (make-typhon-primitive 103 1 1))
    (dict-set! env "tconc-splice!" (make-typhon-primitive 104 2 2))
    (dict-set! env "modulo" (make-typhon-primitive 41 1 1))
    (dict-set! env "keys" (make-typhon-primitive 56 1 1))
    (dict-set! env "partial-key?" (make-typhon-primitive 57 2 2))
    (dict-set! env "polar->rectangular" (make-typhon-primitive 69 1 1))
    (dict-set! env "type" (make-typhon-primitive 111 1 1))
    (dict-set! env "make-rectangular" (make-typhon-primitive 63 1 1))
    (dict-set! env "make-polar" (make-typhon-primitive 64 1 1))
    (dict-set! env "make-dict" (make-typhon-primitive 95 0 0))
    (dict-set! env "make-tconc" (make-typhon-primitive 101 0 1))
    (dict-set! env "magnitude" (make-typhon-primitive 65 1 1))
    (dict-set! env "memq" (make-typhon-primitive 93 2 2))
    (dict-set! env "&" (make-typhon-primitive 42 2 2))
    (dict-set! env "|" (make-typhon-primitive 43 2 2))
    (dict-set! env "^" (make-typhon-primitive 44 2 2))
    (dict-set! env "~" (make-typhon-primitive 45 2 2))
    (dict-set! env "if" (make-typhon-syntax 'primitive-syntax-if))
    (dict-set! env "quote" (make-typhon-syntax 'primitive-syntax-quote))
    (dict-set! env "quasi-quote" (make-typhon-syntax 'primitive-syntax-qquote))
    (dict-set! env "lambda" (make-typhon-syntax 'primitive-syntax-fn))
    (dict-set! env "fn" (make-typhon-syntax 'primitive-syntax-fn))
    (dict-set! env "begin" (make-typhon-syntax 'primitive-syntax-begin))
    (dict-set! env "unquote" (make-typhon-syntax 'primitve-syntax-unquote))
    (dict-set! env "unquote-splice" (make-typhon-syntax 'primitive-syntax-unqsplice))
    (dict-set! env "define" (make-typhon-syntax 'primitive-syntax-define))
    (dict-set! env "define-syntax" (make-typhon-syntax 'primitive-syntax-defsyn))
    (dict-set! env "define-macro" (make-typhon-syntax 'primitive-syntax-defmac))
    (dict-set! env "set!" (make-typhon-syntax 'primitive-syntax-set))
    (dict-set! env "display" (make-typhon-procedure "display" 1 2))
    (dict-set! env "newline" (make-typhon-procedure "newline" 0 1))
    (dict-set! env "digamma-implementation" (make-typhon-procedure "digamma-implementation" 0 0))
    (dict-set! env "load" (make-typhon-procedure "load" 1 2))
    (dict-set! env "read" (make-typhon-procedure "read" 0 1))
    (dict-set! env "write" (make-typhon-procedure "write" 1 2))
    (dict-set! env "format" (make-typhon-procedure "format" 1 -1))
    (dict-set! env "macro-expand" (make-typhon-procedure "macro-expand" 1 1))
    (dict-set! env "syntax-expand" (make-typhon-procedure "syntax-expand" 1 1))
    ;(dict-set! env "interrogate" (make-typhon-procedure "interrogate"))
    (dict-set! env "eval" (make-typhon-procedure "eval" 1 2))
    (dict-set! env "default-environment" (make-typhon-procedure "default-environment" 0 0))
    (dict-set! env "null-environment" (make-typhon-procedure "null-environment" 0 0))
    (dict-set! env "make-environment" (make-typhon-procedure "make-environment" 0 1))
    (dict-set! env "clone-environment" (make-typhon-procedure "clone-environment" 1 1))
    (dict-set! env "set-environment!" (make-typhon-procedure "environment-set!" 3 3))
    (dict-set! env "bind-environment" (make-typhon-procedure "bind-environment" 3 3))
    (dict-set! env "current-environment" (make-typhon-procedure "default-environment" 0 0))
    (dict-set! env "system" (make-typhon-procedure "system" 1 1))
    (dict-set! env "pwd" (make-typhon-procedure "pwd" 0 0))
    (dict-set! env "cd" (make-typhon-procedure "chdir" 1 1))
    (dict-set! env "chdir" (make-typhon-procedure "chdir" 1 1))
    (dict-set! env "ls" (make-typhon-procedure "ls" 1 1))
    (dict-set! env "open" (make-typhon-procedure "open" 2 2))
    (dict-set! env "close" (make-typhon-procedure "close" 1 1))
    (dict-set! env "read-char" (make-typhon-procedure "read-char" 0 1))
    (dict-set! env "read-string" (make-typhon-procedure "read-string" 0 1))
    (dict-set! env "write-char" (make-typhon-procedure "write-char" 1 2))
    (dict-set! env "dial" (make-typhon-procedure "dial" 3 3))
    (dict-set! env "announce" (make-typhon-procedure "announce" 3 3))
    (dict-set! env "listen" (make-typhon-procedure "listen" 1 1))
    (dict-set! env "accept" (make-typhon-procedure "accept" 1 1))
    (dict-set! env "hangup" (make-typhon-procedure "hangup" 1 1))
    (dict-set! env "remote-port-open?" (make-typhon-procedure "remote-port-open?" 2 2))
    (dict-set! env "gethostbyname" (make-typhon-procedure "gethostbyname" 1 1))
    (dict-set! env "gethostbyaddr" (make-typhon-procedure "gethostbyaddr" 1 1))
    (dict-set! env "peek-char" (make-typhon-procedure "peek-char" 0 1))
    (dict-set! env "port-filename" (make-typhon-procedure "port-filename" 1 1))
    (dict-set! env "port-mode" (make-typhon-procedure "port-mode" 1 1))
    (dict-set! env "port-remote-port" (make-typhon-procedure "port-remote-port" 1 1))
    (dict-set! env "port-protocol" (make-typhon-procedure "port-protocol" 1 1))
    (dict-set! env "port-state" (make-typhon-procedure "port-state" 1 1))
    (dict-set! env "port-type" (make-typhon-procedure "port-type" 1 1))
    (dict-set! env "end-of-port?" (make-typhon-procedure "end-of-port?" 1 1))
    (dict-set! env "random" (make-typhon-procedure "random" 0 1))
    (dict-set! env "seed-random" (make-typhon-procedure "seed-random" 1 1))
    (dict-set! env "sys/gettimeofday" (make-typhon-procedure "sys/gettimeofday" 0 0))
    (dict-set! env "sys/getuid" (make-typhon-procedure "sys/getuid" 0 0))
    (dict-set! env "sys/geteuid" (make-typhon-procedure "sys/geteuid" 0 0))
    (dict-set! env "sys/getgid" (make-typhon-procedure "sys/getgid" 0 0))
    (dict-set! env "sys/getegid" (make-typhon-procedure "sys/getegid" 0 0))
    (dict-set! env "sys/set*id" (make-typhon-procedure "sys/set*id" 1 1))
    (dict-set! env "sys/open" (make-typhon-procedure "sys/open" 2 2))
    (dict-set! env "sys/close" (make-typhon-procedure "sys/close" 1 1))
    (dict-set! env "sys/read" (make-typhon-procedure "sys/read" 1 1))
    (dict-set! env "sys/write" (make-typhon-procedure "sys/write" 2 2))
    (dict-set! env "sys/pipe" (make-typhon-procedure "sys/pipe" 0 0))
    (dict-set! env "sys/fork" (make-typhon-procedure "sys/fork" 0 0))
    (dict-set! env "sys/wait" (make-typhon-procedure "sys/wait" 1 1))
    (dict-set! env "sys/exec" (make-typhon-procedure "sys/exec" 3 3))
    (dict-set! env "sys/popen" (make-typhon-procedure "sys/popen" 1 1))
    (dict-set! env "sys/pclose" (make-typhon-procedure "sys/pclose" 1 1))
    (dict-set! env "sys/vfork" (make-typhon-procedure "sys/vfork" 0 0))
    (dict-set! env "sys/kill" (make-typhon-procedure "sys/kill" 2 2))
    (dict-set! env "sys/stat" (make-typhon-procedure "sys/stat" 1 2))
    (dict-set! env "sys/*sockopt" (make-typhon-procedure "sys/*sockopt" 3 3))
    (dict-set! env "sys/time" (make-typhon-procedure "sys/time" 0 0))
    (dict-set! env "sys/chown" (make-typhon-procedure "sys/chown" 2 2))
    (dict-set! env "sys/chmod" (make-typhon-procedure "sys/chmod" 2 2))
    (dict-set! env "sys/chroot" (make-typhon-procedure "sys/chroot" 2 2))
    (dict-set! env "sys/getenv" (make-typhon-procedure "sys/getenv" 1 1))
    (dict-set! env "sys/setenv" (make-typhon-procedure "sys/setenv" 2 2))
    (dict-set! env "sys/fcntl" (make-typhon-procedure "sys/fcntl" 3 3))
    (dict-set! env "sys/fcntl-const" (make-typhon-procedure "sys/fcntl-const" 1 1))
    (dict-set! env "sys/sleep" (make-typhon-procedure "sys/sleep" 1 1))
    (dict-set! env "sys/usleep" (make-typhon-procedure "sys/usleep" 1 1))
    (dict-set! env "sys/nanosleep" (make-typhon-procedure "sys/nanosleep" 1 1))
    (dict-set! env "sys/select" (make-typhon-procedure "sys/select" 3 3))
    (dict-set! env "sys/getpid" (make-typhon-procedure "sys/getpid" 0 0))
    (dict-set! env "sys/getppid" (make-typhon-procedure "sys/getppid" 0 0))
    (dict-set! env "write-buffer" (make-typhon-procedure "write-buffer" 2 2))
    (dict-set! env "read-buffer" (make-typhon-procedure "read-buffer" 2 2))
    (dict-set! env "read-string" (make-typhon-procedure "read-string" 1 2)))

(define (typhon@lookup item env)
    " look up item in the current environment, returning #f for not found"
    ;(display "in typhon@lookup: ")
    ;(display "item == ")
    ;(write item)
    ;(display " and env == ")
    ;(write env)
    ;(newline)
    (cond
        (not (symbol? item)) item ;; to support ((fn (x) (+ x x)) (+ x x) 3)
        (null? env) (make-typhon-error (format "unbound variable: ~a" item)) 
        (dict-has? (car env) item) (nth (car env) item) 
        else (typhon@lookup item (cdr env))))

(define (typhon@appropos item env)
    #f)

(define (compile-begin lst params env tail?)
    (cond
        (null? lst)
            '()
        (and
            tail?
            (null? (cdr lst))) ;; tail
            (typhon@compile (car lst) params env #t)
        else
            (append
                (typhon@compile (car lst) params env tail?)
                (compile-begin (cdr lst) params env tail?))))

(define (compile-lambda rst env tail?)
    (list 'compiled-lambda
        (vector
            env
            (coerce
                (compile-begin (cdr rst) (car rst) env #f)
                'vector)
            (car rst)))) 

(define (typhon@add-env! name value environment)
    " adds name to the environment, but also returns
      (load #v), so that the compiler adds the correct
      value (this is in the semantics of Vesta, so I thought
      it should be left in Hydra as well)"
    (cset! (car environment) name value))

(define (typhon@set-env! name value environment)
    " sets a value in the current environment, and returns
      an error if that binding has not been previously defined"
    (cond
        (null? environment) (make-typhon-error (format "SET! error: undefined name \"~a\"" name))
        (dict-has? (car environment) name)
            (cset! (car environment) name value)
        else (typhon@set-env! name value (cdr environment))))

(define (reverse-append x)
    "append but in reverse"
    (cond
        (null? x) x
        (null? (cdr x)) (car x)
        else (append (reverse-append (cddr x)) (cadr x) (car x))))

(define (show x m) (display m) (display x) (display "\n") x)

(define (typhon@error msg)
    "simple, hydra specific errors"
    (list 'error msg))

(define (typhon@eval line env dump)
    "simple wrapper around typhon@vm & typhon@compile"
    (with code (coerce (typhon@compile line '() env #f) 'vector)
        (typhon@vm code (length code) env 0 '() '() dump 0)))

(define (typhon@compile-help sym iter-list params env tail?)
    " a helper function for typhon@compile, which collects
      the old use of append-map into a single function that
      Eprime can compile (still haven't added HOFs to E'...
      embarrassing, I know)
    "
    (if (null? iter-list)
        iter-list
        (append
            (typhon@compile (car iter-list) params env tail?)
            (list (list (typhon-primitive-value (typhon@lookup sym env))))
            (typhon@compile-help sym (cdr iter-list) params env tail?))))

(define (typhon@map iter-list params env tail?)
    (if (null? iter-list)
        iter-list
        (cons
            (typhon@compile (car iter-list) params env tail?)
            (typhon@map (cdr iter-list) params env tail?))))

;; tail?:
;; Need to think about this a bit; the "tail?" parameter is meant to signify that
;; something can be turned into one of the 3-types of `ret` instructions that the
;; VM knows about. However, we cannot blindly pass tail? around; inside of forms
;; it doesn't matter; for instance, `(= x 10)` might be in the tail position,
;; but we don't want to pass `tail?` = `#t` in when compiling `x` or `10`, only
;; for the top-level `=` form.
(define (typhon@compile line params env tail?)
    (catch compile-error
        (if (null? line)
            '()
            (cond
                (vector? line)
                    (if tail?
                        (list (list 115 line))
                        (list (list 3 line)))
                (dict? line) 
                    (if tail? 
                        (list (list 115 (dict type: 'dict value: line)))
                        (list (list 3 (dict type: 'dict value: line))))
                ;(number? line)
                ;    (if tail?
                ;        (list (list 115 line))
                ;        (list (list 3 line)))
                (symbol? line) 
                    (let ((param-mem? (memq line params)))
                        (if (not (eq? param-mem? #f))
                            (if tail?
                                (list
                                    (list
                                        116
                                        (- 
                                            (length params)
                                            (length param-mem?))))
                                (list
                                    (list
                                        112 ;; fast load
                                        (- 
                                            (length params)
                                            (length param-mem?)))))
                            (if tail?
                                (list (list 116 line))
                                (list (list 31 line))))) ;; environment-load
                (pair? line) 
                    (let* ((fst (car line)) ;; decompose line into first & rest
                           (v (typhon@lookup fst env)) ;; find fst in env
                           (rst (cdr line))
                           (param-mem? (memq fst params)))
                        ;(display "in decompse LET; fst == ")
                        ;(write fst)
                        ;(display " and rst == " )
                        ;(write rst)
                        ;(display " and v == " )
                        ;(write v)
                        ;(newline)
                        ;(if (dict? v)
                        ;    (begin
                        ;        (display "type: ")
                        ;        (display (nth v 'type 'NOT-FOUND))
                        ;        (newline))
                        ;    #v)
                       (cond 
                            (not (eq? param-mem? #f))
                                (append
                                    (reverse-append 
                                        (typhon@map rst params env #f))
                                    (list (list 112 (- (length params) (length param-mem?))))
                                    (list (list 110 'param)))
                            (typhon-syntax? v) ;; primitive syntax
                                (cond
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-quote)
                                        (cond
                                            ; this case is covered by the generic tail? case...
                                            ;(and (null? (car rst)) tail?)
                                            ;    '((115 ()))
                                            tail? ;; e.g. tail pos, but *not* null
                                                (list (list 115 (car rst))) ;; could be combined with first case...
                                            (null? (car rst)) ;; e.g. null, but *not* tail pos
                                                '((4))
                                            else
                                                (list (list 3 (car rst))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-nth)
                                        (cond
                                            (= (length rst) 2)
                                                (append
                                                    '((4)) ; this should really be a typhon@error
                                                    (typhon@compile (cadr rst) params env #f)
                                                    (typhon@compile (car rst) params env #f)
                                                    '((55)))
                                            (= (length rst) 3)
                                                (append
                                                    (typhon@compile (caddr rst) params env #f)
                                                    (typhon@compile (cadr rst) params env #f)
                                                    (typhon@compile (car rst) params env #f)
                                                    '((55)))
                                            else
                                                (throw compile-error (make-typhon-error "incorrect arity for NTH")))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-plus)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 0))
                                                    (typhon@compile (car rst) params env #f)
                                                    (list (list (typhon-primitive-value (typhon@lookup '%+ env)))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env #f)
                                                    (typhon@compile-help '%+ (cdr rst) params env #f))
                                            else (list (list 3 0)))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-minus)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 0))
                                                    (typhon@compile (car rst) params env #f)
                                                    (list (list (typhon-primitive-value (typhon@lookup '%- env)))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env #f)
                                                    (typhon@compile-help '%- (cdr rst) params env #f))
                                            else (throw compile-error (make-typhon-error "minus fail")))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-mult)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 0))
                                                    (typhon@compile (car rst) params env #f)
                                                    (list (list (typhon-primitive-value (typhon@lookup '%* env)))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env #f)
                                                    (typhon@compile-help '%* (cdr rst) params env #f))
                                            else (list (list 3 1)))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-div)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 1))
                                                    (typhon@compile (car rst) params env #f)
                                                    (list (list (typhon-primitive-value (typhon@lookup '%/ env)))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env #f)
                                                    (typhon@compile-help '%/ (cdr rst) params env #f))
                                            else (throw compile-error (make-typhon-error "division fail")))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-numeq)
                                        (cond
                                            (= (length rst) 1)
                                                (list (list 3 #t))
                                            (> (length rst) 1)
                                                (append
                                                    (typhon@compile (car rst) params env #f)
                                                    (typhon@compile-help '%= (cdr rst) params env #f))
                                            else (throw compile-error (make-typhon-error "numeq fail")))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-define)
                                        ;; this almost certainly need not be an instruction...
                                        ;; just about all syntax could just be at the syntactic
                                        ;; level...
                                        (let ((name (car rst))
                                              (value (cadr rst)))
                                            (cond
                                                (pair? name) 
                                                    (append
                                                        (typhon@compile (cons 'fn (cons (cdar rst) (cdr rst))) params env #f)
                                                        (list (list 3 (caar rst)))
                                                        (list (list (typhon-syntax-name (typhon@lookup '%define env))))) 
                                                (symbol? name)
                                                    (append
                                                        (typhon@compile value params env #f)
                                                        (list (list 3 name))
                                                        (list (list (typhon-syntax-name (typhon@lookup '%define env)))))
                                                else (throw compile-error (make-typhon-error "DEFINE error: define SYMBOL VALUE | DEFINE PAIR S-EXPR*"))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-set)
                                        (let ((name (car rst))
                                              (value (cadr rst)))
                                           (if (symbol? name) 
                                                (append
                                                    (typhon@compile value params env #f)
                                                    (list (list 3 name))
                                                    (list (list (typhon-syntax-name (typhon@lookup '%set! env)))))
                                                (throw compile-error (make-typhon-error "SET!: set! SYMBOL S-EXPR*"))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-defsyn)
                                        (let ((name (car rst))
                                              (rules (cdr rst)))
                                            (typhon@add-env! name (list 'user-syntax rules) env)
                                            (list (list 107))) ;; %nop
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-defmac)
                                        #t
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-fn)
                                        ;; see, *here* we want to inspect if `tail?` is
                                        ;; true, and if so, use one of the return types,
                                        ;; instead of a load...
                                        (list
                                            (list 3 ;; load
                                                (compile-lambda rst env #f))
                                            (list (typhon-syntax-name (typhon@lookup '%makeclosure env))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-begin)
                                        (compile-begin rst params env tail?)
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-lt)
                                        (append 
                                            (typhon@compile (car rst) params env #f)
                                            (typhon@compile-help '%< (cdr rst) params env #f))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-gt)
                                        (append 
                                            (typhon@compile (car rst) params env #f)
                                            (typhon@compile-help '%> (cdr rst) params env #f))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-lte)
                                        (append 
                                            (typhon@compile (car rst) params env #f)
                                            (typhon@compile-help '%<= (cdr rst) params env #f))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-gte)
                                        (append 
                                            (typhon@compile (car rst) params env #f)
                                            (typhon@compile-help '%>= (cdr rst) params env #f))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-list)
                                        ;; if rst is null?, then generate a load-null instruction (4)
                                        ;; otherwise generate the instructions for the list, a length
                                        ;; and a call to %list
                                        (if (null? rst)
                                            (list (list 4))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env #f))
                                                (list (list 3 (length rst)))
                                                (list (list (typhon-syntax-name (typhon@lookup '%list env))))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-vector)
                                        ;; if rst is null?, then generate a load with an empty vector
                                        ;; otherwise generate the instructions for the vector, a length
                                        ;; and a call to %vector
                                        (if (null? rst)
                                            (list (list 3 (make-vector 0)))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env #f))
                                                (list (list 3 (length rst)))
                                                (list (list (typhon-syntax-name (typhon@lookup '%vector env))))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-dict)
                                        ;; if rst is null?, then generate a load with an empty dict
                                        ;; otherwise generate the instructions for the dict, a length
                                        ;; and a call to %dict
                                        (if (null? rst)
                                            ;; again, need to check if `tail?` is `#t` above this
                                            ;; so that we can possibly turn this into a `ret` instead
                                            ;; of a load...
                                            (list (list 3 (make-dict)))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env #f))
                                                (list (list 3 (length rst)))
                                                (list (list (typhon-syntax-name (typhon@lookup '%dict env))))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-string)
                                        (if (null? rst)
                                            (list (list 3 (make-string 0)))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env #f))
                                                (list (list 3 (length rst)))
                                                (list (list (typhon-syntax-name (typhon@lookup '%string env))))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-string-append)
                                        (if (null? rst)
                                            (list (list 3 (make-string 0)))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env #f))
                                                (list (list 3 (length rst)))
                                                (list (list (typhon-syntax-name (typhon@lookup '%string-append env))))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-append)
                                        (if (null? rst)
                                            (list (list 4))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env #f))
                                                (list (list 3 (length rst)))
                                                (list (list (typhon-syntax-name (typhon@lookup '%append env))))))
                                        
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-makevector)
                                        (with l (length rst)
                                            (cond
                                                (= l 1)
                                                    (append
                                                        '((4))
                                                        (typhon@compile (car rst) params env #f)
                                                        (list (list (typhon-syntax-name (typhon@lookup '%make-vector env)))))
                                                (= l 2)
                                                    (append
                                                        (reverse-append (typhon@map rst params env #f))
                                                        (list (list (typhon-syntax-name (typhon@lookup '%make-vector env)))))
                                                else (throw compile-error (make-typhon-error "make-vector len : INTEGER (v : SEXPR) => VECTOR"))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-makestring)
                                        (with l (length rst)
                                            (cond
                                                (= l 1)
                                                    (append
                                                        '((3 #\space))
                                                        (typhon@compile (car rst) params env #f)
                                                        (list (list (typhon-syntax-name (typhon@lookup '%make-string env)))))
                                                (= l 2)
                                                    (append
                                                        (reverse-append (typhon@map rst params env #f))
                                                        (list (list (typhon-syntax-name (typhon@lookup '%make-string env)))))
                                                else (throw compile-error (make-typhon-error "make-string len : INTEGER (c : CHAR) => STRING"))))
                                    (eq? (typhon-syntax-name v) 'primitive-syntax-if)
                                        ;; need to generate code for <cond>
                                        ;; add CMP instruction '(30)
                                        ;; generate code for <then>
                                        ;; generate code for <else>
                                        ;; add count to CMP instruction to jump to <else>
                                        ;; add count to <then> to skip <else>
                                        (let* ((<cond> (typhon@compile (car rst) params env #f))
                                               (<then> (typhon@compile (cadr rst) params env tail?)) ;; should we rely on this here?
                                               (<else> (typhon@compile (caddr rst) params env tail?))
                                               (then-len (+ (length <then>) 2)) ;; +2 in order to avoid the jump over else
                                               (else-len (+ (length <else>) 1)))
                                            ;; the `28 else-len` is a great opportunity to use a return
                                            ;; instruction here...
                                            ;(display "In syntax-if; tail is: ")
                                            ;(write tail?)
                                            ;(newline)
                                            (append <cond>
                                                (list (list 29 then-len)) ;; compare & jump
                                                <then>
                                                (list (list 28 else-len)) ;; jump else
                                                <else>)) 
                                    else 
                                        (throw compile-error (make-typhon-error "syntax has not been implemented at this time")))
                                (pair? fst) 
                                    ;; fst is a pair, so we just blindly attempt to compile it.
                                    ;; May cause an error that has to be caught in CALL. some lifting might fix this...
                                    (append (reverse-append (typhon@map rst params env #f))
                                            (typhon@compile fst params env #f)
                                            (list (list 110)))
                                (typhon@usyntax? v)
                                    (let ((syn (syntax-expand1 (cadr v) line)))
                                        ;(display "in typhon@usyntax? in typhon@compile. v == ")
                                        ;(write v)
                                        ;(display "\n and fst == ")
                                        ;(write fst)
                                        ;(display "\n and expansion == ")
                                        ;(write syn)
                                        ;(display "\n")
                                        (typhon@compile
                                            syn
                                            params
                                            env #f)) ;; TODO: don't use env, use the environment stored in the syntax object; NEW: nope; expand items in place?
                                (typhon@umacro? v)
                                    #f
                                (typhon-procedure? v) ;; need to add some method of checking proc arity here.
                                    (let* ((rlen (length rst)))
                                        (append
                                            (reverse-append (typhon@map rst params env #f))
                                            (list (list 16 (typhon-procedure-name v) rlen))))
                                (typhon-primitive? v) ;; primitive procedure
                                    ;; need to generate the list of HLAP code, reverse it
                                    ;; and flatten it. basically, if we have:
                                    ;; (cons (+ 1 2) (cons (+ 3 4) '()))
                                    ;; we need to:
                                    ;; (quote ())
                                    ;; (+ 3 4)
                                    ;; (cons)
                                    ;; (+ 1 2)
                                    ;; (cons)
                                    ;; this isn't the *most* efficient, but it is pretty easy
                                    (let ((min-arity (typhon-procedure-min-arity v))
                                          (max-arity (typhon-procedure-max-arity v))
                                          (opcode (typhon-primitive-value v))
                                          (rst-len (length rst)))
                                        (cond
                                            (= min-arity max-arity)
                                                (if (= min-arity rst-len)
                                                    (append
                                                        (reverse-append
                                                            (typhon@map rst params env #f))
                                                        (list (list opcode)))
                                                    (make-typhon-error (format "arity mismatch for ~a" fst)))
                                            (> max-arity min-arity)
                                                (if (and
                                                        (>= rst-len min-arity)
                                                        (<= rst-len max-arity))
                                                    (append
                                                        (reverse-append
                                                            (typhon@map rst params env #f))
                                                        (list
                                                            (list opcode rst-len)))
                                                    (make-typhon-error (format "arity mismatch for ~a" fst)))
                                            (= max-arity -1)
                                                (if (>= rst-len min-arity)
                                                    (append
                                                        (reverse-append
                                                            (typhon@map rst params env #f))
                                                        (list
                                                            (list opcode rst-len)))
                                                    (make-typhon-error (format "arity mismatch for ~a" fst)))))
                                (typhon-antipole-primitive? v) ;; primitive procedure
                                    ;; need to generate the list of HLAP code, but *not* reverse it
                                    (let ((min-arity (typhon-antipole-primitive-min-arity v))
                                          (max-arity (typhon-antipole-primitive-max-arity v))
                                          (opcode (typhon-antipole-primitive-value v))
                                          (rst-len (length rst)))
                                        (display "in antipolar primitive section...\n")
                                        (cond
                                            (= min-arity max-arity)
                                                (if (= min-arity rst-len)
                                                    (append
                                                        (apply
                                                            append
                                                            (typhon@map rst params env #f))
                                                        (list (list opcode)))
                                                    (make-typhon-error (format "arity mismatch for ~a" fst)))
                                            (> max-arity min-arity)
                                                (if (and
                                                        (>= rst-len min-arity)
                                                        (<= rst-len max-arity))
                                                    (append
                                                        (apply
                                                            append
                                                            (typhon@map rst params env #f))
                                                        (list
                                                            (list 3 rst-len)
                                                            (list opcode)))
                                                    (make-typhon-error (format "arity mismatch for ~a" fst)))
                                            (= max-arity -1)
                                                (if (>= rst-len min-arity)
                                                    (append
                                                        (apply
                                                            append
                                                            (typhon@map rst params env #f))
                                                        (list
                                                            (list 3 rst-len)
                                                            (list opcode)))
                                                    (make-typhon-error (format "arity mismatch for ~a" fst)))))
                                                
                                (typhon@lambda? v) ;; hydra closure; change this into (load-from-env fst) (call-from-stack) 
                                    (append
                                        (reverse-append
                                            (typhon@map rst params env #f))
                                            (list (list 31 fst))
                                            (list (list 110 'found)))
                                (typhon@continuation? v) ;; hydra continuation
                                    (append (reverse-append (typhon@map rst params env #f))
                                        (list (list 3 v))
                                        (list (list 108))) ;; 108 -> %ap
                                (symbol? fst) ;; fst is a symbol, but it has no mapping in our current env; write to environment-load
                                    (append
                                        (reverse-append
                                            (typhon@map rst params env #f)) 
                                            (list (list 31 fst))
                                            (list (list 110 'not-found)))
                                else (throw compile-error (make-typhon-error "error: the only applicable types are primitive procedures, closures & syntax"))))
                else 
                    (if tail?
                        (list (list 115 line))
                        (list (list 3 line)))))))

;; need to separate user values from what 
;; is returned in the eval...
(define (top-level-print x)
    " print #<foo> at the top level"
    (cond
        (typhon@lambda? x) (display "#<closure>")
        (typhon@continuation? x) (display "#<continuation>")
        (typhon-primitive? x) (display (format "#<primitive-procedure ~a>" (typhon-primitive-value x)))
        (typhon-procedure? x) (display (format "#<procedure ~a>" (typhon-procedure-name x)))
        (typhon-syntax? x) (display (format "#<syntax ~a>" (typhon-syntax-name x)))
        (typhon-error? x) (display (format "ERROR: ~a" (typhon-error-message x)))
        (typhon@usyntax? x) (display "#<syntax rules>")
        else (write x)))

(define (typhon@load-loop fh env dump)
    (let ((o (read fh)))
        (if (eq? o #e)
            #v
            (begin
                (typhon@eval o env dump) 
                (typhon@load-loop fh env dump)))))

(define (typhon@load src-file env dump)
    "an implementation of the primitive procedure load"
    (let ((f (open (coerce src-file :string) :read)))
        (typhon@load-loop f env dump)
        (close f)
        #v))

(define (typhon@help o env)
    (let ((res (typhon@lookup o env)))
        (if (typhon@lambda? res)
            (begin
                (display "params: ")
                (display (nth (cadr res) 2)))
            #v)))
                                    
(define (typhon@repl env dump)
    (display ";t ")
    (with inp (read)
     (cond 
        (and (eq? (type inp) "Pair") (eq? (car inp) 'unquote))
            (cond
                (eq? (cadr inp) 'exit) #v
                (eq? (cadr inp) 'q) #v
                (eq? (cadr inp) 'quit) #v
                (eq? (cadr inp) 'bye) #v
                (eq? (cadr inp) 'dribble) (begin (typhon@repl env dump))
                (eq? (cadr inp) 'save) (begin (typhon@repl env dump))
                (eq? (cadr inp) 'save-and-die) (begin (typhon@repl env dump))
                (eq? (cadr inp) 'a)
                    (with item (read)
                        (write (typhon@appropos item env))
                        (newline)
                        (typhon@repl env dump))
                (or
                    (eq? (cadr inp) 'h)
                    (eq? (cadr inp) 'help))
                    (with item (read)
                        (typhon@help item env)
                        (newline)
                        (typhon@repl env dump))
                (eq? (cadr inp) 'i)
                    (with item (read)
                        (write (typhon@lookup item env))
                        (newline)
                        (typhon@repl env dump))
                else (begin (display (format "Unknown command: ~a~%" (cadr inp))) (typhon@repl env dump)))
        (eof-object? inp)
            #v
        (not (pair? inp))
            (if (eq? inp #v)
                (typhon@repl env dump)
                (begin
                    (top-level-print (typhon@lookup inp env))
                    (display "\n")
                    (typhon@repl env dump)))
        else
            (with r (typhon@eval inp env dump) 
                (if (eq? r #v)
                 (typhon@repl env dump)
                 (begin
                    (if (typhon-error? r)
                        #v
                        (typhon@add-env! '_ r env))
                    (top-level-print r)
                    (display "\n")
                    (typhon@repl env dump)))))))

(define (typhon@main args)
    (let* ((e {})
          (dump (make-vector 1000 #v)))
          ;(dump (make-typhon-dump 0 v-dump)))
        (typhon@init-env e)
        (if (> (length args) 0)
            (begin
                (typhon@add-env! '*command-line* (cslice args 1 (length args)) (list e))
                (typhon@load (nth args 0) (list e) dump))
            (begin
                (display "\n\t()\n\t  ()\n\t()  ()\nDigamma/Typhon: 2014.3/r0\n")
                (typhon@add-env! '*command-line* '() (list e))
                (typhon@repl (list e) dump)))))

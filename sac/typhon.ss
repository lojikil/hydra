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
;; - DONE: good compilation mechanism for hydra@eval
;; - DONE: method for hydra@vm to manage things like (cons (car (cons 1 2)) (cdr (1 2)))
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
;; - define-instruction syntax that can be used to populate hydra@vm as well as 
;;   clean up redundancies in code (like manual calls to hydra@vm in each instruction)
;; - define a clean method of boxed representations of types, one that can be used from
;;   Vesta or E'. Not sure if this is a decent use of time here, in a VM, since that 
;;   should be a function of the run time, not the interpreter (we wouldn't want Ceres
;;   & Hydra to have different representations, for instance). Still, it would be nice
;;   if errors & other types can be simply encoded '(error "error"); SRFI-9, esp. if it
;;   has E' support, might be a good option (need to unbox types in E' though, for the
;;   most efficient representation, as well as unions).
;; - Make all instructions support (type . value) pairs, so as to avoid a situation where
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
;; the first two *can* be handled OOB by hydra@eval, but the third
;; cannot really be handled properly. It should be re-written to 
;; (load (1 2 3 4 5))
;; (car)
;; (load f)
;; (%define)
;; - SRFIs to be added: 9, 22, 34, 35, 36, 57, 60, 89, 88 (already done, via Vesta's run time)

;; Include files for C output
;; once SRFI-0 support is here, use that to make things a bit
;; nicer

(%include "murt.h" #f)

;; mini-prelude
;; should be removed once Enyalios supports load better...

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
(define-syntax pair? () ((pair? n) (eq? (type n) "Pair")))
(define-syntax vector? () ((vector? n) (eq? (type n) "Vector")))
(define-syntax dict? () ((dict? n) (eq? (type n) "Dictionary")))
(define-syntax symbol? () ((symbol? n) (eq? (type n) "Symbol")))
(define-syntax key? () ((key? n) (eq? (type n) "Key")))
(define-syntax number? () ((number? n) (eq? (type n) "Number")))
(define-syntax string? () ((string? n) (eq? (type n) "String")))
(define-syntax bool? () ((bool? n) (eq? (type n) "Boolean")))
(define-syntax goal? () ((goal? n) (eq? (type n) "Goal")))
(define (not x)
        (cond
            (eq? x #s) #u
            (eq? x #f) #t
            (eq? x #u) #s
            else #f))
(define-syntax zero? () ((zero? n) (= n 0)))
(define-syntax eof-object? () ((eof-object? n) (eq? n #e)))
(define-syntax void? () ((void? x) (eq? x #v)))

(define (zip xs ys)
	(if (null? xs)
		'()
		(cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys)))))

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
		(and (eq? (type x) "Pair") (eq? (type y) "Pair"))
			(if (equal? (car x) (car y))
				(equal? (cdr x) (cdr y))
				#f)
		(and (eq? (type x) "Vector") (eq? (type y) "Vector"))
			(if (= (length x) (length y))
				(vector-equal? x y 0)
				#f)
		(and (eq? (type x) "Number") (eq? (type y) "Number"))
			(= x y)
		else
			(eq? x y)))

(load "./experiments/sr.ss")
;; end mini-prelude.

(define-syntax hydra@instruction () 
    ((hydra@instruction c) (car c)))

(define-syntax hydra@operand ()
    ((hydra@operand c) (cadr c)))

(define-syntax hydra@lambda? ()  ((hydra@lambda? x)
    (and (pair? x) (eq? (car x) 'compiled-lambda))))

(define-syntax hydra@primitive? () ((hydra@primitive? x)
    (and (pair? x) (eq? (car x) 'primitive))))

(define-syntax hydra@syntax? () ((hydra@syntax? x)
    (and (pair? x) (eq? (car x) 'syntax))))

(define-syntax hydra@error? () ((hydra@error? x)
    (and (pair? x) (eq? (car x) 'error))))

(define-syntax hydra@continuation? () ((hydra@continuation? x)
    (and (pair? x) (eq? (car x) 'continuation))))

(define-syntax hydra@procedure? () ((hydra@procedure? x)
    (and (pair? x) (eq? (car x) 'procedure))))

(define-syntax hydra@usyntax? () ((hydra@usyntax? x)
    (and (pair? x) (eq? (car x) 'user-syntax)))) 

(define (hydra@umacro? x)
    #f)

(define (loop-set-env! env params vals)
    (if (null? params)
        vals
        (begin
            (cset! env (car params) (car vals))
            (loop-set-env! env (cdr params) (cdr vals)))))

(define (build-environment environment stack params)
    "Adds a new window to the environment, removes |params| items from the stack
     and binds those values in the new window. It returns a list of environment and
     the new stack."
    ;; rough match; doesn't take into account optional parameters.
    ;; would probably be better to have an inner function that iterates over
    ;; the parameters & returns those that match. It would then be easier to 
    ;; have optional parameters...
    (let ((ls (length stack)) (lp (length params)) (nu-env (make-dict)))
        (if (< ls lp)
            (error "non-optional parameters are not statisfied by stack items in build-environment")
            (if (= lp 0)
                (list (cons nu-env environment) (cdr stack))
                (with new-stack
                    (loop-set-env!
                        nu-env
                        params stack)
                    (list (cons nu-env environment) new-stack))))))

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
                    (hydra@load (car args) env dump)
                (= arity 2)
                    (hydra@load (car args) (cadr args) dump)
                else
                    (error "Incorrect arity for procedure: load"))
        else
            (error (format "unknown procedure \"~a\"" proc))))

(define (hydra@vm code env ip stack dump)
     " process the actual instructions of a code object; the basic idea is that
       the user enters:
       h; (car (cdr (cons 1 (cons 2 '()))))
       which is compiled by hydra@eval into:
       (4)   ;; nil
       (3 2) ;; load 2
       (2)   ;; cons
       (3 1) ;; load 1
       (2)   ;; cons
       (1)   ;; cdr
       (0)   ;; car
       
       which hydra@vm can then interpret in a tail-call fashion.
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
     (cond
        (or (eq? (type (car stack)) "Error")
            (hydra@error? (car stack)))
            (car stack)
        (>= ip (length code))
        (if (= (car dump) 0)
            (car stack)
            (let ((top-dump (cadr dump))
                  (offset (car dump)))
                (hydra@vm
                    (nth top-dump (- offset 1))
                    (nth top-dump (- offset 2))
                    (+ (nth top-dump (- offset 3)) 1)
                    (cons (car stack) (nth top-dump (- offset 4)))
                    (list (- offset 4) top-dump))))
         else
         (let* ((c (nth code ip))
                (instr (hydra@instruction c)))
              ;(display (format "current ip: ~n~%" ip))
              ;(display "current instruction: ")
              ;(display (nth code ip))
              ;(display "\n")
              ;(display "current stack: ")
              ;(write stack)
              ;(display "\n")
              ;(display "current dump: ")
              ;(write dump)
              ;(display "\n")
              (case instr 
                    (0) ;; car
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (car (car stack)) (cdr stack)) dump)
                    (1) ;; cdr
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (cdr (car stack)) (cdr stack)) dump)
                    (2) ;; cons
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (cons (car stack)
                                                (cadr stack))
                                       (cddr stack)) dump)
                    (3) ;; load
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (hydra@operand c) stack) dump)
                    (4) ;; nil
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons '() stack) dump)
                    (5) ;; -
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (- (cadr stack) (car stack)) (cddr stack)) dump)
                    (6) ;; +
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (+ (car stack) (cadr stack)) (cddr stack)) dump)
                    (7) ;; * 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (* (car stack) (cadr stack)) (cddr stack)) dump)
                    (8) ;; / 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (/ (cadr stack) (car stack)) (cddr stack)) dump)
                    (9) ;;  < 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (< (cadr stack) (car stack)) (cddr stack)) dump)
                    (10) ;; >
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (> (cadr stack) (car stack)) (cddr stack)) dump)
                    (11) ;; <= 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (<= (cadr stack) (car stack)) (cddr stack)) dump)
                    (12) ;; >= 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (>= (cadr stack) (car stack)) (cddr stack)) dump)
                    (13) ;; length
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (length (car stack)) (cdr stack)) dump)
                    (14) ;; exact?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (exact? (car stack)) (cdr stack)) dump)
                    (15) ;; inexact?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (inexact? (car stack)) (cdr stack)) dump)
                    (16) ;; procedure call
                    (let* ((arity (caddr c))
                           (args (cslice stack 0 arity))
                           (stk (cslice stack arity (length (cdr stack))))
                           (ret (procedure-runner (hydra@operand c) arity args env dump)))
                           (hydra@vm
                                code
                                env
                                (+ ip 1)
                                (cons ret stk)
                                dump))
                    (18) ;; real?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (real? (car stack)) (cdr stack)) dump)
                    (19) ;; integer?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (integer? (car stack)) (cdr stack)) dump)
                    (20) ;; complex?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (complex? (car stack)) (cdr stack)) dump)
                    (21) ;; rational?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (rational? (car stack)) (cdr stack)) dump)
                    (22) ;; gcd
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (gcd (car stack) (cadr stack)) (cddr stack)) dump)
                    (23) ;; lcm
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (lcm (car stack) (cadr stack)) (cddr stack)) dump)
                    (24) ;; numerator 
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (numerator (car stack)) (cdr stack)) dump)
                    (25) ;; denomenator
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (denomenator (car stack)) (cdr stack)) dump)
                    (26) ;; = 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (= (car stack) (cadr stack)) (cddr stack)) dump)
                    (27) ;; eq?
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (eq? (car stack) (cadr stack)) (cddr stack)) dump)
                    (28) ;; jump
                        (hydra@vm code
                                 env
                                 (+ ip (hydra@operand c))
                                 stack dump)
                    (29) ;; cmp
                        (if (car stack) ;; if the top of the stack is true
                            (hydra@vm code env (+ ip 1) (cdr stack) dump) ;; jump to the <then> portion
                            (hydra@vm code env (+ ip (hydra@operand c)) (cdr stack) dump))
                    (30) ;; call
                        ;; need to make call check it's operand now...
                        (let ((call-proc (hydra@operand c)))
                            ;(display "in <else> of CALL\n")
                            ;(display "c == ")
                            ;(write c)
                            ;(newline)
                            ;(display "(car stack) == ")
                            ;(write (car stack))
                            ;(display "\n")
                            (if (symbol? call-proc)
                                (set! call-proc (hydra@lookup call-proc env))
                                #v)
                            (cond
                                (hydra@error? call-proc)
                                    call-proc
                                (hydra@lambda? call-proc)
                                    ;; create a list from the current registers, cons this to dump, and 
                                    ;; recurse over hydra@vm. 
                                    ;; need to support CALLing primitives too, since they could be passed
                                    ;; in to HOFs...
                                    (if (> (car dump) (length (cadr dump)))
                                        (error "Dump stack overflow")
                                        (let ((env-and-stack (build-environment (nth (cadr call-proc) 0) stack (nth (cadr call-proc) 2)))
                                              (v-dump (cadr dump))
                                              (offset (car dump)))
                                            (cset! v-dump offset (cadr env-and-stack))
                                            (cset! v-dump (+ offset 1) ip)
                                            (cset! v-dump (+ offset 2) env)
                                            (cset! v-dump (+ offset 3) code)
                                            (hydra@vm
                                                (nth (cadr call-proc) 1)
                                                (car env-and-stack)
                                                0 '() 
                                                (list (+ offset 4) v-dump))))
                                (hydra@primitive? (car stack)) ;; if primitives stored arity, slicing would be easy...
                                    (begin
                                        (display "in hydra@primitive\n\t")
                                        (display (car stack))
                                        (display "\n")
                                        #t)
                                ;;(hydra@procedure? (car stack))
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
                        (with r (hydra@lookup (hydra@operand c) env)
                            (if (hydra@error? r)
                                r
                                (hydra@vm
                                    code 
                                    env
                                    (+ ip 1) 
                                    (cons r stack)
                                    dump)))
                    (32) ;; tail-call 
                        (if (and (not (null? stack)) (eq? (caar stack) 'compiled-lambda))
                            (hydra@vm
                                (nth (cdar stack) 0)
                                (nth (cdar stack) 1)
                                0 '() 
                                dump)
                            #f)
                    (33) ;; %define
                        (begin
                            (hydra@add-env! (car stack) (cadr stack) env)
                            (hydra@vm
                                code env (+ ip 1)
                                (cons #v stack)
                                dump))
                    (34) ;; %set!
                        (begin
                            (hydra@set-env! (car stack) (cadr stack) env)
                            (hydra@vm
                                code env (+ ip 1)
                                (cons #v stack)
                                dump))
                    (35) ;; ceil
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (ceil (car stack)) (cdr stack)) dump)
                    (36) ;; floor
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (floor (car stack)) (cdr stack)) dump)
                    (37) ;; truncate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (truncate (car stack)) (cdr stack)) dump)
                    (38) ;; round
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (round (car stack)) (cdr stack)) dump)
                    (39) ;; inexact->exact
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (inexact->exact (car stack)) (cdr stack)) dump)
                    (40) ;; quotient
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (quotient (cadr stack) (car stack)) (cddr stack)) dump)
                    (41) ;; modulo
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (modulo (cadr stack) (car stack)) (cddr stack)) dump)
                    (42) ;; &
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (& (cadr stack) (car stack)) (cddr stack)) dump)
                    (43) ;; |
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (| (cadr stack) (car stack)) (cddr stack)) dump)
                    (44) ;; ^
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (^ (cadr stack) (car stack)) (cddr stack)) dump)
                    (45) ;; ~
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (~ (car stack)) (cdr stack)) dump)
                    (46) ;; %list
                        ;; take N items off the stack, create a list, and return it
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (cslice (cdr stack) 0 (car stack))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                    (47) ;; %vector
                        ;; take N items off the stack, create a list, and return it
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (coerce (cslice (cdr stack) 0 (car stack)) 'vector)
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                    (48) ;; %make-vector
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-vector (car stack) (cadr stack)) (cddr stack)) dump)
                    (49) ;; %make-string
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-string (car stack) (cadr stack)) (cddr stack)) dump)
                    (50) ;; %string
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply string (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                    (51) ;; %append
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                    (52) ;; first
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (first (car stack)) (cdr stack)) dump)
                    (53) ;; rest
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (rest (car stack)) (cdr stack)) dump)
                    (54) ;; ccons
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (ccons (cadr stack) (car stack)) (cddr stack)) dump)
                    (55) ;; %nth
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (nth (caddr stack) (cadr stack) (car stack)) (cdddr stack)) dump)
                    (56) ;; keys
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (keys (car stack)) (cdr stack)) dump)
                    (57) ;; partial-key?
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (partial-key? (cadr stack) (car stack)) (cddr stack)) dump)
                    (58) ;; cset!
                        (begin
                            (cset! (caddr stack) (cadr stack) (car stack))
                            (hydra@vm code
                                env
                                (+ ip 1)
                                (cons #v (cdddr stack)) dump))
                    (59) ;; empty?
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (empty? (car stack)) (cdr stack)) dump)
                    (60) ;; gensym
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (gensym (car stack)) (cdr stack)) dump)
                    (61) ;; imag-part
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (imag-part (car stack)) (cdr stack)) dump)
                    (62) ;; real-part
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (real-part (car stack)) (cdr stack)) dump)
                    (63) ;; make-rectangular
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-rectangular (car stack) (cadr stack)) (cddr stack)) dump)
                    (64) ;; make-polar
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-polar (car stack) (cadr stack)) (cddr stack)) dump)
                    (65) ;; magnitude
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (magnitude (car stack)) (cdr stack)) dump)
                    (66) ;; argument
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (argument (car stack)) (cdr stack)) dump)
                    (67) ;; conjugate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (conjugate (car stack)) (cdr stack)) dump)
                    (68) ;; conjugate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (conjugate! (car stack)) (cdr stack)) dump)
                    (69) ;; polar->rectangular
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (polar->rectangular (car stack)) (cdr stack)) dump)
                    (70) ;; rectangular->polar
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (rectangular->polar (car stack)) (cdr stack)) dump)
                    (71) ;; sin
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (sin (car stack)) (cdr stack)) dump)
                    (72) ;; cos
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cos (car stack)) (cdr stack)) dump)
                    (73) ;; tan
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (tan (car stack)) (cdr stack)) dump)
                    (74) ;; asin
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (asin (car stack)) (cdr stack)) dump)
                    (75) ;; acos
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (acos (car stack)) (cdr stack)) dump)
                    (76) ;; atan
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (atan (car stack)) (cdr stack)) dump)
                    (77) ;; atan2
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (atan2 (cadr stack) (car stack)) (cddr stack)) dump)
                    (78) ;; sinh
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (sinh (car stack)) (cdr stack)) dump)
                    (79) ;; cosh
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cosh (car stack)) (cdr stack)) dump)
                    (80) ;; tanh
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (tanh (car stack)) (cdr stack)) dump)
                    (81) ;; exp
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp (car stack)) (cdr stack)) dump)
                    (82) ;; ln
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (ln (car stack)) (cdr stack)) dump)
                    (83) ;; abs
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (abs (car stack)) (cdr stack)) dump)
                    (84) ;; sqrt
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (sqrt (car stack)) (cdr stack)) dump)
                    (85) ;; exp2
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (86) ;; expm1
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (expm1 (car stack)) (cdr stack)) dump)
                    (87) ;; log2
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (log2 (car stack)) (cdr stack)) dump)
                    (88) ;; log10
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (log10 (car stack)) (cdr stack)) dump)
                    (89) ;; <<
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (<< (car stack) (cadr stack)) (cddr stack)) dump)
                    (90) ;; >>
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (>> (car stack) (cadr stack)) (cddr stack)) dump)
                    (91) ;; %string-append
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply string-append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1)))
                            dump)
                    (92) ;; assq
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (assq (car stack) (cadr stack)) (cddr stack)) dump)
                    (93) ;; memq
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (memq (car stack) (cadr stack)) (cddr stack)) dump)
                    (94) ;; %dict
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply dict (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1)))
                            dump)
                    (95) ;; make-dict
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-dict) stack) dump)
                    (96) ;; dict-has?
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (dict-has? (car stack) (cadr stack)) (cddr stack)) dump)
                    (97) ;; coerce
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (coerce (car stack) (cadr stack)) (cddr stack)) dump)
                    (98) ;; cupdate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cupdate (car stack) (cadr stack) (caddr stack)) (cdddr stack)) dump)
                    (99) ;; cslice
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cslice (car stack) (cadr stack) (caddr stack)) (cdddr stack)) dump)
                    (100) ;; tconc!
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (101) ;; make-tconc
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (102) ;; tconc-list
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (103) ;; tconc->pair
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (104) ;; tconc-splice
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (105) ;; rationalize
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (rationalize (car stack) (cadr stack)) (cddr stack)) dump)
                    (106) ;; call/cc
                        (let ((retcode (hydra@vm (cons (list 3 (car stack)) (list (list 30)))
                                        env
                                        0
                                        (cons (list 'continuation (copy-code code ip 0) ip env stack dump) '())
                                        '())))
                         (hydra@vm code
                            env
                            (+ ip 1)
                            (cons retcode (cdr stack))
                            dump))
                    (107) ;; %nop
                        (hydra@vm code
                            env
                            (+ ip 1)
                            stack
                            dump)
                    (108) ;; %ap
                        (let ((cont-code (car stack))
                              (v (cadr stack)))
                         (hydra@vm 
                            (nth cont-code 1)
                            (nth cont-code 3)
                            0
                            (cons
                                v
                                (nth cont-code 4))
                            (nth cont-code 5)))
                    (109) ;; %makeclosure
                        ;; makeclosure should rebuild the lambda that it is "enclosing"
                        ;; to ensure clean enclosing, rather than using cset, which just
                        ;; gives us the same problem we have now... duh (well, when you
                        ;; think about it, sure, but the naive approach? sure).
                        (begin
                            ;;(cset! (cadar stack) 0 env)
                            (hydra@vm
                                code
                                env
                                (+ ip 1)
                                (cons
                                    (list 'compiled-lambda
                                        (vector
                                            (srfi1-list-copy env)
                                            (nth (cadar stack) 1)
                                            (nth (cadar stack) 2)))
                                    (cdr stack))
                                dump))
                    (110) ;; call from stack
                        (let ((call-proc (car stack)))
                            ;(display "call-proc == ")
                            ;(write call-proc)
                            ;(newline)
                            ;(display "(car stack) == ")
                            ;(write (car stack))
                            ;(display "\n")
                            (cond
                                (hydra@error? call-proc)
                                    (begin
                                        (display "error: ")
                                        (write call-proc)
                                        (newline)
                                        call-proc)
                                (hydra@lambda? call-proc)
                                    ;; create a list from the current registers, cons this to dump, and 
                                    ;; recurse over hydra@vm. 
                                    ;; need to support CALLing primitives too, since they could be passed
                                    ;; in to HOFs...
                                    (if (> (car dump) (length (cadr dump)))
                                        (error "Dump stack overflow")
                                        (let ((env-and-stack (build-environment (nth (cadr call-proc) 0) (cdr stack) (nth (cadr call-proc) 2)))
                                              (v-dump (cadr dump))
                                              (offset (car dump)))
                                            ;(display "in let?\n")
                                            (cset! v-dump offset (cadr env-and-stack))
                                            (cset! v-dump (+ offset 1) ip)
                                            (cset! v-dump (+ offset 2) env)
                                            (cset! v-dump (+ offset 3) code)
                                            (hydra@vm
                                                (nth (cadr call-proc) 1)
                                                (car env-and-stack)
                                                0 '() 
                                                (list (+ offset 4) v-dump))))
                                (hydra@primitive? (car stack)) ;; if primitives stored arity, slicing would be easy...
                                    #t
                                else
                                    (error "non-applicable CALL-STACK argument")))))))

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
;     + this allows hydra@compile to know the proper arity, and signal an error.
;     + it also means that hydra@vm has to suddenly change: it must now unpack the actual opcode
;       before running (this might not be too bad...)
; 1 - encode the arity of primitives in a separate array.
;     + this allows hydra@compile to remain unchanged, and only minimal changes to hydra@vm
;     + this does not confer the benefits that the above does (that hydra@compile can
;       know about the arity of primitives & signal failure during code generation).

(define (hydra@init-env env)
    (dict-set! env "car" '(primitive . 0))
    (dict-set! env "call/cc" '(primitive . 106))
    (dict-set! env "cdr" '(primitive . 1))
    (dict-set! env "cons" '(primitive . 2))
    (dict-set! env "conjugate" '(primitive . 67))
    (dict-set! env "conjugate!" '(primitive . 68))
    (dict-set! env "complex?" '(primitive . 20))
    (dict-set! env "cos" '(primitive . 72))
    (dict-set! env "cosh" '(primitive . 79))
    (dict-set! env "coerce" '(primitive . 97))
    (dict-set! env "ceil" '(primitive . 35))
    (dict-set! env "ccons" '(primitive . 54))
    (dict-set! env "cset!" '(primitive . 58))
    (dict-set! env "cslice" '(primitive . 99))
    (dict-set! env "cupdate" '(primitive . 98))
    (dict-set! env "%load" '(primitive . 3))
    (dict-set! env "%list" '(primitive . 46))
    (dict-set! env "%nil" '(primitive . 4))
    (dict-set! env "%nop" '(primitive . 107))
    (dict-set! env "%-" '(primitive . 5))
    (dict-set! env "%+" '(primitive . 6))
    (dict-set! env "%*" '(primitive . 7))
    (dict-set! env "%/" '(primitive . 8))
    (dict-set! env "%<" '(primitive . 9))
    (dict-set! env "%<=" '(primitive . 11))
    (dict-set! env "%>" '(primitive . 10))
    (dict-set! env "%>=" '(primitive . 12))
    (dict-set! env "%=" '(primitive . 26))
    (dict-set! env "%jmp" '(primitive . 28))
    (dict-set! env "%cmp" '(primitive . 29))
    (dict-set! env "%call" '(primitive . 30))
    (dict-set! env "%env-load" '(primitive . 31))
    (dict-set! env "%tail-call" '(primitive . 32))
    (dict-set! env "%define" '(primitive . 33))
    (dict-set! env "%set!" '(primitive . 34))
    (dict-set! env "%string" '(primitive . 50))
    (dict-set! env "%vector" '(primitive . 47))
    (dict-set! env "%make-vector" '(primitive . 48))
    (dict-set! env "%make-string" '(primitive . 49))
    (dict-set! env "%ap" '(primitive . 108))
    (dict-set! env "%makeclosure" '(primitive . 109))
    (dict-set! env "%append" '(primitive . 51))
    (dict-set! env "if" '(syntax . primitive-syntax-if))
    (dict-set! env "inexact?" '(primitive . 15))
    (dict-set! env "inexact->exact" '(primitive . 39))
    (dict-set! env "integer?" '(primitive . 19))
    (dict-set! env "imag-part" '(primitive . 61))
    (dict-set! env "fn" '(syntax . primitive-syntax-fn))
    (dict-set! env "begin" '(syntax . primitive-syntax-begin))
    (dict-set! env "floor" '(primitive . 36))
    (dict-set! env "first" '(primitive . 52))
    (dict-set! env "lambda" '(syntax . primitive-syntax-fn))
    (dict-set! env "length" '(primitive . 13))
    (dict-set! env "lcm" '(primitive . 23))
    (dict-set! env "list" '(syntax . primitive-syntax-list))
    (dict-set! env "ln" '(primitive . 82))
    (dict-set! env "log2" '(primitive . 87))
    (dict-set! env "log10" '(primitive . 88))
    (dict-set! env "quote" '(syntax . primitive-syntax-quote))
    (dict-set! env "quotient" '(primitive . 40))
    (dict-set! env "quasi-quote" '(syntax . primitive-syntax-qquote))
    (dict-set! env "unquote" '(syntax . primitve-syntax-unquote))
    (dict-set! env "unquote-splice" '(syntax . primitive-syntax-unqsplice))
    (dict-set! env "exact?" '(primitive . 14))
    (dict-set! env "exp" '(primitive . 81))
    (dict-set! env "exp2" '(primitive . 85))
    (dict-set! env "expm1" '(primitive . 86))
    (dict-set! env "eq?" '(primitive . 27))
    (dict-set! env "empty?" '(primitive . 59))
    (dict-set! env "display" '(procedure . "display"))
    (dict-set! env "newline" '(procedure . "newline"))
    (dict-set! env "%dict" '(primitive . 94))
    (dict-set! env "dict" '(syntax . primitive-syntax-dict))
    (dict-set! env "dict-has?" '(primitive . 96))
    (dict-set! env "denomenator" '(primitive . 25))
    (dict-set! env "define" '(syntax . primitive-syntax-define))
    (dict-set! env "define-syntax" '(syntax . primitive-syntax-defsyn))
    (dict-set! env "define-macro" '(syntax . primitive-syntax-defmac))
    (dict-set! env "apply" '(primitive . 17))
    (dict-set! env "append" '(syntax . primitive-syntax-append))
    (dict-set! env "argument" '(primitive . 66))
    (dict-set! env "asin" '(primitive . 74))
    (dict-set! env "assq" '(primitive . 92))
    (dict-set! env "acos" '(primitive . 75))
    (dict-set! env "atan" '(primitive . 76))
    (dict-set! env "atan2" '(primitive . 77))
    (dict-set! env "abs" '(primitive . 83))
    (dict-set! env "real?" '(primitive . 18))
    (dict-set! env "real-part" '(primitive . 62))
    (dict-set! env "rest" '(primitive . 53))
    (dict-set! env "rectangular->polar" '(primitive . 70))
    (dict-set! env "rational?" '(primitive . 21))
    (dict-set! env "rationalize" '(primitive . 105))
    (dict-set! env "round" '(primitive . 38))
    (dict-set! env "gcd" '(primitive . 22))
    (dict-set! env "gensym" '(primitive . 60))
    (dict-set! env "numerator" '(primitive . 24))
    (dict-set! env "nth" '(primitive . 55))
    (dict-set! env "+" '(syntax . primitive-syntax-plus))
    (dict-set! env "-" '(syntax . primitive-syntax-minus))
    (dict-set! env "*" '(syntax . primitive-syntax-mult))
    (dict-set! env "/" '(syntax . primitive-syntax-div))
    (dict-set! env "<" '(syntax . primitive-syntax-lt))
    (dict-set! env "<=" '(syntax . primitive-syntax-lte))
    (dict-set! env "<<" '(primitive . 89))
    (dict-set! env ">" '(syntax . primitive-syntax-gt))
    (dict-set! env ">=" '(syntax . primitive-syntax-gte))
    (dict-set! env ">>" '(primitive . 90))
    (dict-set! env "=" '(syntax . primitive-syntax-numeq))
    (dict-set! env "set!" '(syntax . primitive-syntax-set))
    (dict-set! env "string" '(syntax . primitive-syntax-string))
    (dict-set! env "%string-append" '(primitive . 91))
    (dict-set! env "sin" '(primitive . 71))
    (dict-set! env "sinh" '(primitive . 78))
    (dict-set! env "sqrt" '(primitive . 84))
    (dict-set! env "truncate" '(primitive . 37))
    (dict-set! env "tan" '(primitive . 73))
    (dict-set! env "tanh" '(primitive . 80))
    (dict-set! env "tconc!" '(primitive . 100))
    (dict-set! env "tconc-list" '(primitive . 102))
    (dict-set! env "tconc->pair" '(primitive . 103))
    (dict-set! env "tconc-splice" '(primitive . 104))
    (dict-set! env "modulo" '(primitive . 41))
    (dict-set! env "make-vector" '(syntax . primitive-syntax-makevector))
    (dict-set! env "make-string" '(syntax . primitive-syntax-makestring))
    (dict-set! env "make-rectangular" '(primitive . 63))
    (dict-set! env "make-polar" '(primitive . 64))
    (dict-set! env "make-dict" '(primitive . 95))
    (dict-set! env "make-tconc" '(primitive . 101))
    (dict-set! env "magnitude" '(primitive . 65))
    (dict-set! env "memq" '(primitive . 93))
    (dict-set! env "&" '(primitive . 42))
    (dict-set! env "|" '(primitive . 43))
    (dict-set! env "^" '(primitive . 44))
    (dict-set! env "~" '(primitive . 45))
    (dict-set! env "vector" '(syntax . primitive-syntax-vector))
    (dict-set! env "keys" '(primitive . 56))
    (dict-set! env "partial-key?" '(primitive . 57))
    (dict-set! env "polar->rectangular" '(primitive . 69))
    (dict-set! env "load" '(procedure . "load"))
    (dict-set! env "read" '(procedure . "read"))
    (dict-set! env "write" '(procedure . "write"))
    (dict-set! env "foo" '(procedure . "fo"))
    (dict-set! env "write-buffer" '(procedure . "write-buffer"))
    (dict-set! env "read-buffer" '(procedure . "read-buffer"))
    (dict-set! env "read-string" '(procedure . "read-string")))

(define (hydra@lookup item env)
    " look up item in the current environment, returning #f for not found"
    (cond
        (not (symbol? item)) item ;; to support ((fn (x) (+ x x)) (+ x x) 3)
        (null? env) (hydra@error (format "unbound variable: ~a" item)) 
        (dict-has? (car env) item) (nth (car env) item) 
        else (hydra@lookup item (cdr env))))

(define (compile-begin lst env)
    (if (null? lst)
        '()
        (append
            (hydra@compile (car lst) env)
            (compile-begin (cdr lst) env))))

(define (compile-lambda rst env)
    (list 'compiled-lambda
        (vector
            env
            (compile-begin (cdr rst) env)
            (car rst)))) 

(define (hydra@add-env! name value environment)
    " adds name to the environment, but also returns
      (load #v), so that the compiler adds the correct
      value (this is in the semantics of Vesta, so I thought
      it should be left in Hydra as well)"
    (cset! (car environment) name value))

(define (hydra@set-env! name value environment)
    " sets a value in the current environment, and returns
      an error if that binding has not been previously defined"
    (cond
        (null? environment) (hydra@error (format "SET! error: undefined name \"~a\"" name))
        (dict-has? (car environment) name)
            (cset! (car environment) name value)
        else (hydra@set-env! name value (cdr environment))))

(define (reverse-append x)
    "append but in reverse"
    (cond
        (null? x) x
        (null? (cdr x)) (car x)
        else (append (reverse-append (cddr x)) (cadr x) (car x))))

(define (show x m) (display m) (display x) (display "\n") x)

(define (hydra@error msg)
    "simple, hydra specific errors"
    (list 'error msg))

(define (hydra@eval line env dump)
    "simple wrapper around hydra@vm & hydra@compile"
    (hydra@vm (hydra@compile line env) env 0 '() dump))

(define (hydra@compile-help sym iter-list env)
    " a helper function for hydra@compile, which collects
      the old use of append-map into a single function that
      Eprime can compile (still haven't added HOFs to E'...
      embarrassing, I know)
    "
    (if (null? iter-list)
        iter-list
        (append
            (hydra@compile (car iter-list) env)
            (list (list (cdr (hydra@lookup sym env))))
            (hydra@compile-help sym (cdr iter-list) env))))

(define (hydra@map iter-list env)
    (if (null? iter-list)
        iter-list
        (cons
            (hydra@compile (car iter-list) env)
            (hydra@map (cdr iter-list) env))))

(define (hydra@compile line env)
    (if (null? line)
        '()
        (cond
            (vector? line) (list (list 3 line))
            (dict? line) (list (list 3 line) )
            (symbol? line) (list (list 31 line)) ;; environment-load
            (pair? line) 
                (let* ((fst (car line)) ;; decompose line into first & rest
                       (v (hydra@lookup fst env)) ;; find fst in env
                       (rst (cdr line))) 
                   (cond 
                        (hydra@syntax? v) ;; primitive syntax
                            (cond
                                (eq? (cdr v) 'primitive-syntax-quote)
                                    (if (null? (car rst))
                                        '((4))
                                        (list (list 3 (car rst))))
                                (eq? (cdr v) 'primitive-syntax-plus)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 0))
                                                (hydra@compile (car rst) env)
                                                (list (list (hydra@lookup '%+ env))))
                                        (> (length rst) 1)
                                            (append 
                                                (hydra@compile (car rst) env)
                                                (hydra@compile-help '%+ (cdr rst) env))
                                        else (list (list 3 0)))
                                (eq? (cdr v) 'primitive-syntax-minus)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 0))
                                                (hydra@compile (car rst) env)
                                                (list (list (hydra@lookup '%- env))))
                                        (> (length rst) 1)
                                            (append 
                                                (hydra@compile (car rst) env)
                                                (hydra@compile-help '%- (cdr rst) env))
                                        else (error "minus fail"))
                                (eq? (cdr v) 'primitive-syntax-mult)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 0))
                                                (hydra@compile (car rst) env)
                                                (list (list (hydra@lookup '%* env))))
                                        (> (length rst) 1)
                                            (append 
                                                (hydra@compile (car rst) env)
                                                (hydra@compile-help '%* (cdr rst) env))
                                        else (list (list 3 1)))
                                (eq? (cdr v) 'primitive-syntax-div)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 1))
                                                (hydra@compile (car rst) env)
                                                (list (list (hydra@lookup '%/ env))))
                                        (> (length rst) 1)
                                            (append 
                                                (hydra@compile (car rst) env)
                                                (hydra@compile-help '%/ (cdr rst) env))
                                        else (error "division fail"))
                                (eq? (cdr v) 'primitive-syntax-numeq)
                                    (cond
                                        (= (length rst) 1)
                                            (list (list 3 #t))
                                        (> (length rst) 1)
                                            (append
                                                (hydra@compile (car rst) env)
                                                (hydra@compile-help '%= (cdr rst) env))
                                        else (error "numeq fail"))
                                (eq? (cdr v) 'primitive-syntax-define)
                                    (let ((name (car rst))
                                          (value (cadr rst)))
                                        (cond
                                            (pair? name) 
                                                (append
                                                    (hydra@compile (cons 'fn (cons (cdar rst) (cdr rst))) env)
                                                    (list (list 3 (caar rst)))
                                                    (list (list (cdr (hydra@lookup '%define env))))) 
                                            (symbol? name)
                                                (append
                                                    (hydra@compile value env)
                                                    (list (list 3 name))
                                                    (list (list (cdr (hydra@lookup '%define env)))))
                                            else (error "DEFINE error: define SYMBOL VALUE | DEFINE PAIR S-EXPR*")))
                                (eq? (cdr v) 'primitive-syntax-set)
                                    (let ((name (car rst))
                                          (value (cadr rst)))
                                       (if (symbol? name) 
                                            (append
                                                (hydra@compile value env)
                                                (list (list 3 name))
                                                (list (list (cdr (hydra@lookup '%set! env)))))
                                            (error "SET!: set! SYMBOL S-EXPR*")))
                                (eq? (cdr v) 'primitive-syntax-defsyn)
                                    (let ((name (car rst))
                                          (rules (cdr rst)))
                                        (hydra@add-env! name (list 'user-syntax rules) env)
                                        (list (list 107))) ;; %nop
                                (eq? (cdr v) 'primitive-syntax-defmac)
                                    #t
                                (eq? (cdr v) 'primitive-syntax-fn)
                                    (list
                                        (list 3 ;; load
                                            (compile-lambda rst env))
                                        (list (cdr (hydra@lookup '%makeclosure env))))
                                (eq? (cdr v) 'primitive-syntax-begin)
                                    (compile-begin rst env)
                                (eq? (cdr v) 'primitive-syntax-lt)
                                    (append 
                                        (hydra@compile (car rst) env)
                                        (hydra@compile-help '%< (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-gt)
                                    (append 
                                        (hydra@compile (car rst) env)
                                        (hydra@compile-help '%> (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-lte)
                                    (append 
                                        (hydra@compile (car rst) env)
                                        (hydra@compile-help '%<= (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-gte)
                                    (append 
                                        (hydra@compile (car rst) env)
                                        (hydra@compile-help '%>= (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-list)
                                    ;; if rst is null?, then generate a load-null instruction (4)
                                    ;; otherwise generate the instructions for the list, a length
                                    ;; and a call to %list
                                    (if (null? rst)
                                        (list (list 4))
                                        (append
                                            (reverse-append
                                                (hydra@map rst env))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (hydra@lookup '%list env))))))
                                (eq? (cdr v) 'primitive-syntax-vector)
                                    ;; if rst is null?, then generate a load with an empty vector
                                    ;; otherwise generate the instructions for the vector, a length
                                    ;; and a call to %vector
                                    (if (null? rst)
                                        (list (list 3 (make-vector 0)))
                                        (append
                                            (reverse-append
                                                (hydra@map rst env))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (hydra@lookup '%vector env))))))
                                (eq? (cdr v) 'primitive-syntax-dict)
                                    ;; if rst is null?, then generate a load with an empty dict
                                    ;; otherwise generate the instructions for the dict, a length
                                    ;; and a call to %dict
                                    (if (null? rst)
                                        (list (list 3 (make-dict)))
                                        (append
                                            (reverse-append
                                                (hydra@map rst env))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (hydra@lookup '%dict env))))))
                                (eq? (cdr v) 'primitive-syntax-string)
                                (eq? (cdr v) 'primitive-syntax-string)
                                    (if (null? rst)
                                        (list (list 3 (make-string 0)))
                                        (append
                                            (reverse-append
                                                (hydra@map rst env))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (hydra@lookup '%string env))))))
                                (eq? (cdr v) 'primitive-syntax-append)
                                    (if (null? rst)
                                        (list (list 4))
                                        (append
                                            (reverse-append
                                                (hydra@map rst env))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (hydra@lookup '%append env))))))
                                    
                                (eq? (cdr v) 'primitive-syntax-makevector)
                                    (with l (length rst)
                                        (cond
                                            (= l 1)
                                                (append
                                                    '((4))
                                                    (hydra@compile (car rst) env)
                                                    (list (list (cdr (hydra@lookup '%make-vector env)))))
                                            (= l 2)
                                                (append
                                                    (reverse-append (hydra@map rst env))
                                                    (list (list (cdr (hydra@lookup '%make-vector env)))))
                                            else (hydra@error "make-vector len : INTEGER (v : SEXPR) => VECTOR")))
                                (eq? (cdr v) 'primitive-syntax-makestring)
                                    (with l (length rst)
                                        (cond
                                            (= l 1)
                                                (append
                                                    '((3 #\space))
                                                    (hydra@compile (car rst) env)
                                                    (list (list (cdr (hydra@lookup '%make-string env)))))
                                            (= l 2)
                                                (append
                                                    (reverse-append (hydra@map rst env))
                                                    (list (list (cdr (hydra@lookup '%make-string env)))))
                                            else (hydra@error "make-string len : INTEGER (c : CHAR) => STRING")))
                                (eq? (cdr v) 'primitive-syntax-if)
                                    ;; need to generate code for <cond>
                                    ;; add CMP instruction '(30)
                                    ;; generate code for <then>
                                    ;; generate code for <else>
                                    ;; add count to CMP instruction to jump to <else>
                                    ;; add count to <then> to skip <else>
                                    (let* ((<cond> (hydra@compile (car rst) env))
                                           (<then> (hydra@compile (cadr rst) env))
                                           (<else> (hydra@compile (caddr rst) env))
                                           (then-len (+ (length <then>) 2)) ;; +2 in order to avoid the jump over else
                                           (else-len (+ (length <else>) 1)))
                                        (append <cond>
                                            (list (list 29 then-len)) ;; compare & jump
                                            <then>
                                            (list (list 28 else-len)) ;; jump else
                                            <else>)) 
                                else 
                                    (hydra@error "syntax has not been implemented at this time"))
                            (pair? fst) 
                                ;; fst is a pair, so we just blindly attempt to compile it.
                                ;; May cause an error that has to be caught in CALL. some lifting might fix this...
                                (append (reverse-append (hydra@map rst env))
                                        (hydra@compile fst env)
                                        (list (list 110)))
                            (hydra@usyntax? v)
                                (hydra@compile
                                    (syntax-expand1 (cadr v) line)
                                    env)
                            (hydra@umacro? v)
                                #f
                            (hydra@procedure? v) ;; need to add some method of checking proc arity here.
                                (let* ((rlen (length rst)))
                                    (append
                                        (reverse-append (hydra@map rst env))
                                        (list (list 16 (cdr v) rlen))))
                            (hydra@primitive? v) ;; primitive procedure
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
                                (append
                                    (reverse-append
                                        (hydra@map rst env))
                                    (list (list (cdr v))))
                            (hydra@lambda? v) ;; hydra closure
                                (append (reverse-append (hydra@map rst env))
                                            (list (list 30 v)))
                            (hydra@continuation? v) ;; hydra continuation
                                (append (reverse-append (hydra@map rst env))
                                    (list (list 3 v))
                                    (list (list 108))) ;; 108 -> %ap
                            (symbol? fst) ;; fst is a symbol, but it has no mapping in our current env; write to environment-load
                                (append (reverse-append
                                            (hydra@map rst env)) 
                                            (list (list 30 fst)))
                            else (error "error: the only applicable types are primitive procedures, closures & syntax")))

            else (list (list 3 line)))))

;; need to separate user values from what 
;; is returned in the eval...
(define (top-level-print x)
    " print #<foo> at the top level"
    (cond
        (hydra@lambda? x) (display "#<closure>")
        (hydra@continuation? x) (display "#<continuation>")
        (hydra@primitive? x) (display (format "#<primitive-procedure ~a>" (cdr x)))
        (hydra@procedure? x) (display (format "#<procedure ~a>" (cdr x)))
        (hydra@syntax? x) (display (format "#<syntax ~a>" (cdr x)))
        (hydra@error? x) (display (format "ERROR: ~a" (cdr x)))
        (hydra@usyntax? x) (display "#<syntax rules>")
        else (write x)))

(define (hydra@load-loop fh env dump)
    (let ((o (read fh)))
        (if (eq? o #e)
            #v
            (begin
                (hydra@eval o env dump) 
                (hydra@load-loop fh env dump)))))

(define (hydra@load src-file env dump)
    "an implementation of the primitive procedure load"
    (let ((f (open (coerce src-file :string) :read)))
        (hydra@load-loop f env dump)
        (close f)
        #v))
                                    
(define (hydra@repl env dump)
    (display "h; ")
    (with inp (read)
     (cond 
        (and (eq? (type inp) "Pair") (eq? (car inp) 'unquote))
            (cond
                (eq? (cadr inp) 'exit) #v
                (eq? (cadr inp) 'q) #v
                (eq? (cadr inp) 'quit) #v
                (eq? (cadr inp) 'bye) #v
                (eq? (cadr inp) 'dribble) (begin (hydra@repl env dump))
                (eq? (cadr inp) 'save) (begin (hydra@repl env dump))
                (eq? (cadr inp) 'save-and-die) (begin (hydra@repl env dump))
                (pair? (cadr inp))
                    (let ((cmd (caadr inp))
                          (arg (cadadr inp)))
                        (cond
                            (eq? cmd 'i)
                                (write (hydra@lookup arg env)))
                        (newline)
                        (hydra@repl env dump))
                else (begin (display (format "Unknown command: ~a~%" (cadr inp))) (hydra@repl env dump)))
        (eof-object? inp)
            #v
        (not (pair? inp))
            (if (eq? inp #v)
                (hydra@repl env dump)
                (begin
                    (top-level-print (hydra@lookup inp env))
                    (display "\n")
                    (hydra@repl env dump)))
        else
            (with r (hydra@eval inp env dump) 
                (if (eq? r #v)
                 (hydra@repl env dump)
                 (begin
                    (if (hydra@error? r)
                        #v
                        (hydra@add-env! '_ r env))
                    (top-level-print r)
                    (display "\n")
                    (hydra@repl env dump)))))))

(define (hydra@main args)
    (let ((e {})
          (dump (make-vector 1000 #v)))
        (hydra@init-env e)
        (if (> (length args) 0)
            (begin
                (hydra@add-env! '*command-line* (cslice args 1 (length args)) (list e))
                (hydra@load (nth args 0) (list e) (list 0 dump)))
            (begin
                (display "\n\t()\n\t  ()\n\t()  ()\nDigamma/Typhon: 2012.0/r0\n")
                (hydra@add-env! '*command-line* '() (list e))
                (hydra@repl (list e) (list 0 dump))))))

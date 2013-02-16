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

;; mini-prelude
;; should be removed once Enyalios supports load better...

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (null? n) (eq? n '()))
(define (pair? n) (eq? (type n) "Pair"))
(define (vector? n) (eq? (type n) "Vector"))
(define (dict? n) (eq? (type n) "Dictionary"))
(define (symbol? n) (eq? (type n) "Symbol"))
(define (key? n) (eq? (type n) "Key"))
(define (number? n) (eq? (type n) "Number"))
(define (string? n) (eq? (type n) "String"))
(define (bool? n) (eq? (type n) "Boolean"))
(define (goal? n) (eq? (type n) "Goal"))
(define (not x)
        (cond
            (eq? x #s) #u
            (eq? x #f) #t
            (eq? x #u) #s
            else #f))
(define (zero? n) (= n 0))
(define (eof-object? n) (eq? n #e))
(define (void? x) (eq? x #v))

(define (zip xs ys)
	(if (null? xs)
		'()
		(cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys)))))

(define (list-copy l)
    " really, should be included from SRFI-1, but this simply makes a copy
      of the spine of a pair
      "
    (if (null? l)
        l
        (cons (car l) (list-copy (cdr l)))))

(define (cadddar x) (car (cdddar x)))

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

;; end mini-prelude.

(define (hydra@instruction c)
    (car c))

(define (hydra@operand c)
    (cadr c))

(define (dict-copy k dict new-dict)
    "shallow copy a dictionary"
    (if (null? k)
        new-dict
        (begin
            (cset! new-dict (car k) (nth dict (car k)))
            (dict-copy (cdr k) dict new-dict))))

(define (build-environment environment stack params)
    "Adds a new window to the environment, removes |params| items from the stack
     and binds those values in the new window. It returns a list of environment and
     the new stack."
    ;; rough match; doesn't take into account optional parameters.
    ;; would probably be better to have an inner function that iterates over
    ;; the parameters & returns those that match. It would then be easier to 
    ;; have optional parameters...
    ;; Oh! Don't just use environment, use (copy-dict environment)
    ;; actually, need to copy entire frame? ugh. That's going to be ugly
    (let ((ls (length stack)) (lp (length params)) (nu-env (dict)))
        (if (< ls lp)
            (error "non-optional parameters are not statisfied by stack items in build-environment")
            (if (= lp 0)
                (list (cons nu-env environment) (cdr stack))
                (begin 
                    (foreach
                        (lambda (x)
                            (cset! nu-env (car x) (cadr x)))
                        (zip params (cslice stack 0 lp)))
                    (list (cons nu-env environment) (cslice stack lp ls)))))))

(define (copy-code code ip offset)
    " copies the spine of code, but at ip & ip+1, insert %nop instructions
      instead, over-writing the call/cc & load-lambda instructions therein.
      "
    (cond
        (null? code) '()
        (= offset (- ip 1)) (append (list '(107) '(107)) (copy-code (cddr code) ip (+ offset 2)))
        else (append (list (car code)) (copy-code (cdr code) ip (+ offset 1)))))

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
        (if (null? dump)
            (car stack)
            (hydra@vm (caar dump) (cadar dump) (+ (caddar dump) 1) (cons (car stack) (cadddar dump)) (cdr dump)))
        else
        (let* ((c (nth code ip))
                (instr (hydra@instruction c)))

                ;(display "current instruction: ")
                ;(display (nth code ip))
                ;(newline)   
              (cond ;; case would make a lot of sense here...
                  (eq? instr 0) ;; car
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (car (car stack)) (cdr stack)) dump)
                  (eq? instr 1) ;; cdr
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (cdr (car stack)) (cdr stack)) dump)
                  (eq? instr 2) ;; cons
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (cons (car stack)
                                                (cadr stack))
                                       (cddr stack)) dump)
                  (eq? instr 3) ;; load
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (hydra@operand c) stack) dump)
                  (eq? instr 4) ;; nil
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons '() stack) dump)
                  (eq? instr 5) ;; -
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (- (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 6) ;; +
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (+ (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 7) ;; * 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (* (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 8) ;; / 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (/ (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 9) ;;  < 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (< (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 10) ;; >
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (> (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 11) ;; <= 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (<= (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 12) ;; >= 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (>= (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 13) ;; length
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (length (car stack)) (cdr stack)) dump)
                  (eq? instr 14) ;; exact?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (exact? (car stack)) (cdr stack)) dump)
                  (eq? instr 15) ;; inexact?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (inexact? (car stack)) (cdr stack)) dump)
                  (eq? instr 16) ;; display
                    (begin
                        (display (car stack))
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons #v (cdr stack)) dump))
                  (eq? instr 18) ;; real?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (real? (car stack)) (cdr stack)) dump)
                  (eq? instr 19) ;; integer?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (integer? (car stack)) (cdr stack)) dump)
                  (eq? instr 20) ;; complex?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (complex? (car stack)) (cdr stack)) dump)
                  (eq? instr 21) ;; rational?
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (rational? (car stack)) (cdr stack)) dump)
                  (eq? instr 22) ;; gcd
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (gcd (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 23) ;; lcm
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (lcm (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 24) ;; numerator 
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (numerator (car stack)) (cdr stack)) dump)
                  (eq? instr 25) ;; denomenator
                        (hydra@vm code
                                  env
                                  (+ ip 1)
                                  (cons (denomenator (car stack)) (cdr stack)) dump)
                  (eq? instr 26) ;; = 
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (= (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 27) ;; eq?
                        (hydra@vm code
                                 env
                                 (+ ip 1)
                                 (cons (eq? (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 28) ;; jump
                        (hydra@vm code
                                 env
                                 (+ ip (hydra@operand c))
                                 stack dump)
                  (eq? instr 29) ;; cmp
                        (if (car stack) ;; if the top of the stack is true
                            (hydra@vm code env (+ ip 1) (cdr stack) dump) ;; jump to the <then> portion
                            (hydra@vm code env (+ ip (hydra@operand c)) (cdr stack) dump))
                  (eq? instr 30) ;; call
                        (cond
                            (hydra@lambda? (car stack))
                            ;; create a list from the current registers, cons this to dump, and 
                            ;; recurse over hydra@vm. 
                            ;; need to support CALLing primitives too, since they could be passed
                            ;; in to HOFs...
                            (let ((env-and-stack (build-environment (nth (cadar stack) 0) (cdr stack) (nth (cadar stack) 2))))
                                (hydra@vm
                                    (nth (cadar stack) 1)
                                    (car env-and-stack)
                                    0 '() 
                                    (cons (list code env ip (cadr env-and-stack)) dump)))
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
                                #f))
                  (eq? instr 31) ;; environment-load; there is never a raw #f, so this is safe
                        (with r (hydra@lookup (hydra@operand c) env)
                            (if (eq? r #f)
                                #f
                                (hydra@vm
                                    code 
                                    env
                                    (+ ip 1) 
                                    (cons r stack)
                                    dump)))
                  (eq? instr 32) ;; tail-call 
                        (if (and (not (null? stack)) (eq? (caar stack) 'compiled-lambda))
                            (hydra@vm
                                (nth (cdar stack) 0)
                                (nth (cdar stack) 1)
                                0 '() 
                                dump)
                            #f)
                  (eq? instr 33) ;; %define
                        (begin
                            (hydra@add-env! (car stack) (cadr stack) env)
                            (hydra@vm
                                code env (+ ip 1)
                                (cons #v stack)
                                dump))
                  (eq? instr 34) ;; %set!
                        (begin
                            (hydra@set-env! (car stack) (cadr stack) env)
                            (hydra@vm
                                code env (+ ip 1)
                                (cons #v stack)
                                dump))
                  (eq? instr 35) ;; ceil
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (ceil (car stack)) (cdr stack)) dump)
                  (eq? instr 36) ;; floor
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (floor (car stack)) (cdr stack)) dump)
                  (eq? instr 37) ;; truncate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (truncate (car stack)) (cdr stack)) dump)
                  (eq? instr 38) ;; round
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (round (car stack)) (cdr stack)) dump)
                  (eq? instr 39) ;; inexact->exact
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (inexact->exact (car stack)) (cdr stack)) dump)
                  (eq? instr 40) ;; quotient
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (quotient (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 41) ;; modulo
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (modulo (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 42) ;; &
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (& (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 43) ;; |
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (| (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 44) ;; ^
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (^ (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 45) ;; ~
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (~ (car stack)) (cdr stack)) dump)
                  (eq? instr 46) ;; %list
                        ;; take N items off the stack, create a list, and return it
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (cslice (cdr stack) 0 (car stack))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 47) ;; %vector
                        ;; take N items off the stack, create a list, and return it
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (coerce (cslice (cdr stack) 0 (car stack)) 'vector)
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 48) ;; %make-vector
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-vector (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 49) ;; %make-string
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-string (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 50) ;; %string
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply string (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 51) ;; %append
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 52) ;; first
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (first (car stack)) (cdr stack)) dump)
                  (eq? instr 53) ;; rest
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (rest (car stack)) (cdr stack)) dump)
                  (eq? instr 54) ;; ccons
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (ccons (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 55) ;; %nth
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (nth (caddr stack) (cadr stack) (car stack)) (cdddr stack)) dump)
                  (eq? instr 56) ;; keys
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (keys (car stack)) (cdr stack)) dump)
                  (eq? instr 57) ;; partial-key?
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (partial-key? (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 58) ;; cset!
                        (begin
                            (cset! (caddr stack) (cadr stack) (car stack))
                            (hydra@vm code
                                env
                                (+ ip 1)
                                (cons #v (cdddr stack)) dump))
                  (eq? instr 59) ;; empty?
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (empty? (car stack)) (cdr stack)) dump)
                  (eq? instr 60) ;; gensym
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (gensym (car stack)) (cdr stack)) dump)
                  (eq? instr 61) ;; imag-part
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (imag-part (car stack)) (cdr stack)) dump)
                  (eq? instr 62) ;; real-part
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (real-part (car stack)) (cdr stack)) dump)
                    (eq? instr 63) ;; make-rectangular
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-rectangular (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 64) ;; make-polar
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (make-polar (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 65) ;; magnitude
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (magnitude (car stack)) (cdr stack)) dump)
                    (eq? instr 66) ;; argument
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (argument (car stack)) (cdr stack)) dump)
                    (eq? instr 67) ;; conjugate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (conjugate (car stack)) (cdr stack)) dump)
                    (eq? instr 68) ;; conjugate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (conjugate! (car stack)) (cdr stack)) dump)
                    (eq? instr 69) ;; polar->rectangular
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (polar->rectangular (car stack)) (cdr stack)) dump)
                    (eq? instr 70) ;; rectangular->polar
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (rectangular->polar (car stack)) (cdr stack)) dump)
                    (eq? instr 71) ;; sin
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (sin (car stack)) (cdr stack)) dump)
                    (eq? instr 72) ;; cos
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cos (car stack)) (cdr stack)) dump)
                    (eq? instr 73) ;; tan
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (tan (car stack)) (cdr stack)) dump)
                    (eq? instr 74) ;; asin
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (asin (car stack)) (cdr stack)) dump)
                    (eq? instr 75) ;; acos
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (acos (car stack)) (cdr stack)) dump)
                    (eq? instr 76) ;; atan
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (atan (car stack)) (cdr stack)) dump)
                    (eq? instr 77) ;; atan2
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (atan2 (cadr stack) (car stack)) (cddr stack)) dump)
                    (eq? instr 78) ;; sinh
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (sinh (car stack)) (cdr stack)) dump)
                    (eq? instr 79) ;; cosh
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cosh (car stack)) (cdr stack)) dump)
                    (eq? instr 80) ;; tanh
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (tanh (car stack)) (cdr stack)) dump)
                    (eq? instr 81) ;; exp
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp (car stack)) (cdr stack)) dump)
                    (eq? instr 82) ;; ln
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (ln (car stack)) (cdr stack)) dump)
                    (eq? instr 83) ;; abs
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (abs (car stack)) (cdr stack)) dump)
                    (eq? instr 84) ;; sqrt
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (sqrt (car stack)) (cdr stack)) dump)
                    (eq? instr 85) ;; exp2
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 86) ;; expm1
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (expm1 (car stack)) (cdr stack)) dump)
                    (eq? instr 87) ;; log2
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (log2 (car stack)) (cdr stack)) dump)
                    (eq? instr 88) ;; log10
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (log10 (car stack)) (cdr stack)) dump)
                    (eq? instr 89) ;; <<
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (<< (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 90) ;; >>
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (>> (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 91) ;; %string-append
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 92) ;; assq
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (assq (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 93) ;; memq
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (memq (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 94) ;; %dict
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 95) ;; make-dict
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (dict) stack) dump)
                    (eq? instr 96) ;; dict-has?
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (dict-has? (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 97) ;; coerce
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (coerce (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 98) ;; cupdate
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cupdate (car stack) (cadr stack) (caddr stack)) (cdddr stack)) dump)
                    (eq? instr 99) ;; cslice
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (cslice (car stack) (cadr stack) (caddr stack)) (cdddr stack)) dump)
                    (eq? instr 100) ;; tconc!
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 101) ;; make-tconc
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 102) ;; tconc-list
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 103) ;; tconc->pair
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 104) ;; tconc-splice
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 105) ;; rationalize
                        (hydra@vm code
                            env
                            (+ ip 1)
                            (cons (rationalize (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 106) ;; call/cc
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
                    (eq? instr 107) ;; %nop
                        (hydra@vm code
                            env
                            (+ ip 1)
                            stack
                            dump)
                    (eq? instr 108) ;; %ap
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
                    (eq? instr 109) ;; %makeclosure
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
                                            (list-copy env)
                                            (nth (cadar stack) 1)
                                            (nth (cadar stack) 2)))
                                    (cdr stack))
                                dump))))))

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

(define *tlenv* '({
    :car (primitive . 0) ;; (primitive . 0) 
    :cdr (primitive . 1)
    :cons (primitive . 2)
    :%load (primitive . 3) ;; 3 is load a value onto the stack
    :%nil (primitive . 4) ;; 4 is push a nil onto the stack
    :%- (primitive . 5) ;; primitive math operations with arity 2
    :%+ (primitive . 6)
    :%* (primitive . 7)
    :%/ (primitive . 8)
    :%< (primitive . 9)
    :%> (primitive . 10)
    :%<= (primitive . 11)
    :%>= (primitive . 12)
    :if (syntax . primitive-syntax-if)
    :fn (syntax . primitive-syntax-fn)
    :lambda (syntax . primitive-syntax-fn)
    :quote (syntax . primitive-syntax-quote)
    :quasi-quote (syntax . primitive-syntax-qquote)
    :unquote (syntax . primitve-syntax-unquote)
    :unquote-splice (syntax . primitive-syntax-unqsplice)
    :length (primitive . 13)
    :exact? (primitive . 14)
    :inexact? (primitive . 15)
    :display (primitive . 16)
    :apply (primitive . 17)
    :real? (primitive . 18)
    :integer? (primitive . 19)
    :complex? (primitive . 20)
    :rational? (primitive . 21)
    :gcd (primitive . 22)
    :lcm (primitive . 23)
    :numerator (primitive . 24)
    :denomenator (primitive . 25)
    :%= (primitive . 26) ;; probably has to place the value on stack rather than #t, #f for failure
    :eq? (primitive . 27)
    :%jmp (primitive . 28) ;; 28 is jump
    :%cmp (primitive . 29) ;; 29 is compare
    :%call (primitive . 30) ;; 30 is call
    :%env-load (primitive . 31) ;; 31 is environment-load
    :%tail-call (primitive . 32) ;; 32 is tail-call; is this necessary? operand to CALL?
    :+ (syntax . primitive-syntax-plus) ;; variable arity syntax
    :- (syntax . primitive-syntax-minus)
    :* (syntax . primitive-syntax-mult)
    :/ (syntax . primitive-syntax-div)
    :< (syntax . primitive-syntax-lt)
    :> (syntax . primitive-syntax-gt)
    :<= (syntax . primitive-syntax-lte)
    :>= (syntax . primitive-syntax-gte)
    := (syntax . primitive-syntax-numeq)
    :define (syntax . primitive-syntax-define)
    :set! (syntax . primitive-syntax-set)
    :define-syntax (syntax . primitive-syntax-defsyn)
    :define-macro (syntax . primitive-syntax-defmac)
    :%define (primitive . 33)
    :%set! (primitive . 34)
    :ceil (primitive . 35)
    :floor (primitive . 36)
    :truncate (primitive . 37)
    :round (primitive . 38)
    :inexact->exact (primitive . 39)
    :quotient (primitive . 40)
    :modulo (primitive . 41)
    :& (primitive . 42)
    :| (primitive . 43)
    :^ (primitive . 44)
    :~ (primitive . 45)
    :%list (primitive . 46)
    :list (syntax . primitive-syntax-list)
    :%vector (primitive . 47)
    :vector (syntax . primitive-syntax-vector)
    :%make-vector (primitive . 48)
    :make-vector (syntax  . primitive-syntax-makevector)
    :%make-string (primitive . 49)
    :make-string (syntax . primitive-syntax-makestring)
    :%string (primitive . 50)
    :string (syntax . primitive-syntax-string)
    :%append (primitive . 51)
    :append (syntax . primitive-syntax-append)
    :first (primitive . 52)
    :rest (primitive . 53)
    :ccons (primitive . 54)
    :nth (primitive . 55)
    :keys (primitive . 56)
    :partial-key? (primitive . 57)
    :cset! (primitive . 58)
    :empty? (primitive . 59)
    :define-macro (syntax . primitive-syntax-defmac)
    :gensym (primitive . 60)
    :imag-part (primitive . 61)
    :real-part (primitive . 62)
    :make-rectangular (primitive . 63)
    :make-polar (primitive . 64)
    :magnitude (primitive . 65)
    :argument (primitive . 66)
    :conjugate (primitive . 67)
    :conjugate! (primitive . 68)
    :polar->rectangular (primitive . 69)
    :rectangular->polar (primitive . 70)
    :sin (primitive . 71)
    :cos (primitive . 72)
    :tan (primitive . 73)
    :asin (primitive . 74)
    :acos (primitive . 75)
    :atan (primitive . 76)
    :atan2 (primitive . 77)
    :sinh (primitive . 78)
    :cosh (primitive . 79)
    :tanh (primitive . 80)
    :exp (primitive . 81)
    :ln (primitive . 82)
    :abs (primitive . 83)
    :sqrt (primitive . 84)
    :exp2 (primitive . 85)
    :expm1 (primitive . 86)
    :log2 (primitive . 87)
    :log10 (primitive . 88)
    :<< (primitive . 89)
    :>> (primitive . 90)
    :string-append (primitive . 91)
    :assq (primitive . 92)
    :memq (primitive . 93)
    :dict (primitive . 94)
    :make-dict (primitive . 95)
    :dict-has? (primitive . 96)
    :coerce (primitive . 97)
    :cupdate (primitive . 98)
    :cslice (primitive . 99)
    :tconc! (primitive . 100)
    :make-tconc (primitive . 101)
    :tconc-list (primitive . 102)
    :tconc->pair (primitive . 103)
    :tconc-splice (primitive . 104)
    :rationalize (primitive . 105)
    :call/cc (primitive . 106)
    :%nop (primitive . 107) ;;no operation 
    :%ap  (primitive . 108) ;; apply a continuation
    :%makeclosure (primitive . 109)
}))

(define (hydra@lookup item env)
    " look up item in the current environment, returning #f for not found"
    (cond
        (not (symbol? item)) item ;; to support ((fn (x) (+ x x)) (+ x x) 3)
        (null? env) (hydra@error (format "unbound variable: ~a" item)) 
        (dict-has? (car env) item) (nth (car env) item)
        else (hydra@lookup item (cdr env))))

(define (compile-lambda rst env)
    (list 'compiled-lambda
        (vector
            (list-copy env)
            (append-map
                (fn (x) (hydra@compile x env))
                (cdr rst))
            (car rst)))) 

(define (hydra@lambda? x)
    (and (pair? x) (eq? (car x) 'compiled-lambda)))

(define (hydra@primitive? x)
    (and (pair? x) (eq? (car x) 'primitive)))

(define (hydra@syntax? x)
    (and (pair? x) (eq? (car x) 'syntax)))

(define (hydra@error? x)
    (and (pair? x) (eq? (car x) 'error)))

(define (hydra@continuation? x)
    (and (pair? x) (eq? (car x) 'continuation)))

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

(define (show x) (display "show: ") (display x) (display "\n") x)

(define (hydra@error msg)
    "simple, hydra specific errors"
    (list 'error msg))

(define (hydra@eval line env)
    "simple wrapper around hydra@vm & hydra@compile"
    (hydra@vm (hydra@compile line env) env 0 '() '()))

(define (hydra@compile-help sym iter-list env)
    " a helper function for hydra@compile, which collects
      the old use of append-map into a single function that
      Eprime can compile (still haven't added HOFs to E'...
      embarrassing, I know)
    "
    (if (null? iter-list)
        iter-list
        (append
            (append (hydra@compile (car iter-list) env)
                (list (list (cdr (hydra@lookup sym env)))))
            (hydra@compile-help sym (cdr iter-list) env))))

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
                                    (append 
                                        '((3 0))
                                        (hydra@compile-help '%+ rst env))
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
                                    (append 
                                        '((3 1))
                                        (hydra@compile-help '%* rst env))
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
                                    #t
                                (eq? (cdr v) 'primitive-syntax-defmac)
                                    #t
                                (eq? (cdr v) 'primitive-syntax-fn)
                                    (list
                                        (list 3 ;; load
                                            (compile-lambda rst env))
                                        (list (cdr (hydra@lookup '%makeclosure env))))
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
                                                (map
                                                    (fn (x) (hydra@compile x env))
                                                    rst))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (hydra@lookup '%list env))))))
                                (eq? (cdr v) 'primitive-syntax-vector)
                                    ;; if rst is null?, then generate a load-null instruction (4)
                                    ;; otherwise generate the instructions for the list, a length
                                    ;; and a call to %vector
                                    (if (null? rst)
                                        (list (list 4))
                                        (append
                                            (reverse-append
                                                (map
                                                    (fn (x) (hydra@compile x env))
                                                    rst))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (hydra@lookup '%vector env))))))
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
                                                    (reverse-append
                                                        (map (fn (x) (hydra@compile x env)) rst))
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
                                                    (reverse-append
                                                        (map (fn (x) (hydra@compile x env)) rst))
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
                                else #t)
                            (pair? fst) 
                                ;; fst is a pair, so we just blindly attempt to compile it.
                                ;; May cause an error that has to be caught in CALL. some lifting might fix this...
                                (append (reverse-append
                                            (map (fn (x) (hydra@compile x env)) rst))
                                        (hydra@compile fst env)
                                        (list (list 30)))
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
                                        (map (fn (x) (hydra@compile x env))
                                             rst))
                                    (list (list (cdr v))))
                            (hydra@lambda? v) ;; hydra closure
                                (append (reverse-append
                                            (map (fn (x) (hydra@compile x env)) rst))
                                            (list (list 3 v))
                                            (list (list 30)))
                            (hydra@continuation? v) ;; hydra continuation
                                (append (reverse-append
                                            (map (fn (x) (hydra@compile x env)) rst))
                                    (list (list 3 v))
                                    (list (list 108))) ;; 108 -> %ap
                            (symbol? fst) ;; fst is a symbol, but it has no mapping in our current env; write to environment-load
                                (append (reverse-append
                                            (map (fn (x) (hydra@compile x env)) rst))
                                            (list (list 31 fst))
                                            (list (list 30)))
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
        (hydra@syntax? x) (display (format "#<syntax ~a>" (cdr x)))
        (hydra@error? x) (display (format "ERROR: ~a" (cdr x)))
        else (display x)))

(define (hydra@load src-file env)
    "an implementation of the primitive procedure load"
    ;(with f (open src-file :read)
    ;    (with-exception-handler
    ;        (fn (x) (display (format "An error occured while loading ~S: ~a\n" src-file x)) (close f))
    ;        (fn ()
    ;            (letrec ((loop (fn (expr)
    ;                                (if (eq? expr #e)
    ;                                    #v
    ;                                    (begin
    ;                                        (hydra@eval expr env)
    ;                                        (loop (read f)))))))
    ;                (loop (read f)))
    ;            (close f)))))
    #f)
                                    
(define (hydra@repl)
    (display "h; ")
    (with inp (read)
     (if (and (eq? (type inp) "Pair") (eq? (car inp) 'unquote))
        (cond
         (eq? (cadr inp) 'exit) #v
         (eq? (cadr inp) 'q) #v
         (eq? (cadr inp) 'quit) #v
         (eq? (cadr inp) 'bye) #v
         (eq? (cadr inp) 'dribble) (begin (hydra@repl))
         (eq? (cadr inp) 'save) (begin (hydra@repl))
         (eq? (cadr inp) 'save-and-die) (begin (hydra@repl))
         else (begin (display (format "Unknown command: ~a~%" (cadr inp))) (hydra@repl)))
        (if (not (pair? inp))
            (if (eq? inp #v)
                (hydra@repl)
                (begin
                    (top-level-print (hydra@lookup inp *tlenv*))
                    (display "\n")
                    (hydra@repl)))
            (with r (hydra@eval inp *tlenv*)
                (if (eq? r #v)
                 (hydra@repl)
                 (begin
                    (top-level-print r)
                    (display "\n")
                    (hydra@repl))))))))

(define (hydra@main)
    (display "\n\t()\n\t  ()\n\t()  ()\nDigamma/Hydra: 2012.0/r0\n")
    (hydra@repl))    

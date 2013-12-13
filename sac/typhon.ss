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
;; the first two *can* be handled OOB by typhon@eval, but the third
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

(define-syntax typhon@instruction () 
    ((typhon@instruction c) (car c)))

(define-syntax typhon@operand ()
    ((typhon@operand c) (cadr c)))

(define-syntax typhon@lambda? ()  ((typhon@lambda? x)
    (and (pair? x) (eq? (car x) 'compiled-lambda))))

(define-syntax typhon@primitive? () ((typhon@primitive? x)
    (and (pair? x) (eq? (car x) 'primitive))))

(define-syntax typhon@syntax? () ((typhon@syntax? x)
    (and (pair? x) (eq? (car x) 'syntax))))

(define-syntax typhon@error? () ((typhon@error? x)
    (and (pair? x) (eq? (car x) 'error))))

(define-syntax typhon@continuation? () ((typhon@continuation? x)
    (and (pair? x) (eq? (car x) 'continuation))))

(define-syntax typhon@procedure? () ((typhon@procedure? x)
    (and (pair? x) (eq? (car x) 'procedure))))

(define-syntax typhon@usyntax? () ((typhon@usyntax? x)
    (and (pair? x) (eq? (car x) 'user-syntax)))) 

(define (typhon@umacro? x)
    #f)

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
        else
            (error (format "unknown procedure \"~a\"" proc))))

(define (typhon@vm code code-len env ip stack locals dump)
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
     (cond
        (or (type? (car stack) "ERROR")
            (typhon@error? (car stack)))
            (car stack)
        (>= ip code-len)
            (if (= (car dump) 0)
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
                        (list (- offset 6) top-dump))))
        (typhon@error? (nth code ip))
            (nth code ip)
         else
         (let* ((c (nth code ip))
                (instr (typhon@instruction c)))
              ;(display (format "current ip: ~n~%current instruction: " ip))
              ;(write (nth code ip))
              ;(display "\ncurrent stack: ")
              ;(write stack)
              ;(display "\ncurrent dump: ")
              ;(write dump)
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
                                (cons (car (car stack)) (cdr stack)) locals dump)
                    (1) ;; cdr
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (cdr (car stack)) (cdr stack)) locals dump)
                    (2) ;; cons
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (cons (car stack)
                                                (cadr stack))
                                       (cddr stack))
                                 locals
                                 dump)
                    (3) ;; load
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (typhon@operand c) stack)
                                 locals
                                 dump)
                    (4) ;; nil
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons '() stack)
                                 locals
                                 dump)
                    (5) ;; -
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (- (cadr stack) (car stack)) (cddr stack))
                                 locals
                                 dump)
                    (6) ;; +
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (+ (car stack) (cadr stack)) (cddr stack))
                                 locals
                                 dump)
                    (7) ;; * 
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (* (car stack) (cadr stack)) (cddr stack))
                                 locals
                                 dump)
                    (8) ;; / 
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (/ (cadr stack) (car stack)) (cddr stack))
                                 locals
                                 dump)
                    (9) ;;  < 
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (< (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (10) ;; >
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (> (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (11) ;; <= 
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (<= (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (12) ;; >= 
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (>= (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (13) ;; length
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (length (car stack)) (cdr stack)) locals dump)
                    (14) ;; exact?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (exact? (car stack)) (cdr stack)) locals dump)
                    (15) ;; inexact?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (inexact? (car stack)) (cdr stack)) locals dump)
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
                                dump)
                           (typhon@vm
                                code code-len
                                env
                                (+ ip 1)
                                (cons ret (cslice stack (- arity 1) (length (cdr stack))))
                                locals
                                dump)))
                    (18) ;; real?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (real? (car stack)) (cdr stack)) locals dump)
                    (19) ;; integer?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (integer? (car stack)) (cdr stack)) locals dump)
                    (20) ;; complex?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (complex? (car stack)) (cdr stack)) locals dump)
                    (21) ;; rational?
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (rational? (car stack)) (cdr stack)) locals dump)
                    (22) ;; gcd
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (gcd (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (23) ;; lcm
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (lcm (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (24) ;; numerator 
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (numerator (car stack)) (cdr stack)) locals dump)
                    (25) ;; denomenator
                        (typhon@vm code code-len
                                  env
                                  (+ ip 1)
                                  (cons (denomenator (car stack)) (cdr stack)) locals dump)
                    (26) ;; = 
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (= (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (27) ;; eq?
                        (typhon@vm code code-len
                                 env
                                 (+ ip 1)
                                 (cons (eq? (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (28) ;; jump
                        (typhon@vm code code-len
                                 env
                                 (+ ip (typhon@operand c))
                                 stack locals dump)
                    (29) ;; cmp
                        (if (car stack) ;; if the top of the stack is true
                            (typhon@vm code code-len env (+ ip 1) (cdr stack) locals dump) ;; jump to the <then> portion
                            (typhon@vm code code-len env (+ ip (typhon@operand c)) (cdr stack) locals dump))
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
                                    (typhon@error "incorrect arity for user procedure")
                                (typhon@error? call-proc)
                                    call-proc
                                (typhon@lambda? call-proc)
                                    ;; create a list from the current registers, cons this to dump, and 
                                    ;; recurse over typhon@vm. 
                                    ;; need to support CALLing primitives too, since they could be passed
                                    ;; in to HOFs...
                                    (if (> (car dump) (length (cadr dump)))
                                        (error "Dump stack overflow")
                                        (let ((env-and-stack (build-environment (vector-ref (cadr call-proc) 1) stack (vector-ref (cadr call-proc) 2) locals))
                                              (v-dump (cadr dump))
                                              (offset (car dump)))
                                            (vector-set! v-dump offset locals)
                                            (vector-set! v-dump (+ offset 1) (cadr env-and-stack))
                                            (vector-set! v-dump (+ offset 2) ip)
                                            (vector-set! v-dump (+ offset 3) env)
                                            (vector-set! v-dump (+ offset 4) code-len)
                                            (vector-set! v-dump (+ offset 5) code)
                                            (typhon@vm
                                                (vector-ref (cadr call-proc) 1)
                                                (length (vector-ref (cadr call-proc) 1))
                                                (car env-and-stack)
                                                0 '() 
                                                (caddr env-and-stack)
                                                (list (+ offset 6) v-dump))))
                                (typhon@primitive? (car stack)) ;; if primitives stored arity, slicing would be easy...
                                    (begin
                                        (display "in typhon@primitive\n\t")
                                        (display (car stack))
                                        (display "\n")
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
                            (if (typhon@error? r)
                                r
                                (typhon@vm
                                    code
                                    code-len
                                    env
                                    (+ ip 1) 
                                    (cons r stack)
                                    locals
                                    dump)))
                    (32) ;; tail-call 
                        (if (and (not (null? stack)) (eq? (caar stack) 'compiled-lambda))
                            (typhon@vm
                                (nth (cdar stack) 0)
                                (length (nth (cdar stack) 0))
                                (nth (cdar stack) 1)
                                0 '() 
                                locals
                                dump)
                            #f)
                    (33) ;; %define
                        (begin
                            (typhon@add-env! (car stack) (cadr stack) env)
                            (typhon@vm
                                code code-len env (+ ip 1)
                                (cons #v stack)
                                locals
                                dump))
                    (34) ;; %set!
                        (begin
                            (typhon@set-env! (car stack) (cadr stack) env)
                            (typhon@vm
                                code code-len env (+ ip 1)
                                (cons #v stack)
                                locals
                                dump))
                    (35) ;; ceil
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (ceil (car stack)) (cdr stack)) locals dump)
                    (36) ;; floor
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (floor (car stack)) (cdr stack)) locals dump)
                    (37) ;; truncate
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (truncate (car stack)) (cdr stack)) locals dump)
                    (38) ;; round
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (round (car stack)) (cdr stack)) locals dump)
                    (39) ;; inexact->exact
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (inexact->exact (car stack)) (cdr stack)) locals dump)
                    (40) ;; quotient
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (quotient (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (41) ;; modulo
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (modulo (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (42) ;; &
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (& (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (43) ;; |
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (| (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (44) ;; ^
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (^ (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (45) ;; ~
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (~ (car stack)) (cdr stack)) locals dump)
                    (46) ;; %list
                        ;; take N items off the stack, create a list, and return it
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (cslice (cdr stack) 0 (car stack))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump)
                    (47) ;; %vector
                        ;; take N items off the stack, create a list, and return it
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (coerce (cslice (cdr stack) 0 (car stack)) 'vector)
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump)
                    (48) ;; %make-vector
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-vector (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (49) ;; %make-string
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-string (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (50) ;; %string
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (apply string (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump)
                    (51) ;; %append
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (apply append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) locals dump)
                    (52) ;; first
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (first (car stack)) (cdr stack)) locals dump)
                    (53) ;; rest
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (rest (car stack)) (cdr stack)) locals dump)
                    (54) ;; ccons
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (ccons (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (55) ;; %nth
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (nth (car stack) (cadr stack) (caddr stack)) (cdddr stack)) locals dump)
                    (56) ;; keys
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (keys (car stack)) (cdr stack)) locals dump)
                    (57) ;; partial-key?
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (partial-key? (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (58) ;; cset!
                        (begin
                            (cset! (car stack) (cadr stack) (caddr stack))
                            (typhon@vm code code-len
                                env
                                (+ ip 1)
                                (cons #v (cdddr stack)) locals dump))
                    (59) ;; empty?
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (empty? (car stack)) (cdr stack)) locals dump)
                    (60) ;; gensym
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (gensym (car stack)) (cdr stack)) locals dump)
                    (61) ;; imag-part
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (imag-part (car stack)) (cdr stack)) locals dump)
                    (62) ;; real-part
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (real-part (car stack)) (cdr stack)) locals dump)
                    (63) ;; make-rectangular
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-rectangular (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (64) ;; make-polar
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-polar (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (65) ;; magnitude
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (magnitude (car stack)) (cdr stack)) locals dump)
                    (66) ;; argument
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (argument (car stack)) (cdr stack)) locals dump)
                    (67) ;; conjugate
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (conjugate (car stack)) (cdr stack)) locals dump)
                    (68) ;; conjugate
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (conjugate! (car stack)) (cdr stack)) locals dump)
                    (69) ;; polar->rectangular
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (polar->rectangular (car stack)) (cdr stack)) locals dump)
                    (70) ;; rectangular->polar
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (rectangular->polar (car stack)) (cdr stack)) locals dump)
                    (71) ;; sin
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (sin (car stack)) (cdr stack)) locals dump)
                    (72) ;; cos
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cos (car stack)) (cdr stack)) locals dump)
                    (73) ;; tan
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (tan (car stack)) (cdr stack)) locals dump)
                    (74) ;; asin
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (asin (car stack)) (cdr stack)) locals dump)
                    (75) ;; acos
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (acos (car stack)) (cdr stack)) locals dump)
                    (76) ;; atan
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (atan (car stack)) (cdr stack)) locals dump)
                    (77) ;; atan2
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (atan2 (cadr stack) (car stack)) (cddr stack)) locals dump)
                    (78) ;; sinh
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (sinh (car stack)) (cdr stack)) locals dump)
                    (79) ;; cosh
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cosh (car stack)) (cdr stack)) locals dump)
                    (80) ;; tanh
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (tanh (car stack)) (cdr stack)) locals dump)
                    (81) ;; exp
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp (car stack)) (cdr stack)) locals dump)
                    (82) ;; ln
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (ln (car stack)) (cdr stack)) locals dump)
                    (83) ;; abs
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (abs (car stack)) (cdr stack)) locals dump)
                    (84) ;; sqrt
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (sqrt (car stack)) (cdr stack)) locals dump)
                    (85) ;; exp2
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump)
                    (86) ;; expm1
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (expm1 (car stack)) (cdr stack)) locals dump)
                    (87) ;; log2
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (log2 (car stack)) (cdr stack)) locals dump)
                    (88) ;; log10
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (log10 (car stack)) (cdr stack)) locals dump)
                    (89) ;; <<
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (<< (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (90) ;; >>
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (>> (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (91) ;; %string-append
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (apply string-append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1)))
                            locals
                            dump)
                    (92) ;; assq
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (assq (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (93) ;; memq
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (memq (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (94) ;; %dict
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons
                                (apply dict (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1)))
                            locals
                            dump)
                    (95) ;; make-dict
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (make-dict) stack) locals dump)
                    (96) ;; dict-has?
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (dict-has? (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (97) ;; coerce
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (coerce (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (98) ;; cupdate
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cupdate (car stack) (cadr stack) (caddr stack)) (cdddr stack)) locals dump)
                    (99) ;; cslice
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (cslice (car stack) (cadr stack) (caddr stack)) (cdddr stack)) locals dump)
                    (100) ;; tconc!
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump)
                    (101) ;; make-tconc
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump)
                    (102) ;; tconc-list
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump)
                    (103) ;; tconc->pair
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump)
                    (104) ;; tconc-splice
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) locals dump)
                    (105) ;; rationalize
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons (rationalize (car stack) (cadr stack)) (cddr stack)) locals dump)
                    (106) ;; call/cc
                        (let ((retcode (typhon@vm (cons (list 3 (car stack)) (list (list 30))) 2 ;; there's only two instructions here ...
                                        env
                                        0
                                        (cons (list 'continuation (copy-code code ip 0) ip env stack locals dump) '())
                                        locals
                                        '())))
                         (typhon@vm code code-len
                            env
                            (+ ip 1)
                            (cons retcode (cdr stack))
                            locals
                            dump))
                    (107) ;; %nop
                        (typhon@vm code code-len
                            env
                            (+ ip 1)
                            stack
                            locals
                            dump)
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
                            (nth cont-code 6)))
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
                                dump))
                    (110) ;; call from stack
                        (let ((call-proc (car stack)))
                            ;(display "call-proc == ")
                            ;(write call-proc)
                            ;(display "\n")
                            (cond
                                (and
                                    (eq? (cdr stack) '())
                                    (not (eq? (nth (cadr call-proc) 2) '())))
                                    (typhon@error "incorrect arity for user procedure")
                                (typhon@error? call-proc)
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
                                    (if (> (car dump) (length (cadr dump)))
                                        (error "Dump stack overflow")
                                        (let ((env-and-stack (build-environment (vector-ref (cadr call-proc) 0) (cdr stack) (vector-ref (cadr call-proc) 2) locals))
                                              (v-dump (cadr dump))
                                              (offset (car dump)))
                                            ;(display "in let; (car env-and-stack) == ")
                                            ;(write (car env-and-stack))
                                            ;(newline)
                                            (vector-set! v-dump offset locals) ;; can these be made into vector-set! calls?
                                            (vector-set! v-dump (+ offset 1) (cadr env-and-stack))
                                            (vector-set! v-dump (+ offset 2) ip)
                                            (vector-set! v-dump (+ offset 3) env)
                                            (vector-set! v-dump (+ offset 4) code-len)
                                            (vector-set! v-dump (+ offset 5) code)
                                            (typhon@vm
                                                (vector-ref (cadr call-proc) 1)
                                                (length (vector-ref (cadr call-proc) 1))
                                                (car env-and-stack)
                                                0 '() 
                                                (caddr env-and-stack)
                                                (list (+ offset 6) v-dump))))
                                (typhon@primitive? (car stack)) ;; if primitives stored arity, slicing would be easy...
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
                            dump)
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
                            dump)
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
;     + this allows typhon@compile to know the proper arity, and signal an error.
;     + it also means that typhon@vm has to suddenly change: it must now unpack the actual opcode
;       before running (this might not be too bad...)
; 1 - encode the arity of primitives in a separate array.
;     + this allows typhon@compile to remain unchanged, and only minimal changes to typhon@vm
;     + this does not confer the benefits that the above does (that typhon@compile can
;       know about the arity of primitives & signal failure during code generation).

(define (typhon@init-env env)
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
    (dict-set! env "digamma-implementation" '(procedure . "digamma-implementation"))
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
    (dict-set! env "%nth" '(primitive . 55))
    (dict-set! env "nth" '(syntax . primitive-syntax-nth))
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
    (dict-set! env "type" '(primitive . 111))
    (dict-set! env "load" '(procedure . "load"))
    (dict-set! env "read" '(procedure . "read"))
    (dict-set! env "write" '(procedure . "write"))
    (dict-set! env "foo" '(procedure . "fo"))
    (dict-set! env "write-buffer" '(procedure . "write-buffer"))
    (dict-set! env "read-buffer" '(procedure . "read-buffer"))
    (dict-set! env "read-string" '(procedure . "read-string")))

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
        (null? env) (typhon@error (format "unbound variable: ~a" item)) 
        (dict-has? (car env) item) (nth (car env) item) 
        else (typhon@lookup item (cdr env))))

(define (compile-begin lst params env)
    (if (null? lst)
        '()
        (append
            (typhon@compile (car lst) params env)
            (compile-begin (cdr lst) params env))))

(define (compile-lambda rst env)
    (list 'compiled-lambda
        (vector
            env
            (compile-begin (cdr rst) (car rst) env)
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
        (null? environment) (typhon@error (format "SET! error: undefined name \"~a\"" name))
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
    (with code (typhon@compile line '() env)
        (typhon@vm code (length code) env 0 '() '() dump)))

(define (typhon@compile-help sym iter-list params env)
    " a helper function for typhon@compile, which collects
      the old use of append-map into a single function that
      Eprime can compile (still haven't added HOFs to E'...
      embarrassing, I know)
    "
    (if (null? iter-list)
        iter-list
        (append
            (typhon@compile (car iter-list) params env)
            (list (list (cdr (typhon@lookup sym env))))
            (typhon@compile-help sym (cdr iter-list) params env))))

(define (typhon@map iter-list params env)
    (if (null? iter-list)
        iter-list
        (cons
            (typhon@compile (car iter-list) params env)
            (typhon@map (cdr iter-list) params env))))

(define (typhon@compile line params env)
    (catch compile-error
        (if (null? line)
            '()
            (cond
                (vector? line) (list (list 3 line))
                (dict? line) (list (list 3 line) )
                (symbol? line) 
                    (let ((param-mem? (memq line params)))
                        (if (not (eq? param-mem? #f))
                            (list
                                (list
                                    112 ;; fast load
                                    (- 
                                        (length params)
                                        (length param-mem?))))
                            (list (list 31 line)))) ;; environment-load
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
                       (cond 
                            (not (eq? param-mem? #f))
                                (append
                                    (reverse-append 
                                        (typhon@map rst params env))
                                    (list (list 112 (- (length params) (length param-mem?))))
                                    (list (list 110 'param)))
                            (typhon@syntax? v) ;; primitive syntax
                                (cond
                                    (eq? (cdr v) 'primitive-syntax-quote)
                                        (if (null? (car rst))
                                            '((4))
                                            (list (list 3 (car rst))))
                                    (eq? (cdr v) 'primitive-syntax-nth)
                                        (cond
                                            (= (length rst) 2)
                                                (append
                                                    '((4)) ; this should really be a typhon@error
                                                    (typhon@compile (cadr rst) params env)
                                                    (typhon@compile (car rst) params env)
                                                    '((55)))
                                            (= (length rst) 3)
                                                (append
                                                    (typhon@compile (caddr rst) params env)
                                                    (typhon@compile (cadr rst) params env)
                                                    (typhon@compile (car rst) params env)
                                                    '((55)))
                                            else
                                                (throw compile-error (typhon@error "incorrect arity for NTH")))
                                    (eq? (cdr v) 'primitive-syntax-plus)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 0))
                                                    (typhon@compile (car rst) params env)
                                                    (list (list (typhon@lookup '%+ env))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env)
                                                    (typhon@compile-help '%+ (cdr rst) params env))
                                            else (list (list 3 0)))
                                    (eq? (cdr v) 'primitive-syntax-minus)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 0))
                                                    (typhon@compile (car rst) params env)
                                                    (list (list (typhon@lookup '%- env))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env)
                                                    (typhon@compile-help '%- (cdr rst) params env))
                                            else (throw compile-error (typhon@error "minus fail")))
                                    (eq? (cdr v) 'primitive-syntax-mult)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 0))
                                                    (typhon@compile (car rst) params env)
                                                    (list (list (typhon@lookup '%* env))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env)
                                                    (typhon@compile-help '%* (cdr rst) params env))
                                            else (list (list 3 1)))
                                    (eq? (cdr v) 'primitive-syntax-div)
                                        (cond
                                            (= (length rst) 1)
                                                (append '((3 1))
                                                    (typhon@compile (car rst) params env)
                                                    (list (list (typhon@lookup '%/ env))))
                                            (> (length rst) 1)
                                                (append 
                                                    (typhon@compile (car rst) params env)
                                                    (typhon@compile-help '%/ (cdr rst) params env))
                                            else (throw compile-error (typhon@error "division fail")))
                                    (eq? (cdr v) 'primitive-syntax-numeq)
                                        (cond
                                            (= (length rst) 1)
                                                (list (list 3 #t))
                                            (> (length rst) 1)
                                                (append
                                                    (typhon@compile (car rst) params env)
                                                    (typhon@compile-help '%= (cdr rst) params env))
                                            else (throw compile-error (typhon@error "numeq fail")))
                                    (eq? (cdr v) 'primitive-syntax-define)
                                        (let ((name (car rst))
                                              (value (cadr rst)))
                                            (cond
                                                (pair? name) 
                                                    (append
                                                        (typhon@compile (cons 'fn (cons (cdar rst) (cdr rst))) params env)
                                                        (list (list 3 (caar rst)))
                                                        (list (list (cdr (typhon@lookup '%define env))))) 
                                                (symbol? name)
                                                    (append
                                                        (typhon@compile value params env)
                                                        (list (list 3 name))
                                                        (list (list (cdr (typhon@lookup '%define env)))))
                                                else (throw compile-error (typhon@error "DEFINE error: define SYMBOL VALUE | DEFINE PAIR S-EXPR*"))))
                                    (eq? (cdr v) 'primitive-syntax-set)
                                        (let ((name (car rst))
                                              (value (cadr rst)))
                                           (if (symbol? name) 
                                                (append
                                                    (typhon@compile value params env)
                                                    (list (list 3 name))
                                                    (list (list (cdr (typhon@lookup '%set! env)))))
                                                (throw compile-error (typhon@error "SET!: set! SYMBOL S-EXPR*"))))
                                    (eq? (cdr v) 'primitive-syntax-defsyn)
                                        (let ((name (car rst))
                                              (rules (cdr rst)))
                                            (typhon@add-env! name (list 'user-syntax rules) env)
                                            (list (list 107))) ;; %nop
                                    (eq? (cdr v) 'primitive-syntax-defmac)
                                        #t
                                    (eq? (cdr v) 'primitive-syntax-fn)
                                        (list
                                            (list 3 ;; load
                                                (compile-lambda rst env))
                                            (list (cdr (typhon@lookup '%makeclosure env))))
                                    (eq? (cdr v) 'primitive-syntax-begin)
                                        (compile-begin rst params env)
                                    (eq? (cdr v) 'primitive-syntax-lt)
                                        (append 
                                            (typhon@compile (car rst) params env)
                                            (typhon@compile-help '%< (cdr rst) params env))
                                    (eq? (cdr v) 'primitive-syntax-gt)
                                        (append 
                                            (typhon@compile (car rst) params env)
                                            (typhon@compile-help '%> (cdr rst) params env))
                                    (eq? (cdr v) 'primitive-syntax-lte)
                                        (append 
                                            (typhon@compile (car rst) params env)
                                            (typhon@compile-help '%<= (cdr rst) params env))
                                    (eq? (cdr v) 'primitive-syntax-gte)
                                        (append 
                                            (typhon@compile (car rst) params env)
                                            (typhon@compile-help '%>= (cdr rst) params env))
                                    (eq? (cdr v) 'primitive-syntax-list)
                                        ;; if rst is null?, then generate a load-null instruction (4)
                                        ;; otherwise generate the instructions for the list, a length
                                        ;; and a call to %list
                                        (if (null? rst)
                                            (list (list 4))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env))
                                                (list (list 3 (length rst)))
                                                (list (list (cdr (typhon@lookup '%list env))))))
                                    (eq? (cdr v) 'primitive-syntax-vector)
                                        ;; if rst is null?, then generate a load with an empty vector
                                        ;; otherwise generate the instructions for the vector, a length
                                        ;; and a call to %vector
                                        (if (null? rst)
                                            (list (list 3 (make-vector 0)))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env))
                                                (list (list 3 (length rst)))
                                                (list (list (cdr (typhon@lookup '%vector env))))))
                                    (eq? (cdr v) 'primitive-syntax-dict)
                                        ;; if rst is null?, then generate a load with an empty dict
                                        ;; otherwise generate the instructions for the dict, a length
                                        ;; and a call to %dict
                                        (if (null? rst)
                                            (list (list 3 (make-dict)))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env))
                                                (list (list 3 (length rst)))
                                                (list (list (cdr (typhon@lookup '%dict env))))))
                                    (eq? (cdr v) 'primitive-syntax-string)
                                        (if (null? rst)
                                            (list (list 3 (make-string 0)))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env))
                                                (list (list 3 (length rst)))
                                                (list (list (cdr (typhon@lookup '%string env))))))
                                    (eq? (cdr v) 'primitive-syntax-append)
                                        (if (null? rst)
                                            (list (list 4))
                                            (append
                                                (reverse-append
                                                    (typhon@map rst params env))
                                                (list (list 3 (length rst)))
                                                (list (list (cdr (typhon@lookup '%append env))))))
                                        
                                    (eq? (cdr v) 'primitive-syntax-makevector)
                                        (with l (length rst)
                                            (cond
                                                (= l 1)
                                                    (append
                                                        '((4))
                                                        (typhon@compile (car rst) params env)
                                                        (list (list (cdr (typhon@lookup '%make-vector env)))))
                                                (= l 2)
                                                    (append
                                                        (reverse-append (typhon@map rst params env))
                                                        (list (list (cdr (typhon@lookup '%make-vector env)))))
                                                else (throw compile-error (typhon@error "make-vector len : INTEGER (v : SEXPR) => VECTOR"))))
                                    (eq? (cdr v) 'primitive-syntax-makestring)
                                        (with l (length rst)
                                            (cond
                                                (= l 1)
                                                    (append
                                                        '((3 #\space))
                                                        (typhon@compile (car rst) params env)
                                                        (list (list (cdr (typhon@lookup '%make-string env)))))
                                                (= l 2)
                                                    (append
                                                        (reverse-append (typhon@map rst params env))
                                                        (list (list (cdr (typhon@lookup '%make-string env)))))
                                                else (throw compile-error (typhon@error "make-string len : INTEGER (c : CHAR) => STRING"))))
                                    (eq? (cdr v) 'primitive-syntax-if)
                                        ;; need to generate code for <cond>
                                        ;; add CMP instruction '(30)
                                        ;; generate code for <then>
                                        ;; generate code for <else>
                                        ;; add count to CMP instruction to jump to <else>
                                        ;; add count to <then> to skip <else>
                                        (let* ((<cond> (typhon@compile (car rst) params env))
                                               (<then> (typhon@compile (cadr rst) params env))
                                               (<else> (typhon@compile (caddr rst) params env))
                                               (then-len (+ (length <then>) 2)) ;; +2 in order to avoid the jump over else
                                               (else-len (+ (length <else>) 1)))
                                            (append <cond>
                                                (list (list 29 then-len)) ;; compare & jump
                                                <then>
                                                (list (list 28 else-len)) ;; jump else
                                                <else>)) 
                                    else 
                                        (throw compile-error (typhon@error "syntax has not been implemented at this time")))
                                (pair? fst) 
                                    ;; fst is a pair, so we just blindly attempt to compile it.
                                    ;; May cause an error that has to be caught in CALL. some lifting might fix this...
                                    (append (reverse-append (typhon@map rst params env))
                                            (typhon@compile fst params env)
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
                                            env)) ;; TODO: don't use env, use the environment stored in the syntax object; NEW: nope; expand items in place?
                                (typhon@umacro? v)
                                    #f
                                (typhon@procedure? v) ;; need to add some method of checking proc arity here.
                                    (let* ((rlen (length rst)))
                                        (append
                                            (reverse-append (typhon@map rst params env))
                                            (list (list 16 (cdr v) rlen))))
                                (typhon@primitive? v) ;; primitive procedure
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
                                            (typhon@map rst params env))
                                        (list (list (cdr v))))
                                (typhon@lambda? v) ;; hydra closure; change this into (load-from-env fst) (call-from-stack) 
                                    (append
                                        (reverse-append
                                            (typhon@map rst params env))
                                            (list (list 31 fst))
                                            (list (list 110 'found)))
                                (typhon@continuation? v) ;; hydra continuation
                                    (append (reverse-append (typhon@map rst params env))
                                        (list (list 3 v))
                                        (list (list 108))) ;; 108 -> %ap
                                (symbol? fst) ;; fst is a symbol, but it has no mapping in our current env; write to environment-load
                                    (append
                                        (reverse-append
                                            (typhon@map rst params env)) 
                                            (list (list 31 fst))
                                            (list (list 110 'not-found)))
                                else (throw compile-error (typhon@error "error: the only applicable types are primitive procedures, closures & syntax"))))
                else (list (list 3 line))))))

;; need to separate user values from what 
;; is returned in the eval...
(define (top-level-print x)
    " print #<foo> at the top level"
    (cond
        (typhon@lambda? x) (display "#<closure>")
        (typhon@continuation? x) (display "#<continuation>")
        (typhon@primitive? x) (display (format "#<primitive-procedure ~a>" (cdr x)))
        (typhon@procedure? x) (display (format "#<procedure ~a>" (cdr x)))
        (typhon@syntax? x) (display (format "#<syntax ~a>" (cdr x)))
        (typhon@error? x) (display (format "ERROR: ~a" (cdr x)))
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
                (or
                    (eq? (cadr inp) 'h)
                    (eq? (cadr inp) 'help))
                    (with item (read)
                        (typhon@help item env)
                        (newline)
                        (typhon@repl env dump))
                (eq? (cadr inp) 'i) (with item (read) (write (typhon@lookup item env)) (newline) (typhon@repl env dump))
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
                    (if (typhon@error? r)
                        #v
                        (typhon@add-env! '_ r env))
                    (top-level-print r)
                    (display "\n")
                    (typhon@repl env dump)))))))

(define (typhon@main args)
    (let ((e {})
          (dump (make-vector 1000 #v)))
        (typhon@init-env e)
        (if (> (length args) 0)
            (begin
                (typhon@add-env! '*command-line* (cslice args 1 (length args)) (list e))
                (typhon@load (nth args 0) (list e) (list 0 dump)))
            (begin
                (display "\n\t()\n\t  ()\n\t()  ()\nDigamma/Typhon: 2012.0/r0\n")
                (typhon@add-env! '*command-line* '() (list e))
                (typhon@repl (list e) (list 0 dump))))))

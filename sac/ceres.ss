;; A simple hybrid register-stack VM for Digamma
;; Meant to support both interpretation in Vesta as
;; well as compilation with Enyalios
;; copyright 2011 Stefan Edwards; please see the LICENSE
;; file for details

;; TODO:
;; registers that point to locations in the stack. %r0 -> stack[0], %r1 -> stack[1], &c.
;; how many?
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
(define-syntax caar ((caar x) (car (car x))))
(define-syntax cadr ((cadr x) (car (cdr x))))

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

(define (cadddar x) (car (cdddar x)))

;; cheat: implement "frationalize" within hydra
;; work around until I get it working within C
(define (frationalize x y)
    #f)

;; naieve map, foreach, append-map
(define (map proc c)
    (if (empty? c)
        c
        (ccons (proc (first c)) (map proc (rest c)))))

;(define (foreach proc c)
;    (if (empty? c)
;        #v
;        (begin
;            (proc (first c))
;            (foreach proc (rest c)))))

(define (append-map f x)
    (if (null? x)
        x
        (append (f (car x)) (append-map f (cdr x)))))

;; end mini-prelude.

(define-syntax ceres@instruction () 
    ((ceres@instruction c) (car c)))

(define-syntax ceres@operand ()
    ((ceres@operand c) (cadr c)))

(define (loop-set-env! env params vals)
    (if (null? params)
        #v
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
                (begin 
                    (loop-set-env!
                        nu-env
                        params (cslice stack 0 lp))
                    (list (cons nu-env environment) (cslice stack lp ls)))))))

(define (copy-code code ip offset)
    " copies the spine of code, but at ip & ip+1, insert %nop instructions
      instead, over-writing the call/cc & load-lambda instructions therein.
      "
    (cond
        (null? code) '()
        (= offset (- ip 1)) (append (list '(107) '(107)) (copy-code (cddr code) ip (+ offset 2)))
        else (append (list (car code)) (copy-code (cdr code) ip (+ offset 1)))))

(define (ceres@vm code env ip stack dump)
     " process the actual instructions of a code object; the basic idea is that
       the user enters:
       h; (car (cdr (cons 1 (cons 2 '()))))
       which is compiled by ceres@eval into:
       (4)   ;; nil
       (3 2) ;; load 2
       (2)   ;; cons
       (3 1) ;; load 1
       (2)   ;; cons
       (1)   ;; cdr
       (0)   ;; car
       
       which ceres@vm can then interpret in a tail-call fashion.
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
            (ceres@error? (car stack)))
            (car stack)
        (>= ip (length code))
        (if (null? dump)
            (car stack)
            (with top-dump (car dump)
                (ceres@vm
                    (nth top-dump 0)
                    (nth top-dump 1)
                    (+ (nth top-dump 2) 1)
                    (cons (car stack) (nth top-dump 3))
                    (cdr dump))))
         else
         (let* ((c (nth code ip))
                (instr (ceres@instruction c)))
              ;(display (format "current ip: ~n~%" ip))
              ;(display "current instruction: ")
              ;(display (nth code ip))
              ;(display "\n")
              ;(display "current stack: ")
              ;(write stack)
              ;(display "\n")
              (cond ;; case would make a lot of sense here...
                  (eq? instr 0) ;; car
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (car (car stack)) (cdr stack)) dump)
                  (eq? instr 1) ;; cdr
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (cdr (car stack)) (cdr stack)) dump)
                  (eq? instr 2) ;; cons
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (cons (car stack)
                                                (cadr stack))
                                       (cddr stack)) dump)
                  (eq? instr 3) ;; load
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (ceres@operand c) stack) dump)
                  (eq? instr 4) ;; nil
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons '() stack) dump)
                  (eq? instr 5) ;; -
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (- (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 6) ;; +
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (+ (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 7) ;; * 
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (* (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 8) ;; / 
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (/ (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 9) ;;  < 
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (< (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 10) ;; >
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (> (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 11) ;; <= 
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (<= (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 12) ;; >= 
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (>= (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 13) ;; length
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (length (car stack)) (cdr stack)) dump)
                  (eq? instr 14) ;; exact?
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (exact? (car stack)) (cdr stack)) dump)
                  (eq? instr 15) ;; inexact?
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (inexact? (car stack)) (cdr stack)) dump)
                  (eq? instr 16) ;; display
                    (begin
                        (display (car stack))
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons #v (cdr stack)) dump))
                  (eq? instr 18) ;; real?
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (real? (car stack)) (cdr stack)) dump)
                  (eq? instr 19) ;; integer?
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (integer? (car stack)) (cdr stack)) dump)
                  (eq? instr 20) ;; complex?
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (complex? (car stack)) (cdr stack)) dump)
                  (eq? instr 21) ;; rational?
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (rational? (car stack)) (cdr stack)) dump)
                  (eq? instr 22) ;; gcd
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (gcd (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 23) ;; lcm
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (lcm (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 24) ;; numerator 
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (numerator (car stack)) (cdr stack)) dump)
                  (eq? instr 25) ;; denomenator
                        (ceres@vm code
                                  env
                                  (+ ip 1)
                                  (cons (denomenator (car stack)) (cdr stack)) dump)
                  (eq? instr 26) ;; = 
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (= (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 27) ;; eq?
                        (ceres@vm code
                                 env
                                 (+ ip 1)
                                 (cons (eq? (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 28) ;; jump
                        (ceres@vm code
                                 env
                                 (+ ip (ceres@operand c))
                                 stack dump)
                  (eq? instr 29) ;; cmp
                        (if (car stack) ;; if the top of the stack is true
                            (ceres@vm code env (+ ip 1) (cdr stack) dump) ;; jump to the <then> portion
                            (ceres@vm code env (+ ip (ceres@operand c)) (cdr stack) dump))
                  (eq? instr 30) ;; call
                        ;; need to make call check it's operand now...
                        (let ((call-proc (ceres@operand c)))
                            (if (symbol? call-proc)
                                (set! call-proc (ceres@lookup call-proc env))
                                #v)
                            ;(display "call-proc: ")
                            ;(write call-proc)
                            ;(newline)
                            (cond
                                (ceres@error? call-proc)
                                    call-proc
                                (ceres@lambda? call-proc)
                                    ;; create a list from the current registers, cons this to dump, and 
                                    ;; recurse over ceres@vm. 
                                    ;; need to support CALLing primitives too, since they could be passed
                                    ;; in to HOFs...
                                    (let ((env-and-stack (build-environment (nth (cadr call-proc) 0) stack (nth (cadr call-proc) 2))))
                                        (ceres@vm
                                            (nth (cadr call-proc) 1)
                                            (car env-and-stack)
                                            0 '() 
                                            (cons (list code env ip (cadr env-and-stack)) dump)))
                                (ceres@primitive? (car stack)) ;; if primitives stored arity, slicing would be easy...
                                    (begin
                                        (display "in ceres@primitive\n\t")
                                        (display (car stack))
                                        (display "\n")
                                        #t)
                                ;;(ceres@procedure? (car stack))
                                ;;    #t
                                else
                                (begin
                                    (display "in <else> of CALL\n")
                                    (display (car stack))
                                    (display "\n")
                                #f)))
                  (eq? instr 31) ;; environment-load; there is never a raw #f, so this is safe
                        (with r (ceres@lookup (ceres@operand c) env)
                            (if (ceres@error? r)
                                r
                                (ceres@vm
                                    code 
                                    env
                                    (+ ip 1) 
                                    (cons r stack)
                                    dump)))
                  (eq? instr 32) ;; tail-call 
                        (if (and (not (null? stack)) (eq? (caar stack) 'compiled-lambda))
                            (ceres@vm
                                (nth (cdar stack) 0)
                                (nth (cdar stack) 1)
                                0 '() 
                                dump)
                            #f)
                  (eq? instr 33) ;; %define
                        (begin
                            (ceres@add-env! (car stack) (cadr stack) env)
                            (ceres@vm
                                code env (+ ip 1)
                                (cons #v stack)
                                dump))
                  (eq? instr 34) ;; %set!
                        (begin
                            (ceres@set-env! (car stack) (cadr stack) env)
                            (ceres@vm
                                code env (+ ip 1)
                                (cons #v stack)
                                dump))
                  (eq? instr 35) ;; ceil
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (ceil (car stack)) (cdr stack)) dump)
                  (eq? instr 36) ;; floor
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (floor (car stack)) (cdr stack)) dump)
                  (eq? instr 37) ;; truncate
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (truncate (car stack)) (cdr stack)) dump)
                  (eq? instr 38) ;; round
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (round (car stack)) (cdr stack)) dump)
                  (eq? instr 39) ;; inexact->exact
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (inexact->exact (car stack)) (cdr stack)) dump)
                  (eq? instr 40) ;; quotient
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (quotient (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 41) ;; modulo
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (modulo (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 42) ;; &
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (& (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 43) ;; |
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (| (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 44) ;; ^
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (^ (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 45) ;; ~
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (~ (car stack)) (cdr stack)) dump)
                  (eq? instr 46) ;; %list
                        ;; take N items off the stack, create a list, and return it
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons
                                (cslice (cdr stack) 0 (car stack))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 47) ;; %vector
                        ;; take N items off the stack, create a list, and return it
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons
                                (coerce (cslice (cdr stack) 0 (car stack)) 'vector)
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 48) ;; %make-vector
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (make-vector (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 49) ;; %make-string
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (make-string (car stack) (cadr stack)) (cddr stack)) dump)
                  (eq? instr 50) ;; %string
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply string (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 51) ;; %append
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons
                                (apply append (cslice (cdr stack) 0 (car stack)))
                                (cslice (cdr stack) (car stack) (- (length stack) 1))) dump)
                  (eq? instr 52) ;; first
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (first (car stack)) (cdr stack)) dump)
                  (eq? instr 53) ;; rest
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (rest (car stack)) (cdr stack)) dump)
                  (eq? instr 54) ;; ccons
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (ccons (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 55) ;; %nth
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (nth (caddr stack) (cadr stack) (car stack)) (cdddr stack)) dump)
                  (eq? instr 56) ;; keys
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (keys (car stack)) (cdr stack)) dump)
                  (eq? instr 57) ;; partial-key?
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (partial-key? (cadr stack) (car stack)) (cddr stack)) dump)
                  (eq? instr 58) ;; cset!
                        (begin
                            (cset! (caddr stack) (cadr stack) (car stack))
                            (ceres@vm code
                                env
                                (+ ip 1)
                                (cons #v (cdddr stack)) dump))
                  (eq? instr 59) ;; empty?
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (empty? (car stack)) (cdr stack)) dump)
                  (eq? instr 60) ;; gensym
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (gensym (car stack)) (cdr stack)) dump)
                  (eq? instr 61) ;; imag-part
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (imag-part (car stack)) (cdr stack)) dump)
                  (eq? instr 62) ;; real-part
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (real-part (car stack)) (cdr stack)) dump)
                    (eq? instr 63) ;; make-rectangular
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (make-rectangular (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 64) ;; make-polar
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (make-polar (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 65) ;; magnitude
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (magnitude (car stack)) (cdr stack)) dump)
                    (eq? instr 66) ;; argument
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (argument (car stack)) (cdr stack)) dump)
                    (eq? instr 67) ;; conjugate
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (conjugate (car stack)) (cdr stack)) dump)
                    (eq? instr 68) ;; conjugate
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (conjugate! (car stack)) (cdr stack)) dump)
                    (eq? instr 69) ;; polar->rectangular
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (polar->rectangular (car stack)) (cdr stack)) dump)
                    (eq? instr 70) ;; rectangular->polar
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (rectangular->polar (car stack)) (cdr stack)) dump)
                    (eq? instr 71) ;; sin
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (sin (car stack)) (cdr stack)) dump)
                    (eq? instr 72) ;; cos
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (cos (car stack)) (cdr stack)) dump)
                    (eq? instr 73) ;; tan
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (tan (car stack)) (cdr stack)) dump)
                    (eq? instr 74) ;; asin
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (asin (car stack)) (cdr stack)) dump)
                    (eq? instr 75) ;; acos
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (acos (car stack)) (cdr stack)) dump)
                    (eq? instr 76) ;; atan
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (atan (car stack)) (cdr stack)) dump)
                    (eq? instr 77) ;; atan2
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (atan2 (cadr stack) (car stack)) (cddr stack)) dump)
                    (eq? instr 78) ;; sinh
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (sinh (car stack)) (cdr stack)) dump)
                    (eq? instr 79) ;; cosh
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (cosh (car stack)) (cdr stack)) dump)
                    (eq? instr 80) ;; tanh
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (tanh (car stack)) (cdr stack)) dump)
                    (eq? instr 81) ;; exp
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp (car stack)) (cdr stack)) dump)
                    (eq? instr 82) ;; ln
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (ln (car stack)) (cdr stack)) dump)
                    (eq? instr 83) ;; abs
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (abs (car stack)) (cdr stack)) dump)
                    (eq? instr 84) ;; sqrt
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (sqrt (car stack)) (cdr stack)) dump)
                    (eq? instr 85) ;; exp2
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 86) ;; expm1
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (expm1 (car stack)) (cdr stack)) dump)
                    (eq? instr 87) ;; log2
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (log2 (car stack)) (cdr stack)) dump)
                    (eq? instr 88) ;; log10
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (log10 (car stack)) (cdr stack)) dump)
                    (eq? instr 89) ;; <<
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (<< (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 90) ;; >>
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (>> (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 91) ;; %string-append
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 92) ;; assq
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (assq (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 93) ;; memq
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (memq (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 94) ;; %dict
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 95) ;; make-dict
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (make-dict) stack) dump)
                    (eq? instr 96) ;; dict-has?
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (dict-has? (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 97) ;; coerce
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (coerce (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 98) ;; cupdate
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (cupdate (car stack) (cadr stack) (caddr stack)) (cdddr stack)) dump)
                    (eq? instr 99) ;; cslice
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (cslice (car stack) (cadr stack) (caddr stack)) (cdddr stack)) dump)
                    (eq? instr 100) ;; tconc!
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 101) ;; make-tconc
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 102) ;; tconc-list
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 103) ;; tconc->pair
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 104) ;; tconc-splice
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (exp2 (car stack)) (cdr stack)) dump)
                    (eq? instr 105) ;; rationalize
                        (ceres@vm code
                            env
                            (+ ip 1)
                            (cons (rationalize (car stack) (cadr stack)) (cddr stack)) dump)
                    (eq? instr 106) ;; call/cc
                        (let ((retcode (ceres@vm (cons (list 3 (car stack)) (list (list 30)))
                                        env
                                        0
                                        (cons (list 'continuation (copy-code code ip 0) ip env stack dump) '())
                                        '())))
                         (ceres@vm code
                            env
                            (+ ip 1)
                            (cons retcode (cdr stack))
                            dump))
                    (eq? instr 107) ;; %nop
                        (ceres@vm code
                            env
                            (+ ip 1)
                            stack
                            dump)
                    (eq? instr 108) ;; %ap
                        (let ((cont-code (car stack))
                              (v (cadr stack)))
                         (ceres@vm 
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
                            (ceres@vm
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
;     + this allows ceres@compile to know the proper arity, and signal an error.
;     + it also means that ceres@vm has to suddenly change: it must now unpack the actual opcode
;       before running (this might not be too bad...)
; 1 - encode the arity of primitives in a separate array.
;     + this allows ceres@compile to remain unchanged, and only minimal changes to ceres@vm
;     + this does not confer the benefits that the above does (that ceres@compile can
;       know about the arity of primitives & signal failure during code generation).

(define (ceres@init-env env)
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
    (dict-set! env "display" '(primitive . 16))
    (dict-set! env "dict" '(primitive . 94))
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
    (dict-set! env "string-append" '(primitive . 91))
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
    (dict-set! env "read" '(procedure . "read"))
    (dict-set! env "write" '(procedure . "write"))
    (dict-set! env "foo" '(procedure . "fo"))
    (dict-set! env "write-buffer" '(procedure . "write-buffer"))
    (dict-set! env "read-buffer" '(procecure . "read-buffer"))
    (dict-set! env "read-string" '(procedure . "read-string")))

(define (ceres@lookup item env)
    " look up item in the current environment, returning #f for not found"
    (cond
        (not (symbol? item)) item ;; to support ((fn (x) (+ x x)) (+ x x) 3)
        (null? env) (ceres@error (format "unbound variable: ~a" item)) 
        (dict-has? (car env) item) (nth (car env) item)
        else (ceres@lookup item (cdr env))))

(define (compile-lambda-helper lst env)
    (if (null? lst)
        '()
        (append
            (ceres@compile (car lst) env)
            (compile-lambda-helper (cdr lst) env))))

(define (compile-lambda rst env)
    (list 'compiled-lambda
        (vector
            env
            (compile-lambda-helper (cdr rst) env)
            (car rst)))) 

(define (ceres@lambda? x)
    (and (pair? x) (eq? (car x) 'compiled-lambda)))

(define (ceres@primitive? x)
    (and (pair? x) (eq? (car x) 'primitive)))

(define (ceres@syntax? x)
    (and (pair? x) (eq? (car x) 'syntax)))

(define (ceres@error? x)
    (and (pair? x) (eq? (car x) 'error)))

(define (ceres@continuation? x)
    (and (pair? x) (eq? (car x) 'continuation)))

(define (ceres@add-env! name value environment)
    " adds name to the environment, but also returns
      (load #v), so that the compiler adds the correct
      value (this is in the semantics of Vesta, so I thought
      it should be left in Hydra as well)"
    (cset! (car environment) name value))

(define (ceres@set-env! name value environment)
    " sets a value in the current environment, and returns
      an error if that binding has not been previously defined"
    (cond
        (null? environment) (ceres@error (format "SET! error: undefined name \"~a\"" name))
        (dict-has? (car environment) name)
            (cset! (car environment) name value)
        else (ceres@set-env! name value (cdr environment))))

(define (reverse-append x)
    "append but in reverse"
    (cond
        (null? x) x
        (null? (cdr x)) (car x)
        else (append (reverse-append (cddr x)) (cadr x) (car x))))

(define (show x m) (display m) (display x) (display "\n") x)

(define (ceres@error msg)
    "simple, hydra specific errors"
    (list 'error msg))

(define (ceres@eval line env)
    "simple wrapper around ceres@vm & ceres@compile"
    (ceres@vm (ceres@compile line env) env 0 '() '()))

(define (ceres@compile-help sym iter-list env)
    " a helper function for ceres@compile, which collects
      the old use of append-map into a single function that
      Eprime can compile (still haven't added HOFs to E'...
      embarrassing, I know)
    "
    (if (null? iter-list)
        iter-list
        (append
            (ceres@compile (car iter-list) env)
            (list (list (cdr (ceres@lookup sym env))))
            (ceres@compile-help sym (cdr iter-list) env))))

(define (ceres@map iter-list env)
    (if (null? iter-list)
        iter-list
        (cons
            (ceres@compile (car iter-list) env)
            (ceres@map (cdr iter-list) env))))

(define (ceres@compile line env)
    (if (null? line)
        '()
        (cond
            (vector? line) (list (list 3 line))
            (dict? line) (list (list 3 line) )
            (symbol? line) (list (list 31 line)) ;; environment-load
            (pair? line) 
                (let* ((fst (car line)) ;; decompose line into first & rest
                       (v (ceres@lookup fst env)) ;; find fst in env
                       (rst (cdr line))) 
                   (cond 
                        (ceres@syntax? v) ;; primitive syntax
                            (cond
                                (eq? (cdr v) 'primitive-syntax-quote)
                                    (if (null? (car rst))
                                        '((4))
                                        (list (list 3 (car rst))))
                                (eq? (cdr v) 'primitive-syntax-plus)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 0))
                                                (ceres@compile (car rst) env)
                                                (list (list (ceres@lookup '%+ env))))
                                        (> (length rst) 1)
                                            (append 
                                                (ceres@compile (car rst) env)
                                                (ceres@compile-help '%+ (cdr rst) env))
                                        else (list (list 3 0)))
                                (eq? (cdr v) 'primitive-syntax-minus)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 0))
                                                (ceres@compile (car rst) env)
                                                (list (list (ceres@lookup '%- env))))
                                        (> (length rst) 1)
                                            (append 
                                                (ceres@compile (car rst) env)
                                                (ceres@compile-help '%- (cdr rst) env))
                                        else (error "minus fail"))
                                (eq? (cdr v) 'primitive-syntax-mult)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 0))
                                                (ceres@compile (car rst) env)
                                                (list (list (ceres@lookup '%* env))))
                                        (> (length rst) 1)
                                            (append 
                                                (ceres@compile (car rst) env)
                                                (ceres@compile-help '%* (cdr rst) env))
                                        else (list (list 3 1)))
                                (eq? (cdr v) 'primitive-syntax-div)
                                    (cond
                                        (= (length rst) 1)
                                            (append '((3 1))
                                                (ceres@compile (car rst) env)
                                                (list (list (ceres@lookup '%/ env))))
                                        (> (length rst) 1)
                                            (append 
                                                (ceres@compile (car rst) env)
                                                (ceres@compile-help '%/ (cdr rst) env))
                                        else (error "division fail"))
                                (eq? (cdr v) 'primitive-syntax-numeq)
                                    (cond
                                        (= (length rst) 1)
                                            (list (list 3 #t))
                                        (> (length rst) 1)
                                            (append
                                                (ceres@compile (car rst) env)
                                                (ceres@compile-help '%= (cdr rst) env))
                                        else (error "numeq fail"))
                                (eq? (cdr v) 'primitive-syntax-define)
                                    (let ((name (car rst))
                                           (value (cadr rst)))
                                        (cond
                                            (pair? name) 
                                                (append
                                                    (ceres@compile (cons 'fn (cons (cdar rst) (cdr rst))) env)
                                                    (list (list 3 (caar rst)))
                                                    (list (list (cdr (ceres@lookup '%define env))))) 
                                            (symbol? name)
                                                (append
                                                    (ceres@compile value env)
                                                    (list (list 3 name))
                                                    (list (list (cdr (ceres@lookup '%define env)))))
                                            else (error "DEFINE error: define SYMBOL VALUE | DEFINE PAIR S-EXPR*")))
                                (eq? (cdr v) 'primitive-syntax-set)
                                    (let ((name (car rst))
                                          (value (cadr rst)))
                                       (if (symbol? name) 
                                            (append
                                                (ceres@compile value env)
                                                (list (list 3 name))
                                                (list (list (cdr (ceres@lookup '%set! env)))))
                                            (error "SET!: set! SYMBOL S-EXPR*")))
                                (eq? (cdr v) 'primitive-syntax-defsyn)
                                    #t
                                (eq? (cdr v) 'primitive-syntax-defmac)
                                    #t
                                (eq? (cdr v) 'primitive-syntax-fn)
                                    (list
                                        (list 3 ;; load
                                            (compile-lambda rst env))
                                        (list (cdr (ceres@lookup '%makeclosure env))))
                                (eq? (cdr v) 'primitive-syntax-lt)
                                    (append 
                                        (ceres@compile (car rst) env)
                                        (ceres@compile-help '%< (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-gt)
                                    (append 
                                        (ceres@compile (car rst) env)
                                        (ceres@compile-help '%> (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-lte)
                                    (append 
                                        (ceres@compile (car rst) env)
                                        (ceres@compile-help '%<= (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-gte)
                                    (append 
                                        (ceres@compile (car rst) env)
                                        (ceres@compile-help '%>= (cdr rst) env))
                                (eq? (cdr v) 'primitive-syntax-list)
                                    ;; if rst is null?, then generate a load-null instruction (4)
                                    ;; otherwise generate the instructions for the list, a length
                                    ;; and a call to %list
                                    (if (null? rst)
                                        (list (list 4))
                                        (append
                                            (reverse-append
                                                (ceres@map rst env))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (ceres@lookup '%list env))))))
                                (eq? (cdr v) 'primitive-syntax-vector)
                                    ;; if rst is null?, then generate a load-null instruction (4)
                                    ;; otherwise generate the instructions for the list, a length
                                    ;; and a call to %vector
                                    (if (null? rst)
                                        (list (list 4))
                                        (append
                                            (reverse-append
                                                (ceres@map rst env))
                                            (list (list 3 (length rst)))
                                            (list (list (cdr (ceres@lookup '%vector env))))))
                                (eq? (cdr v) 'primitive-syntax-makevector)
                                    (with l (length rst)
                                        (cond
                                            (= l 1)
                                                (append
                                                    '((4))
                                                    (ceres@compile (car rst) env)
                                                    (list (list (cdr (ceres@lookup '%make-vector env)))))
                                            (= l 2)
                                                (append
                                                    (reverse-append (ceres@map rst env))
                                                    (list (list (cdr (ceres@lookup '%make-vector env)))))
                                            else (ceres@error "make-vector len : INTEGER (v : SEXPR) => VECTOR")))
                                (eq? (cdr v) 'primitive-syntax-makestring)
                                    (with l (length rst)
                                        (cond
                                            (= l 1)
                                                (append
                                                    '((3 #\space))
                                                    (ceres@compile (car rst) env)
                                                    (list (list (cdr (ceres@lookup '%make-string env)))))
                                            (= l 2)
                                                (append
                                                    (reverse-append (ceres@map rst env))
                                                    (list (list (cdr (ceres@lookup '%make-string env)))))
                                            else (ceres@error "make-string len : INTEGER (c : CHAR) => STRING")))
                                (eq? (cdr v) 'primitive-syntax-if)
                                    ;; need to generate code for <cond>
                                    ;; add CMP instruction '(30)
                                    ;; generate code for <then>
                                    ;; generate code for <else>
                                    ;; add count to CMP instruction to jump to <else>
                                    ;; add count to <then> to skip <else>
                                    (let* ((<cond> (ceres@compile (car rst) env))
                                           (<then> (ceres@compile (cadr rst) env))
                                           (<else> (ceres@compile (caddr rst) env))
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
                                (append (reverse-append (ceres@map rst env))
                                        (ceres@compile fst env)
                                        (list (list 30)))
                            (ceres@primitive? v) ;; primitive procedure
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
                                        (ceres@map rst env))
                                    (list (list (cdr v))))
                            (ceres@lambda? v) ;; hydra closure
                                (append (reverse-append (ceres@map rst env))
                                            (list (list 30 v)))
                            (ceres@continuation? v) ;; hydra continuation
                                (append (reverse-append (ceres@map rst env))
                                    (list (list 3 v))
                                    (list (list 108))) ;; 108 -> %ap
                            (symbol? fst) ;; fst is a symbol, but it has no mapping in our current env; write to environment-load
                                (append (reverse-append
                                            (ceres@map rst env)) 
                                            (list (list 30 fst)))
                            else (error "error: the only applicable types are primitive procedures, closures & syntax")))

            else (list (list 3 line)))))

;; need to separate user values from what 
;; is returned in the eval...
(define (top-level-print x)
    " print #<foo> at the top level"
    (cond
        (ceres@lambda? x) (display "#<closure>")
        (ceres@continuation? x) (display "#<continuation>")
        (ceres@primitive? x) (display (format "#<primitive-procedure ~a>" (cdr x)))
        (ceres@syntax? x) (display (format "#<syntax ~a>" (cdr x)))
        (ceres@error? x) (display (format "ERROR: ~a" (cdr x)))
        else (display x)))

(define (ceres@load-loop fh env)
    (let ((o (read fh)))
        (if (eq? o #e)
            #v
            (begin
                (ceres@eval o env) 
                (ceres@load-loop fh env)))))

(define (ceres@load src-file env)
    "an implementation of the primitive procedure load"
    (let ((f (open src-file :read)))
        (ceres@load-loop f env)
        (close f)))
                                    
(define (ceres@repl env)
    (display "h; ")
    (with inp (read)
     (if (and (eq? (type inp) "Pair") (eq? (car inp) 'unquote))
        (cond
         (eq? (cadr inp) 'exit) #v
         (eq? (cadr inp) 'q) #v
         (eq? (cadr inp) 'quit) #v
         (eq? (cadr inp) 'bye) #v
         (eq? (cadr inp) 'dribble) (begin (ceres@repl env))
         (eq? (cadr inp) 'save) (begin (ceres@repl env))
         (eq? (cadr inp) 'save-and-die) (begin (ceres@repl env))
         else (begin (display (format "Unknown command: ~a~%" (cadr inp))) (ceres@repl env)))
        (if (not (pair? inp))
            (if (eq? inp #v)
                (ceres@repl env)
                (begin
                    (top-level-print (ceres@lookup inp env))
                    (display "\n")
                    (ceres@repl env)))
            (with r (ceres@eval inp env) 
                (if (eq? r #v)
                 (ceres@repl env)
                 (begin
                    (top-level-print r)
                    (display "\n")
                    (ceres@repl env))))))))

(define (ceres@main args)
    (let ((e {}))
        (ceres@init-env e)
        (if (> (length args) 0)
            (begin
                (ceres@add-env! '*command-line* (cslice args 1 (length args)) (list e))
                (ceres@load (nth args 0) (list e)))
            (begin
                (display "\n\t()\n\t  ()\n\t()  ()\nDigamma/Ceres: 2012.0/r0\n")
                (ceres@add-env! '*command-line* '() (list e))
                (ceres@repl (list e))))))

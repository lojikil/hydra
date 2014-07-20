; from: http://en.wikipedia.org/wiki/Unification_(computer_science)

;In Prolog:
;1. A variable which is uninstantiated—i.e. no previous unifications were
;   performed on it—can be unified with an atom, a term, or another unins-
;   tantiated variable, thus effectively becoming its alias. In many
;   modern Prolog dialects and in first-order logic, a variable cannot
;   be unified with a term that contains it; this is the so called
;   occurs check.
;2. Two atoms can only be unified if they are identical.
;3. Similarly, a term can be unified with another term if the top funct-
;   ion symbols and arities of the terms are identical and if the
;   parameters can be unified simultaneously. Note that this is a
;   recursive behavior.

;; Todo:
;; - add some sort of run*
;; - support Union types (aka "assq all lists to check list intersecion :D)
;; - make every/any and support a mini eval system...

;; make this less prolog-like, and more an interpreter. Yes we need to be able
;; to unify things, but more the point is that:
;; - ANY/EVERY should work
;; - We needn't have types on both the left & the right
;; - We should be able to support types like (char Dict) or (Int Vector)
;; - Some basic-level of constraint should be able to be generated

(define (show x (msg '())) 
    (if (null? msg)
        (display "x: ")
        (display msg)) 
    (write x)
    (newline)
    x)

(define (var? x)
    (and (pair? x) (eq? (car x) '?)))

(define (every? proc lst)
    (cond
        (null? lst) #t
        (proc (car lst)) (every? proc (cdr lst))
        else #f))

(define (success? x)
    (and
        (pair? x)
        (eq? (car x) #s)))

(define (every-type? types container)
    (cond
        (empty? container) #s
        (type-checks-out? (first container) types)
            (every-type? types (rest container))
        (success? (compound-checks-out? (first container) types))
            (every-type? types (rest container))
        else
            #u))

(define (user-type? x)
    "looks up if the atom specified by X is a user type."
    #f)

;; TODO: one of these types needs to accept that variables are
;; part of types as well. Also, type? & compound-type? need to 
;; have some mechanism to lookup return types from procedures

(define (compound-type? x)
    "is the type passed in a compound-able primitive type? Read, is the
     type a Dict, Vector, Tree or Pair, and does the other types check?
     "
    (and
        (pair? x)
        (or
            (eq? (car x) 'Dict)
            (eq? (car x) 'Vector)
            (eq? (car x) 'Pair)
            (eq? (car x) 'Produt)
            (eq? (car x) 'Sum))
        (or ;; DONE: ohhhhh; this is kinda wrong... the compound type fails, then (Pair f) => Pair, which is #t. Ugh
            (and
                (> (length x) 2)
                (compound-type? (cdr x))) 
            (and
                (= (length x) 2) 
                (type? (cadr x))))))

(define (type-var? x) ;; just (var? x) ?
    #f)

(define (type? x)
    "verifies that the object is indeed a type. Types are either simple
     (Pair, Any, Number, Integer, String, &c.) or Compound (Pair Pair Integer, 
     Dict String, &c.)."
    (or
        (eq? x 'Any)
        (eq? x 'Union)
        (eq? x 'Product)
        (eq? x 'Integer)
        (eq? x 'Int)
        (eq? x 'Real)
        (eq? x 'Rational)
        (eq? x 'Complex)
        (eq? x 'Number)
        (eq? x 'String)
        (eq? x 'Atom)
        (eq? x 'Key)
        (eq? x 'Pair)
        (eq? x 'Vector)
        (eq? x 'Dict)
        (eq? x 'Port)
        (eq? x 'Void)
        (eq? x 'Bool)
        (eq? x 'Goal)
        (eq? x 'Nil)
        (compound-type? x)
        #f))
        ;;(user-type? x) ;; structs & newtypes...
        ;;(and
        ;;    (pair? x)
        ;;    (every? type? x))))

(define (compound-checks-out? obj obj-type)
    (display "Type: ")
    (write obj-type)
    (newline)
    (cond
        (not (pair? obj-type)) (type-checks-out? obj obj-type)
        (eq? (car obj-type) 'Pair)
            (if (eq? (type obj) "Pair")
                (if (compound-type? (cdr obj-type))
                    (show (every-type? (cdr obj-type) obj))
                    (every-type? (cadr obj-type) obj))
                #u)
        (eq? (car obj-type) 'Vector)
            (and
                (eq? (type obj) "Vector")
                (every-type? (cdr obj-type) obj))
        (eq? (car obj-type) 'Dict)
            (and
                (eq? (type obj) "Dict")
                (every-type? (cdr obj-type) obj))
        (eq? (car obj-type) 'Product) ;; this is just an every check, no?
            #f
        (eq? (car obj-type) 'Sum)
            #f
        else
            #f))

(define (type-checks-out? obj type)
    "A course grained check to see if the type of `obj` matches what is
    specified by `type`. Does not handle:
    - sub-typing
    - constraints
    - Polymorphic variants

    just yet. All of those are on the to-do list for this. *however*, as
    a first pass, this isn't bad; basically, this is a PoC to make sure
    I'm not totally off my rocker.

    Also, this needs to work with things like:

    - (Int Vector)
    - ((? a) Dict)
    - user structs with types (how does one encode that? (Product ?a ?a) ?

    and the like. Lots of work to be done here...

    Ocaml notes records thusly:
    # type ratio = {num: int; denom: int};;
    type ratio = { num : int; denom : int; }

    # let add_ratio r1 r2 =
        {num = r1.num * r2.denom + r2.num * r1.denom;
             denom = r1.denom * r2.denom};;
             val add_ratio : ratio -> ratio -> ratio = <fun>

    # add_ratio {num=1; denom=3} {num=2; denom=5};;
             - : ratio = {num = 11; denom = 15}

    Which is interesting... of course, tuples are denoted differently:

    # ('a','b')
      ;;
      - : char * char = ('a', 'b')"
    (display "type: ")
    (write type)
    (display " and obj: ")
    (write obj)
    (newline)
    (or
        (eq? type 'Any)
        (and
            (show (compound-type? type) "compound-type? ")
            (compound-checks-out? type obj))
        (and 
            (show (eq? type 'Integer) "integer check? ")
            (show (integer? obj) "integer? return: "))
        (and
            (eq? type 'Real)
            (real? obj))
        (and
            (eq? type 'Rational)
            (rational? obj))
        (and
            (eq? type 'Complex)
            (complex? obj))
        (and
            (eq? type 'Number)
            (number? obj))
        (and
            (eq? type 'String)
            (string? obj))
        (and
            (eq? type 'Symbol)
            (symbol? obj))
        (and
            (eq? type 'Key)
            (key? obj))
        (and
            (eq? type 'Pair)
            (pair? obj))
        (and
            (eq? type 'Vector)
            (vector? obj))
        (and
            (eq? type 'Dict)
            (dict? obj))
        (and
            (eq? type 'Port)
            (port? obj))
        (and
            (eq? type 'Void)
            (eq? obj #v))
        (and
            (eq? type 'Bool)
            (boolean? obj))
        (and
            (eq? type 'Goal)
            (goal? obj))
        (and
            (eq? type 'Nil)
            (eq? obj '()))))

(define (run$ o0 o1 env)
    (cond
        (and
            (null? o0)
            (null? o1))
            (list #s env)
        (null? o0) (list #u)
        (null? o1) (list #u)
        (or
            (not (pair? o0))
            (not (pair? o1)))
            #u
        (var? (car o0))
            (cond
                (var? (car o1))
                    (let ((v0 (assq (cadar o0) env))
                          (v1 (assq (cadar o1) env)))
                        (cond
                            (eq? v0 #f) (run$ (cdr o0) (cdr o1) (cons (list (cadar o0) o1) env))
                            (eq? v1 #f) (run$ (cdr o0) (cdr o1) (cons (list (cadar o1) o0) env))
                            else
                                (let ((res (run$ (list v0) (list v1) env)))
                                    (if (eq? (car res) #s)
                                        (run$ (cdr o0) (cdr o1) (append (cadr res) env))
                                        (list #u)))))
                else
                    (let ((v0 (assq (cadar o0) env)))
                        (if (eq? v0 #f)
                            (run$ (cdr o0) (cdr o1) (cons (list (cadar o0) (car o1)) env))
                            (let ((res (run$ v0 (car o1) env)))
                                (if (car res)
                                    (run$ (cdr o0) (cdr o1) (append (cadr res) env))
                                    (list #u))))))
        (compound-type? (car o0))
            (if (compound-checks-out? (car o1) (car o0))
                (run$ (cdr o0) (cdr o1) env)
                (list #u))
        (type? (car o0))
            (if (type-checks-out? (car o1) (car o0))
                (run$ (cdr o0) (cdr o1) env)
                (list #u))
        else (list #u)))

(define (run* o0 o1 env)
    " run* is a simple, Kanren-like term language evaluator for typing PreDigamma programs. It can be used in 
      Digamma proper as well. Originally it was a simple Prolog-style unifier that also accepted types, need
      to add more granular types, as well as the ability to type things with:

      - Constraints
      - Products, Sums (Every & Any, respectively)
      - Variable types (parametric types)
      "
    (cond
        (var? o0) 
            (cond
                (var? o1)
                    (let ((v0 (assq (cadr o0) env))
                          (v1 (assq (cadr o1) env)))
                        (cond
                            (eq? v0 #f) (list (cadr o0) o1)
                            (eq? v1 #f) (list (cadr o1) o0)
                            else (run* (cadr v0) (cadr v1) env)))
;;                (type? o1)
;;                    (let ((v0 (assq (cadr o0) env)))
;;                        (cond
;;                            (eq? v0 #f) (list (cadr o1) o0)
;;                            (var? v0) #f
;;                            (pair? v0) (or (and (eq? o1 (cadr v0)) #s) #u)
;;                            else (type-checks-out? v0 o1))) ;; this here should be the actual type check...
                else
                    (let ((v0 (assq (cadr o0) env)))
                        (if (eq? v0 #f)
                            (list (cadr o0) o1)
                            (run* v0 o1 env))))
;;        (var? o1) ;; need to rework this; should check if o0 is a type first...
;;            (let ((v1 (assq (cadr o1) env)))
;;                (if (eq? v1 #f)
;;                    (list (cadr o1) o0)
;;                    (run* o0 v1 env)))
        (compound-type? o0)
            (show (compound-checks-out? o1 o0) "compound-type? ")
        (type? o0)
            (cond
                (type? o1)
                    (if (eq? o0 o1)
                        #s
                        #u)
                (var? o1)
                    (let ((v1 (assq (cadr o1) env)))
                        (cond
                            (eq? v1 #f) (list (cadr o1) o0)
                            (var? v1) #f
                            (type? v1) (eq? v1 o0)
                            else (type-checks-out? (cadr v1) o0)))
                else
                    (show (type-checks-out? o1 o0) "type-checks-out? "))
;;        (type? o1)
;;            (cond
;;                (type? o0)
;;                    (if (eq? o0 o1)
;;                        #t
;;                        #f)
;;                (var? o0)
;;                    (let ((v0 (assq (cadr o0) env)))
;;                        (cond
;;                            (eq? v0 #f) (list (cadr o1) o0)
;;                            (var? v0) #f
;;                            (type? v0) (eq? o1 v1)
;;                            else #f))
;;                else
;;                    (type-checks-out o0 o1))
        (and
            (eq? o0 '())
            (eq? o1 '()))
            #s
        (eq? o0 o1) '()
        (and (pair? o0) (pair? o1))
            (with u-result (run* (car o0) (car o1) env)
                (cond
                    (pair? u-result)
                        (with n-result (run* (cdr o0) (cdr o1) (cons u-result env))
                            (if (eq? n-result #u)
                                #u
                                (append (list u-result) n-result)))
                    (not (eq? u-result #u))
                        (run* (cdr o0) (cdr o1) env)
                    else #u))
        else  #u))

; A simple collexion of useful utilities, that may over lap with init.ss, but
; to be used at my discresion...
;
; Scheme is like a ball of snow. You can add any amount of snow to it and it still looks like a ball of snow.
; Moreover, snow is cleaner than mud.
; -- Marc Feeley

(define-sytax and ()
    ((and) #t)
    ((and e1) e1)
    ((and e1 e2 e3 ...) (if e1 (and e2 e3 ...) #f)))

(define-syntax or ()
    ((or) #f)
    ((or e1) e1)
    ((or e1 e2 e3 ...) (if e1 #t (or e2 e3 ...))))
    

(define (null? n) (eq? n '()))
(define (pair? n) (eq? (type n) "Pair"))
(define (list? n)
    (cond
        (null? n) #t
        (not (pair? n)) #f
        else (list? (cdr n))))
(define (vector? n) (eq? (type n) "Vector"))
(define (dict? n) (eq? (type n) "Dictionary"))
(define (symbol? n) (eq? (type n) "Symbol"))
(define (key? n) (eq? (type n) "Key"))
(define (number? n) (eq? (type n) "Number"))
(define (string? n) (eq? (type n) "String"))
(define (bool? n) (eq? (type n) "Boolean"))
(define (goal? n) (eq? (type n) "Goal"))
(define (char? n) (eq? (type n) "Character"))
(define (not x)
        (cond
                (eq? x #s) #u
                (eq? x #f) #t
                (eq? x #u) #s
                else #f))
(define (zero? n) (= n 0))
(define (eof-object? n) (eq? n #e))
(define (void? x) (eq? x #v))
(define ncons (fn (x) (cons x '())))
(define (unzip ll)
	(def inner-u-r (fn (xl yl sl)
		(if (null? sl)
			(list xl yl)
			(inner-u-r (append xl (ncons (car (car sl)))) (append yl (ncons (car (cdr (car sl))))) (cdr sl)))))
	(inner-u-r '() '() ll))
(def zip (fn (xs ys)
	(if (null? xs)
		'()
		(cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys))))))

(define-syntax with ()
    ((with x y z ...)
     ((lambda (x) z ...) y)))

(define-syntax cond (else)
  ((_ else e1) e1)
  ((_ e1 e2) (if e1 e2 #f))
  ((_ e1 e2 e3 ...) (if e1 e2 (cond e3 ...))))

(define (endswith? src suffix)
    (cond
        (< (length src) (length suffix)) #f
        (= (length src) (length suffix)) (eq? src suffix)
        else (eq? (cslice src (- (length src) (length suffix)) (length src)) suffix)))

(define (beginswith? src prefix)
    (cond
        (< (length src) (length prefix)) #f
        (= (length src) (length prefix)) (eq? src prefix)
        else (eq? (cslice src 0 (length prefix)) prefix)))

(define (match str pat) #f)

(define (split str pat) #f)

(define (subst str pat) #f)

(define (subst! str pat) #f)

(define (map proc col)
	(if (empty? col)
		col
		(ccons (proc (first col)) (map proc (rest col)))))

(define (map* f l)
    " map*: like map, but takes multiple lists. Just found out this morning that
    (define (foo . bar) ...) and (define (baz :rest z) ...) are both broken in
    Vesta, so this uses the workaround that things must be passed in via a list.
    Poor form, I know. :|"
    (if (or (null? l) (null? (car l)))
        '()
        (cons (apply f (map car l)) (map* f (map cdr l)))))

(define (map-if pred proc col)
    " map-if pred : PREDICATE proc : PROCEDURE col : COLLEXION
      maps values of col -> col', by applying proc to x_i, iff
      pred returns true for col. This was inspired by
      http://funcall.blogspot.com/2012/04/20-minute-puzzle.html

      I originally wrote:
      (def *objnames* (map (fn (x) (nth x 'name)) *objs*))
      (filter (fn (x) (not (memq x *names*))) *objnames*)

      Where *names* & *objs* are the list of strings and
      list of objects from the puzzle. I realized that a HOF
      that did both the job of MAP & FILTER in one pass would
      be a neat & useful function to have in prelude, so here
      it is.
      "
	(if (empty? col)
		col
        (if (pred (first col))
		    (ccons (proc (first col)) (map-if pred proc (rest col)))
            (map-if pred proc (rest col)))))

(define (foreach proc col)
       (if (empty? col)
	       #v
	       (begin (proc (first col)) (foreach proc (rest col)))))

(define foreach-proc foreach)

(define (foreach* f l)
    "foreach*: same as map* above, but for foreach"
    (if (or (null? l) (null? (car l)))
        #v
        (begin
            (apply f (map car l))
            (foreach* f (map cdr l)))))

(define (filter proc col)
	(if (empty? col)
		col
		(if (proc (first col))
			(ccons (first col) (filter proc (rest col)))
			(filter proc (rest col)))))
(define (map-apply proc col)
	(if (empty? col)
		col
		(ccons (apply proc (first col)) (map-apply proc (rest col)))))
(define (read-lines p)
	(with r (read-string p)
		(if (eof-object? r)
			'()
			(cons r (read-lines p)))))
(define (odd? x)
	(= (modulo x 2) 1))
(define (even? x)
	(= (modulo x 2) 0))
(define (foldr op s l)
	(if (empty? l)
		s
		(op (first l) (foldr op s (rest l)))))
(define (foldl op s l)
	(if (empty? l)
		s
		(foldl op (op s (first l)) (rest l))))
(def require (let ((paths ["~/.digamma/lib" "."]) (loaded {})) (fn (x) #f)))
(define (tilde-expand f) ; need to flush this out & support ~user as well...
 (if (eq? (nth f 0) #\~)
  (string-append (sys :getenv "HOME") (cslice f 1 (length f)))
  f))
(def *lib-path* ["~/.digamma/lib" "."])
; Incredible inefficient definition of 'use, but works for now
(define (use l)
       (define (subuse l paths)
        (if (empty? paths)
         (error (format "Unable to load library: ~s" l))
         (if (eq? (sys :stat (string-append (tilde-expand (first paths)) "/" l)) #f)
          (subuse l (rest paths))
          (eval (list 'load (string-append (tilde-expand (first paths)) "/" l)) (default-environment)))))
    (if (endswith? l ".ss")
     (subuse l *lib-path*)
     (subuse (string-append l ".ss") *lib-path*)))
(define (char>=? a b) (>= (coerce a 'int) (coerce b 'int)))
(define (char>? a b) (> (coerce a 'int) (coerce b 'int)))
(define (char<=? a b) (<= (coerce a 'int) (coerce b 'int)))
(define (char<? a b) (< (coerce a 'int) (coerce b 'int)))
(define (char-range? c start end)
	(and (>= (coerce c 'int) (coerce start 'int)) (<= (coerce c 'int) (coerce end 'int))))
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

(define (exact->inexact x)
    (if (rational? x) 
     (* x 1.0) ; works because rational? is satisfied for integer?
     x))

(define (string-index str sub (offset 0) (sub-offset 0) (start 0))
    "SRFI-13-style string index; return the location of the sub-string"
    (cond
        (> offset (length str)) -1
        (>= sub-offset (length sub)) start
        (eq? (nth str offset) (nth sub sub-offset))
             (string-index str sub 
                           (+ offset 1)
                           (+ sub-offset 1)
                           (if (= sub-offset 0)
                               offset
                               start))
        else (string-index str sub (+ offset 1) 0 start)))

(define (string-tokenize-char str c (offset 0) (start 0))
     "break a string apart on a single character; dispatched from
      string-tokenize when the partition argument is a character"
     (cond
         (>= offset (length str)) (cons (cslice str start offset) '())
         (eq? (nth str offset) c) (cons (cslice str start offset)
                                        (string-tokenize-char str c (+ offset 1) (+ offset 1)))
         else (string-tokenize-char str c (+ offset 1) start)))

(def (string-join l ij)
   (if (null? (cdr l))
       (car l)
       (string-append (car l) ij (string-join (cdr l) ij))))

(define (append-map f x) (if (null? x) x (append (f (car x)) (append-map f (cdr x)))))
(define (list->vector l) (coerce l 'vector))
(define (vector->list v) (coerce v 'pair))

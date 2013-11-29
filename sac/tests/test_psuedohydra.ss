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
(define (cadddar x) (car (cdr (cdr (cdr (car x))))))

(define (hydra@instruction c)
    (car c))

(define (hydra@operand c)
    (cadr c))

(define (hydra@vm code env ip stack dump)
    (if (>= ip (length code))
        (if (empty? dump)
            (car stack)
            (hydra@vm (caar dump) (cadar dump) (+ (caddar dump) 1) (cons (car stack) (cadddar dump)) (cdr dump)))
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
                                 (cons (/ (cadr stack) (car stack)) (cddr stack)) dump)))))


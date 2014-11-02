;; Type inferencer; counter part to tc.ss
;; Lots of work to be done here, but basic
;; test shouldn't be terrible

(define *env* '(
    ((car (Pair 'a)) => 'a)
    ((cdr (Pair 'a)) => 'a)
    ((nth (Seq 'a) Int (~ 'a)) => 'a)
    ((get (Col 'a) String 'a) => 'a)
    ((+ Number ...) => Number)
    ((- Number ...) => Number)
    ((* Number ...) => Number)
    ((/ Number ...) => Number)
    ((zero? Number) => Bool)
    ((empty? (Union Seq Col)) => Bool)
    ((length (Seq 'a)) => Int)
    ((if Bool 'a 'b) => Bool)
    ((eq? 'a 'b) => Bool)
    ((equal? 'a 'b) => Bool)))

;; NOTE
;; items like `set!` and `fn` produce types
;; but they're not really required for the
;; inferencer... right? How do you type 
;; forms like this? Certainly, we can express
;; the ouput types, and possibly some of the
;; commestibles, but it seems less regular than
;; the other forms we're typing here...
;; Need to masticate on this

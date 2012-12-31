;; µRuntime for Digamma
;; low-level runtime written in PreDigamma, with lots
;; of %primitive accesses

(define-record "<pair>" pair
    (Cons head tail)
    pair?
    (head car)
    (tail cdr))

(define-polymorphic-variant (SExp 'a)
    (Cons 'a 'a)
    (Vector 'a)
    (String 'a)
    (Trie 'a 'a)
    (Hash 'a 'a)
    (Number 'a)
    (Record 'a)
    (Union ('a))
    (otherwise 'a))

(define (tail-pos-cond x)
    (cond
        (< x 10) 'yes
        (= x 10) 'equal
        else 'gt))

(define (non-tail-pos-cond x)
    (cond
        (< x 10) 'yes
        (= x 10) 'equal
        else 'gt)
    x)

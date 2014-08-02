;; simple tests to make sure enyalios is generating Dicts correctly

(define (dict-test-0 x)
    (dict value: x))

(define (dict-test-1 x)
    (let ((z {}))
        (cset! z value: x)
        z))

(define (dict-test-2)
    { this: "dict"
      test: "here"})

;; a file to hold syntax-expand/define-syntax tests

(define st0 '(syntax-expand1 '(() ((with x y z ...) (let ((x y)) z ...))) '(with r (+ 10 33) (display "r == ") (display r) (newline) r)))

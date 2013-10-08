(load '../experiments/sr.ss)
(display (syntax-expand1 '(foo () ((_ x . rest) (goo . rest))) '(foo 1 2 3 4 5)))

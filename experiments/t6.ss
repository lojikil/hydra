#!/usr/bin/env vesta
(load "../enyalios.ss")
(display (compile-let '(((x 3) (y 4) (z 5)) (- x y z) (+ x y z)) '() #f {}))
(newline)

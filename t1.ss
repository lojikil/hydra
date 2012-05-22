(load 'enyalios.ss)
(display (compile-if '((< 3 4) "yes" "no") '() #f))
(newline)

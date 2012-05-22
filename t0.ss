(load 'enyalios.ss)
(define f (open "test1.c" :write))
(il->c 
    '((c-if (c-primitive "flt" (3 4)) "yes") (c-else "no"))
    0
    f)
(close f)

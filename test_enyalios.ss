(load 'enyalios.ss)
(define f (open "test.c" :write))
(il->c '((c-if (c-eq 3 4) (c-return 43)) (c-else (c-return 50))) 0 f)
(close f)

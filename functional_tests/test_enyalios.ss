#!/usr/bin/env vesta
(load "../enyalios.ss")
(define f (open "test.c" :write))
(il->c '((c-if (c-eq 3 4) (c-return 43)) (c-else (c-return 50))) 0 f)
(il->c '((c-if (c-eq 3 4) (c-return 43)) (c-elif (c-eq x 17) (c-return 47)) (c-else (c-return 50))) 0 f)
(il->c '(c-dec foo (x y z) ((c-if (c-eq 3 4) (c-return 43)) (c-elif (c-eq x 17) (c-return 47)) (c-else (c-return 50)))) 0 f)
(close f)
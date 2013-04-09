#!/usr/bin/env vesta

;; A mini-prelude of Enyalios-specific forms that do not exist in
;; standard Digamma. Should codify these & add them to the language's
;; semantics writeup
(define %include list)
(define %prim list)
(define dict-set! cset!)
(define vector-set! cset!)
;; end mini-prelude

(load 'hydra.ss)
;; Vesta has *command-line* defined even if it isn't being run in script
;; mode, so I think Hydra should follow suit
(hydra@main *command-line*)

#!/usr/bin/env vesta
(load 'hydra.ss)
;; Vesta has *command-line* defined even if it isn't being run in script
;; mode, so I think Hydra should follow suit
(hydra@add-env! '*command-line* '() *tlenv*)
(if (> (length *command-line*) 0)
    (begin
        (hydra@set-env! '*command-line* (cslice *command-line* 1 (length *command-line*)) *tlenv*)
        (hydra@load (nth *command-line* 1) *tlenv*))
        (hydra@main))

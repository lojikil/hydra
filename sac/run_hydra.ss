#!/usr/bin/env vesta
(load 'hydra.ss)
;; Vesta has *command-line* defined even if it isn't being run in script
;; mode, so I think Hydra should follow suit
(hydra@main *command-line*)

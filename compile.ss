#!/usr/bin/env vesta
(load 'Eprime.ss)
(load 'enyalios.ss)
(load 't7.ss)

(cond
    (= (length *command-line*) 4)
        (eprime (nth *command-line* 1) (nth *command-line* 2) (nth *command-line* 3))
    (and (= (length *command-line*) 5) (eq? (nth *command-line* 1) "+e"))
        (enyalios (nth *command-line* 2) (nth *command-line* 3) (nth *command-line* 4))
    (and (= (length *command-line*) 5) (eq? (nth *command-line* 1) "-e"))
        (eprime (nth *command-line* 2) (nth *command-line* 3) (nth *command-line* 4))
    else
        (display "usage: compile.ss [(+|-)e] infile outfile init_name\n"))

#!/usr/bin/env vesta
(load 'Eprime.ss)

(if (= (length *command-line*) 4)
    (eprime (nth *command-line* 1) (nth *command-line* 2) (nth *command-line* 3))
    (display "usage: compile.ss infile outfile init_name\n"))

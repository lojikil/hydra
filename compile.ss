#!/usr/bin/env vesta
(load 'Eprime.ss)
(load 'enyalios.ss)
(load 't7.ss)

(if (< (length *command-line*) 1)
    (display "usage: compile.ss infile [(+|-)e] [outfile] [init_name]\n")
    (let* ((source (nth *command-line* 1))
           (flag (nth *command-line* 2 "+e"))
           (output (nth *command-line* 3 (format "~s.c" source)))
           (init (nth *command-line* 4 "enyalios_entry_point")))
        (if (eq? flag "+e")
            (enyalios source output init)
            (eprime source output init))))

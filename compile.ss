#!/usr/bin/env vesta
(load 'Eprime.ss)
(load 'enyalios.ss)

(if (< (length *command-line*) 1)
    (display "usage: compile.ss infile [(+|-)e] [outfile] [init_name]\n")
    (let* ((source (nth *command-line* 1))
           (flag (nth *command-line* 2 "+e"))
           (output (nth *command-line* 3 (format "~s.c" source)))
           (init (nth *command-line* 4 "enyalios_entry_point")))
        (cond
            (eq? flag "+e") (enyalios source output init)
            (eq? flag "+P")
                (begin
                    (set! *profiling* #t)
                    (enyalios source output init))
            else
            (eprime source output init))))

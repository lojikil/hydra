(load 'prelude.ss)
(load 'aneris.ss)
(display (aneris@eval '(car '(1 2 3 4 5)) *tlenv*))
(newline)

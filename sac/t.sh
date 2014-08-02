for i in `seq 0 100`; do
    /usr/bin/time ./typhon fib2.ss >> dump 2>&1
    /usr/bin/time ./typhon-preinc-13SEP2013 fib2.ss >> dump 2>&1
done

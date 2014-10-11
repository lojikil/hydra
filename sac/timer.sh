#!/bin/bash

cnt=0

while ((cnt < 100))
do
    (time ./PREDUMP-typhon fib2.ss) 2>&1 | grep real | tee -a PREDUMP.times
    ((cnt = cnt + 1))
done

cnt=0

while ((cnt < 100))
do
    (time ./typhon fib2.ss) 2>&1 | grep real | tee -a POSTDUMP.times
    ((cnt = cnt + 1))
done

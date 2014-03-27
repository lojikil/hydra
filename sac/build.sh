#!/bin/bash

os=`uname`

if [ "${os[@]:0:6}" = "CYGWIN" ]
then
    os="Cygwin"
fi

case $os in
    DragonFly)
        make CFLAGS="-L /usr/local/lib -I /usr/local/include"
        ;;
    Cygwin)
        make CFLAGS="-DCYGWIN"
        ;;
    *)
        make
        ;;
esac

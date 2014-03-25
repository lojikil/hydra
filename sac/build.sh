os=`uname -o`
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

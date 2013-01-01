#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <time.h>
#include "murt.h"

extern Symbol *enyalios104;

int
main(int ac, char **al)
{
    if(!gc_init())
    {
        printf("could not initialize garbage collector!\n");
        return 1;
    }
    enyalios104 = init_env(0);
    if(!tlenv)
    {
        printf("could not load initial constants!\n");
        return 2;
    }
}

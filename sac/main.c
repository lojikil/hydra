#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <time.h>
#include "murt.h"

extern Symbol *enyalios105;

int
main(int ac, char **al)
{
    SExp *args;
    int idx = 0;
    if(!gc_init())
    {
        printf("could not initialize garbage collector!\n");
        return 1;
    }
    enyalios105 = init_env(0);
    if(!enyalios105)
    {
        printf("could not load initial constants!\n");
        return 2;
    }
    if(ac > 1)
    {
        args = makevector(ac - 1, nil);
        for(idx = 0; idx < (ac - 1); idx++)
            args->object.vec[idx] = makestring(al[idx + 1]);
        typhon_main(args);
    }
    else
        typhon_main(SNIL);
    return 0;
}

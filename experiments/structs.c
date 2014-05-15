#include "murt.h"

/* compile with:
 * cc -o test_simple_struct test_simple_struct.ss.c vesta.c posix.c -lgc
 * this is a hacked up source of test_simple_struct.ss; need to make changes
 * to enyalios to support what I had to do here to get this working.
 */

SExp *foo(SExp *x);
SExp *bar(SExp *klist);
int main();
SExp *make_kons(SExp *kar, SExp *kdr);
SExp *kons_set_kar_(SExp *x, SExp *y);
SExp *kons_set_kdr_(SExp *x, SExp *y);
SExp *kons_kar(SExp *x);
Symbol *enyalios2;

struct kons {
    SExp *kar;
    SExp *kdr;
};

SExp *
make_kons(SExp *kar, SExp *kdr)
{
    SExp *strcttmp12 = (SExp *)hmalloc(sizeof(SExp));
    struct kons *f = (struct kons *)hmalloc(sizeof(struct kons));
    //printf("Made it here? %d\n", __LINE__);
    f->kar = kar;
    //printf("Made it here? %d\n", __LINE__);
    f->kdr = kdr;
    //printf("Made it here? %d\n", __LINE__);
    strcttmp12->object.foreign = (void *)f;
    //printf("Made it here? %d\n", __LINE__);
    return strcttmp12;
}
SExp *
kons_set_kar_(SExp *x, SExp *y)
{
    struct kons *f = NULL;
    //printf("Made it here? %d\n", __LINE__);
    f = (struct kons *)x->object.foreign;
    //printf("Made it here? %d\n", __LINE__);
    f->kar = y;
    //printf("Made it here? %d\n", __LINE__);
    return SVOID;
}
SExp *
kons_set_kdr_(SExp *x, SExp *y)
{
    struct kons *f = NULL;
    //printf("Made it here? %d\n", __LINE__);
    f = (struct kons *)x->object.foreign;
    //printf("Made it here? %d\n", __LINE__);
    f->kdr = y;
    //printf("Made it here? %d\n", __LINE__);
    return SVOID;
}
SExp *
kons_kar(SExp *x)
{
    struct kons *f = NULL;
    //printf("Made it here? %d\n", __LINE__);
    f = (struct kons *)x->object.foreign;
    //printf("Made it here? %d\n", __LINE__);
    return f->kar;
}
SExp *
kons_kdr(SExp *x)
// need to fix getter generation; this was a second set here!
{
    struct kons *f = NULL;
    f = (struct kons *)x->object.foreign;
    //printf("Made it here? %d\n", __LINE__);
    return f->kdr;
}
SExp *
foo(SExp *x)
{
    //printf("Made it here? %d\n", __LINE__);
    if(AINT(x) < 0){
        return SNIL;
    }
    return make_kons(x, foo(fsubt(list(2,x, makeinteger(1)))));
}
SExp *
bar(SExp *klist)
{
    //printf("Made it here? %d\n", __LINE__);
    return f_princ(list(1, kons_kar(klist)), enyalios2);
}
int
main()
{
    SExp *tmp0 = nil;
    if(!gc_init()){
        //printf("gc failed!\n");
        return 1;
    }
    //printf("Made it here? %d\n", __LINE__);
    enyalios2 = init_env(0);
    if(!enyalios2){
        //printf("could not init env!\n");
        return 2;
    }
    //printf("Made it here? %d\n", __LINE__);
    tmp0 = foo(makeinteger(5));
    bar(tmp0);
    printf("\n");
    bar(kons_kdr(tmp0));
    printf("\n");
    bar(kons_kdr(kons_kdr(tmp0)));
    printf("\n");
    //printf("Made it here? %d\n", __LINE__);
    return 0;
}

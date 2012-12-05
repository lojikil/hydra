#include <stdio.h>
#include <gc.h>
#include <string.h>

#define nil NULL
#define nul '\0'

#define hmalloc GC_MALLOC

enum TYPEZ
{
    BOOL,
    CHAR,
    INTEGER,
    STRING
};

typedef struct _SEXP
{
    int type;
    int len;
    union
    {
        char c;
        int i;
        char *str;
    } object;
} SExp;

SExp *makeint(int);
SExp *makechar(char);
SExp *makestring(char *, int);
SExp *makebool(char);


SExp *
makeint(int i)
{
    SExp *ret = (SExp *)hmalloc(sizeof(SExp));
    ret->type = INTEGER;
    ret->object.i = i;
    return ret;
}

SExp *
makechar(char c)
{
    SExp *ret = (SExp *)hmalloc(sizeof(SExp));
    ret->type = CHAR;
    ret->object.c = c;
    return ret;
}

SExp *
makebool(char c)
{
    SExp *ret = (SExp *)hmalloc(sizeof(SExp));
    ret->type = BOOL; 
    ret->object.c = c;
    return ret;
}

SExp *
makestring(char *s, int i)
{
    SExp *ret = (SExp *)hmalloc(sizeof(SExp));
    ret->type = STRING;
    ret->len = i;
    ret->object.str = (char *)hmalloc(sizeof(char) * i);
    strncpy(ret->object.str, s, i);
    return ret;
}

SExp *
flt(SExp *s0, SExp *s1)
{
    SExp *ret = makebool(0);
    if(s0->type != INTEGER || s1->type != INTEGER)
        return ret;
    ret->object.c = (s0->object.i < s1->object.i);
    return ret;
}

int 
main()
{
    SExp *it0 = nil;
    SExp *i = nil, *s = nil, *c = nil, *boo = nil;
    int ii = 0;
    printf("Enter a number: ");
    scanf("%d", &ii);
    i = makeint(ii);
    if((it0 = flt(i, makeint(10))) && (it0->type == BOOL && it0->object.c))
    {
        printf("flt(i, 10)\n");
    }
    else if((it0 = flt(i, makeint(20))) && (it0->type == BOOL && it0->object.c))
    {
        printf("flt(i, 20)\n");
    }
    else
    {
        printf("else\n");
    }
    return 0;
}

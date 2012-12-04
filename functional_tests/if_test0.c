#include <stdio.h>
#include <gc.h>

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
    SExp *ret = (SExp *)malloc(sizeof(SExp));
    ret->type = INTEGER;
    ret->object.i = i;
    return ret;
}

SExp *
makechar(char c)
{
    SExp *ret = (SExp *)malloc(sizeof(SExp));
    ret->type = CHAR;
    ret->object.c = c;
    return ret;
}

SExp *
makebool(char c)
{
    SExp *ret = (SExp *)malloc(sizeof(SExp));
    ret->type = BOOL; 
    ret->object.c = c;
    return ret;
}

SExp *
makestring(char *s, int i)
{
    SExp *ret = (SExp *)malloc(sizeof(SExp));
    ret->type = STRING;
    ret->len = i;
    ret->object.str = (char *)malloc(sizeof(char) * i);
    strncpy(ret->object.str, s, i);
    return ret;
}

int 
main()
{
    SExp *it0 = nil;
    SExp *i = nil, *s = nil, *c = nil, *boo = *nil;

}
}

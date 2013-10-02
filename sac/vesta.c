/* @(#) Reference implementation for Digamma 2009.3
 *
()
  ()
()  ()
Digamma/Vesta 2009.3
 * I think this one fits nicely with the whole simplicity theme ^_^
 * Also, it means Phyla's "brand" will have a distinctive logo, one
 * not necessarily tied to F.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <gc.h>
#include <math.h>
#include <sys/param.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <dirent.h>
#include <signal.h>
#include <errno.h>
#include <stdarg.h>

#include "vesta.h"

const char *typenames[] = {
    "Symbol", "Number","Character","Boolean","Goal", "Vector",
    "Pair","String","Procedure","Closure","Foreign", "Nil",
    "Error","Port","Macro","User","TConc","Primitive",
    "Dictionary","Key","Syntax","End of File","Void",
    "Environment","Unsafe Foreign","Continuation",0
};

const char *toknames[] = {
        "TOK_LPAREN",
        "TOK_RPAREN",
        "TOK_LSQUAR",
        "TOK_RSQUAR",
        "TOK_LCURLY",
        "TOK_RCURLY",
        "TOK_INT",
        "TOK_LITSTR",
        "TOK_NQUOTE",
        "TOK_MQUOTE",
        "TOK_UNQUOT",
        "TOK_SPLICE",
        "TOK_SYMBOL",
        "TOK_CHAR",
        "TOK_TRUE",
        "TOK_FALSE",
        "TOK_SUCC",
        "TOK_UNSUCC",
        "TOK_NAME",
        "TOK_REAL",
        "TOK_RATIO",
        "TOK_COMPL",
        "TOK_KEY",
        "TOK_HERROR",
        "TOK_EOF",
        0
};
extern char **environ;
extern int h_errno;
extern int errno;
const char *numtypes[] = {"Integer", "Real", "Rational", "Complex",0};
SExp *snil = nil, *sfalse = nil, *strue = nil, *ssucc = nil, *sunsucc = nil,*mem_err = nil, *pinf = nil, *ninf = nil, *qnan = nil, *snan = nil;
SExp *fake_rpar = nil, *fake_rsqr = nil, *fake_rcur = nil; /* returns for llread, but should never really mean anything */
SExp *seof = nil, *svoid = nil; /* #e, for use with read*, & eof-object?, #v for void */
static int cons_cnt = 0, gensymglobal = 0;
int quit_note = 0;


SExp *
car(SExp *s)
{
	//printf("s->type == %d\n",s->type);
	if(s->type == PAIR)
		return s->object.clist.first;
	if(s->type == NIL)
		return s;
	return makeerror(0,0,"car's sole argument *must* be of type PAIR");
}
SExp *
cdr(SExp *s)
{
	//printf("s->type == %d\n",s->type);
	if(s->type == PAIR)
		return s->object.clist.rest;
	if(s->type == NIL)
		return s;
	return makeerror(0,0,"cdr's sole argument *must* be of type PAIR");
}
SExp *
cons(SExp *s0, SExp *s1)
{
	SExp *ret = nil;
	if((ret = (SExp *)hmalloc(sizeof(SExp))) == nil)
		return mem_err;
	ret->type = PAIR;
	ret->object.clist.first = s0;
	ret->object.clist.rest = s1;
	cons_cnt++;
	return ret;
}
SExp *
snoc(SExp *s0, SExp *s1)
{
	SExp *ret = nil;
	/*printf("%s %s\n",typenames[TYPE(s0)],typenames[TYPE(s1)]);*/
	if(s0->type == PAIR)
	{
		ret = s0;
		while(ret->object.clist.rest != snil)
			ret = ret->object.clist.rest;
		ret->object.clist.rest = cons(s1,snil);
		return s0; /* yes, snoc is destructive! */
	}
	else
		return cons(s0,cons(s1,snil));
}
SExp *
tconc(SExp *s0, SExp *s1)
{
	SExp *tmp = nil;
	if(s0->type != TCONC)
		return makeerror(1,0,"tconc: type clash");
	if(mcar(s0)->type != NIL)
	{
		tmp = mcdr(s0);
		mcdr(tmp) = cons(s1,snil);
		mcdr(s0) = mcdr(tmp);
	}
	else
	{
		tmp = cons(s1,snil);
		mcar(s0) = tmp;
		mcdr(s0) = tmp;
	}
	return svoid;
}
SExp *
tconc_splice(SExp *s0, SExp *s1)
{
	SExp *tmp = nil, *holder = nil;
	if(s0->type != TCONC)
		return makeerror(1,0,"tconc: type clash");
	/*printf("\n-------------------\ntconc_splice\ns0: ");
	princ(s0);
	printf("\ns1: ");
	princ(s1);*/
	if(mcar(s0)->type != NIL)
	{
		//printf("\n\t\tPath 0");
		tmp = mcdr(s0);
		if(s1->type == PAIR)
		{
			//printf("\n\t\t\tPath 0.0");
			holder = tconcify(s1);
			//printf("\n\t\t\tholder == ");
			//princ(holder);
			mcdr(tmp) = mcar(holder);
			mcdr(s0) = mcdr(holder);
		}
		else if(s1->type == TCONC)
		{
			/*printf("\n\t\t\tPath 0.1");
			printf("\n\t\t\ts0 == ");
			princ(s0);
			printf("\n\t\t\ts1 == ");
			princ(s1);
			printf("\n\t\t\ttmp == ");
			princ(tmp);*/
			/* ok, this is fscking up, because we're not actually adding pairs
			 * to the tconcs properly; we need to fix 0.0, to make the end of the
			 * tconc point to the end of the pair that's being spliced in
			 */
			mcdr(tmp) = mcar(s1);
			mcdr(s0) = mcdr(s1);
		}
		else
		{
			//printf("\n\t\t\tPath 0.2");
			mcdr(tmp) = cons(s1,snil);
			mcdr(s0) = mcdr(tmp);
		}
	}
	else
	{
		//printf("\n\t\tPath 1");
		if(s1->type == PAIR)
		{
			tmp = tconcify(s1);
			mcar(s0) = tmp;
			mcdr(s0) = tmp;
		}
		else if(s1->type == TCONC)
		{
			mcar(s0) = mcar(s1);
			mcdr(s0) = mcdr(s1);
		}
		else
		{	
			tmp = cons(s1,snil);
			mcar(s0) = tmp;
			mcdr(s0) = tmp;
		}
	}
	//printf("\n-------------------\n");
	return svoid;
}
/* Make a tconc-structure from a PAIR s */
SExp *
tconcify(SExp *s)
{
	SExp *ret = snil, *tmp = s;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = TCONC;
	if(s->type == PAIR)
	{
		while(1)
		{
			if(mcdr(tmp) == snil)
				break;
			tmp = cdr(tmp);
		}
		ret->object.clist.first = s;
		ret->object.clist.rest = tmp;
	}
	else
	{
		if(s == snil)
		{
			mcar(ret) = snil;
			mcdr(ret) = snil;
		}
		else
		{
			tmp = cons(s,snil);
			mcar(ret) = tmp;
			mcdr(ret) = tmp;
		}
	}
	return ret;
}
SExp *
bappend(SExp *s0, SExp *s1)
{
	SExp *ret = snil, *tmp0 = snil, *tmp1 = snil, *tmp2 = snil;
	if(s1 == snil)
		return s0;
	if(s0 == snil)
		return s1;
	if(s0->type != PAIR)
		return makeerror(2,0,"bappend error");
	ret = cons(snil,snil);
	tmp0 = ret;
	tmp1 = s0;
	while(tmp1 != snil)
	{
		tmp2 = car(tmp1);
		mcar(tmp0) = tmp2;
		tmp1 = cdr(tmp1);
		if(tmp1 == snil)
			break;
		mcdr(tmp0) = cons(snil,snil);
		tmp0 = mcdr(tmp0);
	}
	mcdr(tmp0) = s1;
	return ret;
}
SExp *
append(SExp *rst)
{
	/*SExp *ret = snil, *holder = nil, *tmp = snil;
	holder = s;
	if(holder->type != PAIR)
		return makeerror(1,0,"Type clash: append plist => list");
	while(holder != snil)
	{
		tmp = car(holder);
		if(tmp->type != PAIR)
			return makeerror(1,0,"Type clash: append plist => list");
		while(tmp != snil)
		{
			ret = snoc(ret,car(tmp));
			tmp = cdr(tmp);
		}
		holder = cdr(holder);
	}
	return ret;*/
	int itmp = 0;
	SExp *tmp0 = snil, *tmp1 = snil, *tmp2 = snil;
	itmp = pairlength(rst);
    /*printf("in append; rst == ");
    princ(rst);
    printf("\n");*/
	switch(itmp)
	{
		case 0:
			return snil;
		case 1:
			return car(rst);
		default:
			tmp1 = cons(snil,snil);
			tmp2 = tmp1;
			while(rst != snil)
			{
				if(mcdr(rst) == snil)
					break;
				tmp0 = car(rst);
				if(tmp0 == snil)
				{
					rst = cdr(rst);
					continue;
				}
				if(tmp0->type != PAIR)
					return makeerror(1,0,"append (l*: PAIR) (e : S-EXPRESSION) => PAIR");
				while(tmp0 != snil)
				{
					if(mcar(tmp1) != snil)
					{
						mcdr(tmp1) = cons(snil,snil);
						tmp1 = mcdr(tmp1);
					}
					mcar(tmp1) = mcar(tmp0);
					tmp0 = mcdr(tmp0);
				}
				rst = cdr(rst);
			}
			mcdr(tmp1) = mcar(rst);
			return tmp2;
	}
}
int
pairlength(SExp *s)
{
	int i = 0;
	SExp *holder = s;
	if(s == nil || s->type != PAIR)
		return 0;
	while(holder != snil)
	{
		i++;
		if(holder->type != PAIR) // improper list
			break;
		holder = cdr(holder);
	}
	return i;
}
SExp *
eqp(SExp *s0, SExp *s1)
{
	SExp *tmp0 = s0, *tmp1 = s1;
	if(tmp0->type != tmp1->type)
		return sfalse;
	switch(tmp0->type)
	{
		case NUMBER:
			if(tmp0->object.n->type != tmp1->object.n->type)
				return sfalse;
			switch(tmp0->object.n->type)
			{
				case INTEGER:
					if(tmp1->object.n->nobject.z == tmp0->object.n->nobject.z)
						return strue;
					return sfalse;
				case REAL:
					if(tmp1->object.n->nobject.real == tmp0->object.n->nobject.real)
						return strue;
					return sfalse;
				case RATIONAL:
					if(tmp1->object.n->nobject.rational.num != tmp0->object.n->nobject.rational.num)
						return sfalse;
					if(tmp1->object.n->nobject.rational.den != tmp0->object.n->nobject.rational.den)
						return sfalse;
					return strue;
				case COMPLEX:
					if(tmp1->object.n->nobject.complex.r != tmp0->object.n->nobject.complex.r)
						return sfalse;
					if(tmp1->object.n->nobject.complex.i != tmp0->object.n->nobject.complex.i)
						return sfalse;
					return strue;
			}
			break;
        case KEY: 
        case ATOM:
            if(!strcasecmp(tmp0->object.str,tmp1->object.str))
                return strue;
            return sfalse;
        case STRING:
            if(tmp0->length == tmp1->length && !strncmp(tmp0->object.str,tmp1->object.str, tmp1->length))
                return strue;
            return sfalse;
		case CHAR:
			if(tmp0->object.c == tmp1->object.c)
				return strue;
			return sfalse;
		case NIL: /* since we know both type(tmp0) and type(tmp1) == NIL, there is no other test required */
			return strue; 
		default:
			if(tmp0 == tmp1)
				return strue;
			return sfalse;
	}
	return sfalse;
}

SExp *
eqp_atom(SExp *item, char *atom)
{
    if(item->type == ATOM && !strcasecmp(item->object.str, atom))
        return strue;
    return sfalse;
}

SExp *
assq(SExp *item, SExp *alist)
{
    SExp *tmp0 = snil, *tmp1 = snil;
    if(alist == nil || (alist->type != PAIR && alist->type != NIL))
        return makeerror(1,0,"assq item : EQABLE-SEXPRESSION alist : ASSOC-LIST => sexpression");
    tmp0 = alist;
    while(tmp0 != snil)
    {
        tmp1 = car(tmp0);
        if(tmp1->type != PAIR)
            return makeerror(1,0,"ASSOC-LIST : (EQABLE-SEXPRESSION SEXPRESSION)*");
        if(eqp(car(tmp1),item) == strue)
            return tmp1;
        tmp0 = cdr(tmp0);
    }
    return sfalse;	
}
SExp *
memq(SExp *item, SExp *mlist)
{
    SExp *tmp0 = snil, *tmp1 = snil;
    if(mlist == nil || (mlist->type != PAIR && mlist->type != NIL))
        return makeerror(1,0,"memq item : EQABLE-SEXPRESSION member-list : PAIR => (PAIR | FALSE)");
    tmp0 = mlist;
    while(tmp0 != snil)
    {
        tmp1 = car(tmp0);
        if(eqp(tmp1,item) == strue)
            return tmp0;
        tmp0 = cdr(tmp0);
    }
    return sfalse;
}
SExp *
list(int n, ...)
{
	va_list ap;
	SExp *ret = tconcify(snil);
	va_start(ap,n);
	for(;n;n--)
		tconc(ret,va_arg(ap,SExp *));
	va_end(ap);
	return mcar(ret);
}
SExp *
list_copy(SExp *l, int tconcp, int less)
/* list_copy: copies the spine of a list.
 * params:
 *  l: the list to copy
 *  tconcp: 1 to return a tconc, 0 to return a list
 *  less: copy from 0 -> len(l) - less
 */
{
    SExp *ret = tconcify(snil);
    int i = 0, len = 0;
    if(l->type != PAIR)
        return makeerror(0,0,"list-copy's sole arg *must* be a pair");
    for(len = pairlength(l) - less; i < len; i++)
    {
        tconc(ret,car(l));
        l = cdr(l);
    }
    if(tconcp)
        return ret;
    return mcar(ret);
}
     
SExp *
vector(int n, ...)
{
	va_list ap;
	int i = 0;
	SExp *ret = makevector(n,nil);
	va_start(ap,n);
	for(;i < n;i++)
		ret->object.vec[i] = va_arg(ap,SExp *);
	va_end(ap);
	return ret;
}

/*void
register_procedure(SExp *(*fn)(SExp *,Symbol *),char *name,int arity, Symbol *e)
{
	SExp *t = nil;
	t = (SExp *)hmalloc(sizeof(SExp));
	t->type = PROCEDURE;
	//t->object.procedure = fn;
	t->object.procedure = (void *)fn;
	add_env(e,name,t);
}*/
int
gc_init()
{
	GC_INIT();
	return 1;
}
int
_igcd(int a, int b)
{
	int t = 0;
	if(a == 0)
		return b;
	while(b != 0)
	{
		t = b;
		b = a % b;
		a = t;
	}
	return a;
}
Symbol *
init_env(int full_env)
{
	/*static SExp *snil = nil, *sfalse = nil, *strue = nil, *ssucc = nil, *sunsucc = nil, *mem_err = nil; */
	Symbol *tl_env = nil;
	snil = (SExp *)hmalloc(sizeof(SExp));
	if(snil == nil)
	{
		printf("[-] snil == nil...\n");
		return nil;
	}
	//LINE_DEBUG;
	snil->type = NIL;
	sfalse = (SExp *)hmalloc(sizeof(SExp));
	sfalse->type = BOOL;
	sfalse->object.c = 0;
	strue = (SExp *)hmalloc(sizeof(SExp));
	strue->type = BOOL;
	strue->object.c = 1;
	ssucc = (SExp *)hmalloc(sizeof(SExp));
	ssucc->type = GOAL;
	ssucc->object.c = 1;
	sunsucc = (SExp *)hmalloc(sizeof(SExp));
	sunsucc->type = GOAL;
	sunsucc->object.c = 0;
	mem_err = snil;
	pinf = (SExp *)hmalloc(sizeof(SExp));
	pinf->type = NUMBER;
	pinf->object.n = (Number *)hmalloc(sizeof(Number));
	pinf->object.n->type = REAL;
	pinf->object.n->nobject.real = (double)0x7f800000; /* positive infinity */
	ninf = (SExp *)hmalloc(sizeof(SExp));
	ninf->type = NUMBER;
	ninf->object.n = (Number *)hmalloc(sizeof(Number));
	ninf->object.n->type = REAL;
	ninf->object.n->nobject.real = (double)0xff800000; /* negative infinity */
	qnan = (SExp *)hmalloc(sizeof(SExp));
	qnan->type = NUMBER;
	qnan->object.n = (Number *)hmalloc(sizeof(Number));
	qnan->object.n->type = REAL;
	qnan->object.n->nobject.real = (double)0x7fffffff; /* Quiet NaN */
	snan = (SExp *)hmalloc(sizeof(SExp));
	snan->type = NUMBER;
	snan->object.n = (Number *)hmalloc(sizeof(Number));
	snan->object.n->type = REAL;
	snan->object.n->nobject.real = (double)0x7fbfffff; /* Signalling NaN */
	fake_rpar = (SExp *)hmalloc(sizeof(SExp));
	fake_rpar->type = NIL;
	fake_rsqr = (SExp *)hmalloc(sizeof(SExp));
	fake_rsqr->type = NIL;
	fake_rcur = (SExp *)hmalloc(sizeof(SExp));
	fake_rcur->type = NIL;
	seof = (SExp *)hmalloc(sizeof(SExp));
	seof->type = SEOF;
	svoid = (SExp *)hmalloc(sizeof(SExp));
	svoid->type = SVOID;
	tl_env = (Symbol *)hmalloc(sizeof(Symbol));

	tl_env->snil = snil;
	tl_env->svoid = svoid;
	tl_env->seof = seof;
	tl_env->strue = strue;
	tl_env->sfalse = sfalse;
	tl_env->ssucc = ssucc;
	tl_env->sunsucc = sunsucc;
    tl_env->guards = tl_env->snil;

    // The below are commented out, because it seems to
    // kill Boehm
    tl_env->fake_rpar = fake_rpar;
    tl_env->fake_rsqr = fake_rsqr;
    tl_env->fake_rcur = fake_rcur;
    tl_env->qnan = qnan;
    tl_env->snan = snan; 

	/* seed the random system*/
	srandom(time(nil));

    if(!full_env)
    {
	    tl_env->data = nil;
	    tl_env->cur_offset = 0;
	    tl_env->cur_size = 0;
        return tl_env;
    }
    
	tl_env->data = (Window *)hmalloc(sizeof(Window)); // 64 initial "windows"
	tl_env->cur_offset = 0;
	tl_env->cur_size = 64;
	tl_env->data->env = (Trie *)hmalloc(sizeof(Trie)); // initial "window"
	tl_env->data->next = nil;

	return tl_env;
}
void
clean_env()
{
	int i = 0; 
	/*free(heap);
	free(free_list);*/
	while(0) {
		i++;
	}
}
SExp *
makechar(char c)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = CHAR;
	ret->object.c = c;
	ret->metadata = nil;
	return ret;
}
SExp *
makeport(FILE *fp,char *src, int proto, int bind, char *mode)
{
	SExp *ret = snil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = PORT;
	ret->metadata = nil;
	ret->object.p = (Port *)hmalloc(sizeof(Port));
	PTYPE(ret) = PFILE;
	FILEPORT(ret) = fp;
	FILEADDRESS(ret) = hstrdup(src);
	PROTONUMBER(ret) = proto;
	NETBIND(ret) = bind;
	strncpy(FILEMODE(ret),mode,3);
	return ret;
}
SExp *
makenumber(int t)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = NUMBER;
	ret->metadata = nil;
	ret->object.n = (Number *)hmalloc(sizeof(Number));
	ret->object.n->type = t;
	if(t == INTEGER)
		set_int(ret,0);
	return ret;
}
SExp *
makeinteger(int s)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = NUMBER;
	ret->metadata = nil;
	ret->object.n = (Number *)hmalloc(sizeof(Number));
	ret->object.n->type = INTEGER;
	ret->object.n->nobject.z = s;
	return ret;
}
SExp *
makereal(double f)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = NUMBER;
	ret->metadata = nil;
	ret->object.n = (Number *)hmalloc(sizeof(Number));
	ret->object.n->type = REAL;
	ret->object.n->nobject.real = f;
	return ret;
}
SExp *
makerational(int n,int d)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = NUMBER;
	ret->metadata = nil;
	ret->object.n = (Number *)hmalloc(sizeof(Number));
	ret->object.n->type = RATIONAL;
	NUM(ret) = n;
	DEN(ret) = d;
	return ret;
}
SExp *
makecomplex(double r,double i)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = NUMBER;
	ret->metadata = nil;
	ret->object.n = (Number *)hmalloc(sizeof(Number));
	ret->object.n->type = COMPLEX;
	CEREAL(ret) = r;
	IMMAG(ret) = i;
	return ret;
}
SExp *
makeatom(char *s)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	if(ret == nil)
	{
		printf("hmalloc returned nil! PANIC!\n");
		quit_note = 1;
		return nil;
	}
	ret->type = ATOM;
	ret->metadata = nil;
	set_str(ret,s);
	return ret;
}
SExp *
makekey(char *s)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	if(ret == nil)
	{
		printf("hmalloc returned nil! PANIC!\n");
		quit_note = 1;
		return nil;
	}
	ret->type = KEY;
	ret->metadata = nil;
	set_str(ret,s);
	return ret;
}
SExp *
makestring_v(int size, char fill)
{
	SExp *ret = nil;
	int iter = 0;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = STRING;
	ret->metadata = nil;
	ret->object.str = (char *)hmalloc(sizeof(char) * size + 1);
	if(fill != nul)
	{
		for(;iter < size;iter++)
			ret->object.str[iter] = fill;
		ret->object.str[iter] = nul;
	}
	ret->length = size;
	return ret;
}
SExp *
makestring(const char *s)
{
	SExp *ret = nil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = STRING;
	ret->metadata = nil;
	set_str(ret,s);
	ret->length = strlen(s);
	return ret;
}
SExp *
makeerror(int t, int s, char *f)
{
	SExp *ret = snil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = ERROR;
	ret->metadata = nil;
	ret->object.error.source = (s & 0xFF);
	ret->object.error.level = t;
	ret->object.error.message = hstrdup(f);
	return ret;	
}
/*SExp *
makeprimitive(int n, char *s, int f)
{
	SExp *ret = snil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = PRIM;
	ret->metadata = nil;
	ret->object.primitive.evalp = (char) f & 0xFF;
	ret->object.primitive.name = hstrdup(s);
	ret->object.primitive.num = n;
	return ret;
}*/
SExp *
makevector(int n, SExp *fill)
{
	SExp *ret = snil;
	int iter = 0;
	ret = (SExp *)hmalloc(sizeof(SExp));
	if(n > 0)
	{
		ret->object.vec = (SExp **)hmalloc(sizeof(SExp *) * n);
		if(fill != nil) /* for efficiency; don't fill a vector not used by the user */
			for(;iter < n;iter++)
				ret->object.vec[iter] = fill;
	}
	ret->length = n;
	ret->type = VECTOR;
	ret->metadata = nil;
	return ret;
}
SExp *
makedict()
{
	SExp *ret = snil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = DICT;
	ret->metadata = nil;
	ret->object.dict = (Trie *)hmalloc(sizeof(Trie));
	ret->object.dict->key = nul;
	ret->object.dict->n_len = 0;
	ret->object.dict->n_cur = 0;
	ret->object.dict->nodes = nil;
	return ret;
}
/*SExp *
makeenv(Symbol *e)
{
	SExp *ret = snil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = ENVIRONMENT;
	ret->metadata = nil;
    if(e == nil)
    {*/
       /* this should most likely be a nullenv, but it's possible that
        * there should be another flag here, so that this can also
        * call init_env...
        * also, maybe we should just cloneenv & re-malloc the Trie, or
        * something similar, because we still want nil &al to be the same values
        */
        /*e = (Symbol *)hmalloc(sizeof(Symbol));
        e->data = (Window *)hmalloc(sizeof(Window));
        e->cur_offset = 0;
        e->cur_size = 64;
        e->data->env = (Trie *)hmalloc(sizeof(Trie));
        e->data->next = nil;
    }
    else
	    ret->object.foreign = (void *) e;
	return ret;
}*/
char *
hstrdup(const char *s)
{
	char *ret = nil;
	int l = 0, i = 0;
	if(s == nil)
		return nil;
	l = strlen(s);
	if((ret = (char *)hmalloc(sizeof(char) * l + 1)) == nil)
		return nil;
	for(;i < l;i++)
		ret[i] = s[i];
	ret[i] = nul;
	return ret;
}
/*SExp *
symlookup(char *str, Symbol *env)
{
	int idx = 0,len = 0;
	SExp *d = nil;
	Window *cur = nil;
	Trie *hd = nil;
	//printf("%s:%d\n",__FUNCTION__,__LINE__);
	if(env == nil || env->data->env == nil)
		return nil;
	//printf("%s:%d\n",__FUNCTION__,__LINE__);
	cur = env->data;
	while(cur != nil)
	{
		//printf("SYMLOOKUP loop\n");
		d = trie_get(str,cur->env);
		if(d != nil)
			break;
		cur = cur->next;
	}
	//printf("%s:%d\n",__FUNCTION__,__LINE__);
	//if(d != nil)
	//{
	//	printf("d == ");
	//	princ(d);
	//	printf("\n");
	//}
	//else
	//	printf("d == nil\n");
	return d;
}
int
new_window(Symbol *inp)
{
	Window *w = nil;
	w = (Window *)hmalloc(sizeof(Window));
	w->env = (Trie *)hmalloc(sizeof(Trie));
	w->next = inp->data;
	inp->data = w;
	return 1;
}
int
close_window(Symbol *inp)
{
	if(inp != nil && inp->data != nil)
	{
		inp->data = inp->data->next;
		return 1;
	}
	return 0;
}*/
Symbol *
shallow_clone_env(Symbol *src)
{
	Symbol *ret = nil;
	int i = 0;
	if(src == nil)
		return nil;
	ret = (Symbol *)hmalloc(sizeof(Symbol));
	ret->cur_offset = src->cur_offset;
	ret->cur_size = src->cur_size;
	ret->data = src->data;
	ret->snil = src->snil;
	ret->svoid = src->svoid;
	ret->strue = src->strue;
	ret->sfalse = src->sfalse;
	ret->ssucc = src->ssucc;
	ret->sunsucc = src->sunsucc;
	ret->seof = src->seof;
	ret->tick = src->tick;
    ret->qnan = src->qnan;
    ret->snan = src->snan;
    ret->guards = src->guards;
	return ret;	
}
/*Symbol *
add_env(Symbol *inp, char *name, SExp *data)
{
	Trie *hd = nil;
	SExp *d = nil;
	if(inp != nil && inp->data != nil)
	{
		hd = inp->data->env;
		trie_put(name,data,hd);
		return inp;
	}
	return nil;
}*/
SExp *
trie_put(char *str, SExp *value, Trie *head)
{
	Trie *tmp = nil;
	int iter = 0;
	if(strlen(str) == 0)
	{
		head->data = value;
		return strue;
	}
	if(head->nodes == nil)
	{
		head->nodes = (Trie **)hmalloc(sizeof(Trie *) * 4); /* allocate 4, use one */
		head->n_cur = 1;
		head->n_len = 4;
		head->nodes[0] = trie_alloc(str,value);
		return strue;
	}
	for(iter = 0;iter < head->n_cur;iter++)
	{
		if(head->nodes[iter]->key == str[0])
		{
			trie_put(&str[1],value,head->nodes[iter]);
			return strue;
		}
	}
	/* ok, so the child list is not empty, and we didn't find a matching key, so
	** we have to check now if we have room left in nodes or if it needs to be re-
	** alloc'd, and talloc the remainder of the key...
	*/
	iter = head->n_cur;
	if(iter < head->n_len)
	{
		head->nodes[iter] = trie_alloc(str,value);
		head->n_cur++;
	}
	else
	{
		head->nodes = hrealloc(head->nodes,sizeof(Trie **) * (head->n_len + 4));
		head->nodes[iter] = trie_alloc(str,value);
		head->n_cur++;
		head->n_len += 4;
	}
	return strue;
}
/*trie_put(char *str, SExp *value, Trie *head)
{
	Trie *tmp = head, *i_tmp = nil;
	int iter = 0, str_offset = 0, str_len = 0;
    str_len = strlen(str);
    while(1)
    {
        i_tmp = tmp;
	    if(str_offset >= str_len)
	    {
		    tmp->data = value;
		    return strue;
	    }
	    if(tmp->nodes == nil)
	    {
		    tmp->nodes = (Trie **)hmalloc(sizeof(Trie *) * 4); // allocate 4, use one 
		    tmp->n_cur = 1;
		    tmp->n_len = 4;
		    tmp->nodes[0] = trie_alloc(&str[str_offset], value);
		    return strue;
	    }
	    for(iter = 0;iter < tmp->n_cur;iter++)
	    {
		    if(tmp->nodes[iter]->key == str[str_offset])
		    {
                str_offset++;
                i_tmp = tmp->nodes[iter];
                break;
		    }
	    }
        if(i_tmp != tmp)
        {
            tmp = i_tmp;
            continue;
        }
	    // ok, so the child list is not empty, and we didn't find a matching key, so
	    // we have to check now if we have room left in nodes or if it needs to be re-
	    // alloc'd, and talloc the remainder of the key...
	    //
	    iter = tmp->n_cur;
	    if(iter < tmp->n_len)
	    {
		    tmp->nodes[iter] = trie_alloc(&str[str_offset], value);
		    tmp->n_cur++;
	    }
	    else
	    {
		    tmp->nodes = hrealloc(tmp->nodes,sizeof(Trie **) * (tmp->n_len + 4));
		    tmp->nodes[iter] = trie_alloc(&str[str_offset], value);
		    tmp->n_cur++;
		    tmp->n_len += 4;
	    }
	    return strue;
    }
}*/
SExp *
trie_get(char *key, Trie *head)
{
	int iter = 0;
	if(strlen(key) == 0)
		return head->data;
	if(head->nodes == nil)
		return nil;
	for(;iter < head->n_cur;iter++)
		if(head->nodes[iter]->key == key[0])
			return 	trie_get(&key[1],head->nodes[iter]);
	return nil;
}
SExp *
trie_keys(Trie *hd, SExp *thus_far) // pass in a tconc object that can be cloned when you want to return...
{
	int iter = 0;
	SExp *ret = tconcify(snil), *tmp0 = nil, *tmp1 = nil;
	//printf("Made it to trie_keys\n");
	if(hd == nil)
		return snil;
	//printf("hd == nil: %d\n",hd == nil);
	if(hd->data != nil)
	{
		//printf("\tMade it to hd->data != nil!\n");
		tmp0 = fstring(mcar(thus_far));
		if(tmp0->type == ERROR)
			return tmp0;
		tconc(thus_far,makechar(hd->key));
		tconc(ret,fstring(mcar(thus_far)));
	}
	//printf("\tmade it to 0\n");
	if(hd->nodes != nil)
	{
		//printf("\tmade it to 1\n");
		for(;iter < hd->n_cur; iter++)
		{
			// capture the current state of thus_far in tmp
			// since tconc is destructive...
			tmp1 = mcar(thus_far);
			tmp0 = tconcify(snil);
			/*printf("\tMade it to 2\nthus_far(%s) == ",typenames[thus_far->type]);
			princ(thus_far);
			printf("\n");*/
			while(tmp1 != snil)
			{
				/*printf("tmp1 == nil? %d\n",tmp1 == nil);
				printf("\t\tLooping near 3\ntmp1 == ");
				princ(tmp1);*/
				tconc(tmp0,car(tmp1));
				//printf("\n\t\tLooping near 4\n");
				tmp1 = cdr(tmp1);
			}
			/*printf("thus_far == ");
			princ(thus_far);
			printf("\nret == ");
			princ(ret);
			printf("\n");*/
			if(hd->key != 0 && hd->data == nil)
			{
				tconc(tmp0,makechar(hd->key));
				//ret = cons(trie_keys(hd->nodes[iter],tmp0),ret);
				tmp1 = trie_keys(hd->nodes[iter],tmp0);
				/*while(tmp1 != snil)
				{
					if(mcar(tmp1)->type == ATOM)
						break;
					tmp1 = cdr(tmp1);
				}*/
				/*printf("ret before tconc_splice: ");
				princ(ret);
				printf("\nret after tconc_splice: ");*/
				tconc_splice(ret,tmp1);
				/*princ(ret);
				printf("\n");*/
			}
			else
			{
				tconc_splice(ret,trie_keys(hd->nodes[iter],tmp0));
			}
		}
	}
	/*printf("Ret == ");
	princ(ret);
	printf("\n");*/
	return ret;
}
SExp *
trie_values(Trie *head)
{
	return sfalse;
}
SExp *
trie_pairs(Trie *head)
{
	return sfalse;
}
Trie *
trie_alloc(char *src, SExp *value)
{
	Trie **ret = nil;
	int iter = 0, len = strlen(src);
	if(src == nil || len == 0)
		return nil;
	ret = (Trie **)hmalloc(sizeof(Trie *) * len);
	for(;iter < len;iter++)
	{
		ret[iter] = (Trie *)hmalloc(sizeof(Trie));
		ret[iter]->n_len = 1;
		ret[iter]->n_cur = 1;
		ret[iter]->key = src[iter];
		ret[iter]->data = nil;
		ret[iter]->nodes = (Trie **)hmalloc(sizeof(Trie *));
	}
	ret[iter - 1]->data = value;
	for(iter = 0;iter < len - 1;iter++)
		ret[iter]->nodes[0] = ret[iter + 1];
	ret[iter]->nodes = nil;
	return ret[0];
}
void
trie_walk(Trie *head, int level)
{
	int iter = 0;
	Trie *child;
	for(;iter < level; iter++)
		printf("   ");
	printf("%c",head->key);
	if(head->data != nil)
	{
		printf(": ");
		princ(head->data);
	}
	printf("\n");
	if(head->nodes != nil)
	{
		printf("head->n_cur == %d\n",head->n_cur);
		for(iter = 0;iter < head->n_cur;iter++)
			trie_walk(head->nodes[iter],level + 1);
	}
}
SExp *
trie_partial(Trie *head, char *key, int len)
{
	int i = 0;
	if(len == 0 && head->data != nil)
		return strue;
	if(head->nodes != nil)
	{
		for(;i < head->n_cur;i++)
			if(head->nodes[i]->key == key[0])
				return trie_partial(head->nodes[i],&key[1],len + 1);
		if(len > 0)
			return makeinteger(len);
		return sfalse;
	}
	if(len > 0)
		return makeinteger(len);
	return sfalse;
}
SExp *
trie_hasp(Trie *head, char *key)
{
	SExp *t = nil;
	if(head == nil || key == nil)
		return sfalse;
	t = trie_get(key,head);
	if(t == nil)
		return sfalse;
	return strue;
}

AVLNode *
makeavlnode(int val)
/* given an integer, create a node
 * with that integer as its key member,
 * and correctly set left,right to nil
 */
{
    AVLNode *ret = hmalloc(sizeof(AVLNode));
    ret->key = val;
    ret->data = nil;
    ret->left = nil;
    ret->right = nil;
    ret->parent = nil;
    return ret;
}

int
avl_containsp(AVLNode *tree, int value)
/* avl_containsp for value in a given tree, returning
 * 1 if it is found and 0 otherwise. If mode
 * is true, print both the values & the directions
 * taken as we avl_containsp for value
 */
{
    if(tree->key == value)
        return 1;
    else if(tree->left != nil && value < tree->key)
    {
        return avl_containsp(tree->left, value);
    }
    else if(tree->right != nil && value > tree->key)
    {
        return avl_containsp(tree->right, value);
    }
    return 0;
}

int
avl_insert(AVLNode *tree, int value, SExp *data)
/* insert value in to the tree, 
 * returning 0 if the key is new
 * to the tree and 1 otherwise
 */
{
    if(tree->key == value)
    {
        tree->data = data;
        return 1;
    }
    else if(value < tree->key)
    {
        if(tree->left == nil)
        {
            tree->left = makeavlnode(value);
            tree->left->parent = tree;
            tree->left->data = data;
            return 1;
        }
        return avl_insert(tree->left, value, data);
    }
    else // value > tree -> key
    {
        if(tree->right == nil)
        {
            tree->right = makeavlnode(value);
            tree->right->parent = tree;
            tree->right->data = data;
            return 1;
        }
        return avl_insert(tree->right, value, data);
    }
}

SExp *
avl_get(AVLNode *tree, int key)
{
    AVLNode *tmp = tree;
    while(1)
    {
        if(tmp == nil)
            return nil;
        if(tmp->key == key)
            return tmp->data;
        if(key < tmp->key)
            tmp = tmp->left;
        if(key > tmp->key)
            tmp = tmp->right;
    }
}

int
weight(AVLNode *tree)
/* the weight of a tree is
 * weight(left node) - weight(right node);
 */
{
    int left = 0, right = 0;
    if(tree == nil)
        return 1;
    if(tree->left != nil)
        left = 1 + weight(tree->left);
    if(tree->right != nil)
        right = 1 + weight(tree->right);
    return left + right;
}

AVLNode *
rotate_left(AVLNode *p)
/* left rotate via the following rules:
 * let q := p.right;
 * q is to be the new root
 * p.right := q.left;
 * q.left := p
 */
{
    AVLNode *q = nil, *tmpp = nil;
    if(p == nil || p->right == nil)
        return p;
    tmpp = p->parent;
    q = p->right;
    p->right = q->left;
    q->left = p;
    p->parent = q;
    q->parent = tmpp;
    return q;
}

AVLNode *
rotate_right(AVLNode *q)
/* right rotate via the following rules:
 * let p := q.left
 * P is to be the new root
 * q.left := p.right
 * p.right := q
 */
{
    AVLNode *p = nil, *tmpq = nil;
    if(q == nil || q->left == nil)
        return q;
    tmpq = q->parent;
    p = q->left;
    q->left = p->right;
    p->right = q;
    q->parent = p;
    p->parent = tmpq;
    return p;
}

AVLNode *
balance(AVLNode *tree)
/* balance should walk up tree
 * until it reaches the root node.
 * it should be called from insert 
 * at the point of insertion & walk
 * up the tree from there.
 */
{
    int factorl = weight(tree->left), factorr = weight(tree->right);
    int factor = factorl - factorr;
    if(factor <= -2)
    {
        /*factorl = weight(tree->left), factorr = weight(tree->right);
        factor = factorl - factorr;*/
        if(factorr >= 1)
        {
            tree->right = rotate_right(tree->right);
            tree = rotate_left(tree);
        }
        else if(factorr <= -1)
        {
            tree = rotate_left(tree);
        }
    }
    else if(factor >= 2)
    {
        /*factorl = weight(tree->left), factorr = weight(tree->right);
        factor = factorl - factorr;*/
        if(factorl <= -1)
        {
            tree->left = rotate_left(tree->left);
            tree = rotate_right(tree);
        }
        else if(factorl >= 1)
        {
            tree = rotate_right(tree);
        }
    }
    if(tree->parent != nil)
        return balance(tree->parent);
    return tree;
}

SExp *
newline(SExp *s, Symbol *env)
{
	int itmp = pairlength(s);
	SExp *port = snil;
	//LINE_DEBUG;
	switch(itmp)
	{
		case 0:
			printf("\n");
			break;
		case 1:
			port = car(s);
			if(port->type != PORT)
				return makeerror(2,0,"newline's *p* must be bound to a PORT");
			fprintf(FILEPORT(port),"\n");
			break;
		default:
			return makeerror(2,0,"newline [p : PORT] => NIL");
	}
	//LINE_DEBUG;
	return svoid;
}
SExp *
f_princ(SExp *s, Symbol *env)
{
	int i = pairlength(s);
	SExp *t = env->snil, *f = env->snil;
	if(i < 1 || i > 2)
		return makeerror(2,0,"display expects at least one argument, and no more than two: display o : SEXPRESSION [p : PORT] => SEXRESSION");
	f = car(s);
	if(i == 1)
		llprinc(f,stdout,0); /* should look up what *current-output-port* is set to... */
	else 
	{
		t = car(cdr(s));
		if(t->type != PORT)
			return makeerror(2,0,"display's p argument must be of type PORT");
		llprinc(f,FILEPORT(t),0);
	}
	return f;
}

void
princ(SExp *s)
{
	/* Need to make this accept a Digamma * object, and use the 
	 * current stdout (e->stdout) for llprinc, so that set-current-output &
	 * friends have actual use...
	 */
	llprinc(s,stdout,0);
}
void
llprinc(SExp *s, FILE *fd, int mode)
{
	SExp *fst = snil, *rst = snil, *tmp = snil;
	int iter = 0;
	if(s == nil)
		return ;
	switch(s->type)
	{
		case PAIR:
			fprintf(fd,"(");
			rst = s;
			while(rst != snil)
			{
				if(rst->type != NIL && mcdr(rst)->type != PAIR)
				{
					llprinc(car(rst),fd,mode);
					if(mcdr(rst)->type != NIL)
					{
						fprintf(fd," . ");
						llprinc(cdr(rst),fd,mode);
					}
					break;
				}
				else
				{
					fst = car(rst);
					llprinc(fst,fd,mode);
					rst = cdr(rst);
					if(rst->type != PAIR && rst->type != NIL)
						fprintf(fd," . ");
					else
						fprintf(fd," ");
				}
			}
			fprintf(fd,")");
			break;
		case TCONC:
			llprinc(mcar(s),fd,mode);
			break;
		case PRIM:
			fprintf(fd,"#<\"%s\" %d>", s->object.primitive.name, s->object.primitive.num);
			break;
		case NUMBER:
			switch(s->object.n->type)
			{
				case INTEGER:
					fprintf(fd,"%d", s->object.n->nobject.z);
					break;
				case REAL:
					fprintf(fd,"%.32g", s->object.n->nobject.real);
					break;
				case RATIONAL:
					fprintf(fd,"%d/%d", s->object.n->nobject.rational.num,s->object.n->nobject.rational.den);
					break;
				case COMPLEX:
					if(s->object.n->nobject.complex.r >= 0.0)
						fprintf(fd,"+%f",s->object.n->nobject.complex.r);
					else
						fprintf(fd,"%f",s->object.n->nobject.complex.r);
					if(s->object.n->nobject.complex.i >= 0.0)
						fprintf(fd,"+%fi",s->object.n->nobject.complex.i);
					else
						fprintf(fd,"%fi",s->object.n->nobject.complex.i);
					break;
			}
			break;
		case GOAL:
			if(s == ssucc)
				fprintf(fd,"#s");
			else 
				fprintf(fd,"#u");
			break;
		case BOOL:
			if(s == strue)
				fprintf(fd,"#t");
			else
				fprintf(fd,"#f");
			break;
		case CHAR:
			if(mode)
			{
				switch(s->object.c)
				{
					case ' ':
						fprintf(fd,"#\\space");
						break;
					case '\n':
						fprintf(fd,"#\\linefeed");
						break;
					case '\r':
						fprintf(fd,"#\\carriage");
						break;
					case '\t':
						fprintf(fd,"#\\tab");
						break;
					case '\v':
						fprintf(fd,"#\\vtab");
						break;
					case 0x07:
						fprintf(fd,"#\\bell");
						break;
					case 0x00:
						fprintf(fd,"#\\nul");
						break;
					default:
						fprintf(fd,"#\\%c",s->object.c);
						break;
				}
			}
			else
				fprintf(fd,"%c",s->object.c);
			break;
		case VECTOR:
			fprintf(fd,"[ ");
			for(;iter < s->length;iter++)
			{
				llprinc(s->object.vec[iter],fd,mode);
				fprintf(fd," ");
			}
			fprintf(fd,"]");
			break;
		case KEY:
			fprintf(fd,":");
		case ATOM:
			fprintf(fd,"%s",s->object.str);
			break;
		case STRING:
			// write mode should escape characters like \t & \n
			if(mode)
			{
				fprintf(fd,"\"");
				for(;iter < s->length;iter++)
				{
					switch(s->object.str[iter])
					{
						case '\"':
							fprintf(fd,"\\\"");
							break;
						case '\\':
							fprintf(fd,"\\\\");
							break;
						case '\n':
							fprintf(fd,"\\n");
							break;
						case '\t':
							fprintf(fd,"\\t");
							break;
						case '\r':
							fprintf(fd,"\\r");
							break;
						case '\v':
							fprintf(fd,"\\v");
							break;
						case '\a':
							fprintf(fd,"\\a");
							break;
						case '\b':
							fprintf(fd,"\\b");
							break;
						case '\0':
							fprintf(fd,"\\0");
							break;	
						default:	
							fprintf(fd,"%c",s->object.str[iter]);
							break;
					}	
				}
				fprintf(fd,"\"");
			}
			else
				fprintf(fd,"%s",s->object.str);
			break;
		case NIL:
			fprintf(fd,"()");
			break;
		case SEOF:
			fprintf(fd,"#e");
			break;
		case SVOID:
			fprintf(fd,"#v");
			break;
		default:
			fprintf(fd,"#<%s>",typenames[s->type]);
			break;
	}
}

int
lex(FILE *fdin, char **r)
{
  int c = 0, iter = 0, tmp = 0, state = 0, substate = 0;
	char *ret = *r;
	c = fgetc(fdin);
	//printf("lex start marker\n");
	while(!feof(fdin))
	{
			while(c == ' ' || c == '\t' || c == '\r' || c == '\n')
				c = fgetc(fdin);
			//printf("[!] top level loop: %d %d\n",state,substate);
			switch(state)
			{
				case 0: /* default state; attempt to figure out where to go... */
					if((c >= '0' && c <= '9') || c == '+' || c == '-' || c == '.')
						state = 1;
					else if(c == '#')
						state = 2;
					else if(c == '"')
						state = 3;
					else if(c == '(')
						return TOK_LPAREN;
					else if(c == ')')
						return TOK_RPAREN;
					else if(c == '{')
						return TOK_LCURLY;
					else if(c == '}')
						return TOK_RCURLY;
					else if(c == '[')
						return TOK_LSQUAR;
					else if(c == ']')
						return TOK_RSQUAR;
					else if(c == '\'')
						return TOK_NQUOTE;
					else if(c == '`')
						return TOK_MQUOTE;
					else if(c == ',')
					{
						tmp = fgetc(fdin);
						if(tmp == '@')
							return TOK_SPLICE;
						ungetc(tmp,fdin);
						return TOK_UNQUOT;
					}
					else if(c == ';') /* better comment removal... */
					{
						while(c != '\n')
						{
							/* It's not necessarily true that a newline will
							 * exist at the end of a file, where a comment is...
							 */
							if(feof(fdin))
								return TOK_LEOF;
							c = fgetc(fdin);
						}
						state = 0;
						break;
					}
					else if(feof(fdin))
						return TOK_LEOF;
					else
					{
						ret[iter] = c;
						iter++;
						state = 4;
					}
					break;
				case 1:
          //printf("\t\t[!] Ok, we made it to case 1...\n");
					if((c >= '0' && c <= '9') || c == '+' || c == '-' || c == '.')
					{
						/* some sort of number? Numbers must start with
						* [0-9] but can contain anynumber of other characters,
						* depending on type:
						* complex -> +3.4+5.6i
						*/
						if(c == '+' || c == '-')
						{
							tmp = fgetc(fdin);
							if(!(tmp >= '0' && tmp <= '9'))
							{
								/* we have a symbol most likely */
								ungetc(tmp,fdin);
								ungetc(c,fdin);
								state = 4;
								break;
							}
							ret[0] = c;
							c = tmp;
							iter = 1;
						}
						/* should make this stateful, so we can check that we're actually sending real numbers...
						 * e.g. +3.5-4.6i should lex fine, but under this scheme so would 3iiiiiiii2.0, which
						 * should be a symbol. Although it adds a bit of complexity, a state machine here would
						 * mean less processing afterwards (plus we can make the return type more granular: instead
						 * of simply returning TOK_NUMBER, we could return TOK_COMPLEX, TOK_INTEGER, &c.
						 * Should look at supporting BIGINTS & BigDecimals too...
						 */
            //printf("\t\t[!] Made it to substate switch...\n");
						substate = 0;
						//if(c == '.')
						//	substate = 2;
						while((c >= '0' && c <= '9') || c == '+' || c == '-' || c == '/' || c == '.' || c == 'i' || c == 'e' || c == 'E')
						{
              //printf("\t\t[!] Looping around like mad in substate (%d)!\n",substate);
							switch(substate)
							{
								case 0: /* default case */
									if((c >= '0' && c <= '9') || c == '+' || c == '-')
										substate = 1;
									else if(c == '.')
									    substate = 2;
									else
									{
										substate = 99;
										state = 4; /* something other than a number... */
									}
									ret[iter] = c;
									iter++;
                  					//printf("\t\t[?] substate == %d, c == %c\n",substate,c);
									break;
								case 1: /* integer */
                 					//printf("\t\t[?] Did we make it to the integer case?\n");
									while(1)
									{
										c = fgetc(fdin);
                    					//printf("\t\t[!] I'm looping around forever in lex\n");
                    					ret[iter] = c;
										iter++;
										if(c == '.')
										{
											substate = 2;
											break;
										}
										else if(c == 'e' || c == 'E')
										  {
										    substate = 5;
										    break;
										  }
										else if(c == '/')
										{
											substate = 3;
											break;
										}
										else if(c == '+' || c == '-')
										{
											substate = 4;
											break;
										}
										else if(!issymdelim(c))
										{
											ungetc(c,fdin);
											iter--;
											break;
										}
										else if(!(c >= '0' && c <= '9'))
										{
											substate = 99;
											state = 4;
											break;
										}
									}
									if(substate != 1)
										break;
									ret[iter] = nul;
									return TOK_INT;
								case 2: /* real */
									/* we should have already had a '.', or an 'e' */
									while(1)
									{
										c = fgetc(fdin);
										ret[iter] = c;
										iter++;
										if(c == '+' || c == '-')
										{
											substate = 4;
											break;
										}
										else if(c == 'e' || c == 'E')
										  {
										    substate = 5;
										    break;
										  }
										else if(!issymdelim(c))
										{
											iter--;
											//printf("ret[iter - 1] == #\\%c\n",ret[iter - 1]);
											if(ret[iter - 1] == '.') /* single #\. input */
											{
												//printf("Made it?\n");
												ret[iter] = nul;
												ungetc(c,fdin);
												return TOK_SYMBOL;
											}
											ungetc(c,fdin);
											break;
										}
										else if(!(c >= '0' && c <= '9'))
										{
											state = 4;
											substate = 99;
											break;
										}
									}
									if(substate != 2)
										break;
									ret[iter] = nul;
									return TOK_REAL;
								case 3: /* rational */
									//printf("\t\t[!] made it to the rational substate...\n");
									while(1)
									{
										c = fgetc(fdin);
										ret[iter] = c;
										iter++;
										//printf("\t\t\t rational substate loop: c == %c\n",c);
										if(!issymdelim(c))
										{
											iter--;
											ungetc(c,fdin);
											break;
										}
										if(!(c >= '0' && c <= '9'))
										{
											state = 4;
											substate = 99;
											break;
										}
									}
									if(substate != 3)
										break;
									ret[iter] = nul;
									return TOK_RATIO;
									break;
								case 4: /* complex */
									/* Make a state machine out of this, so that we *know* we have a
									 * complex, not a symbol who matches the pattern...
									 */
									/* but wait a minute: by the time we get here, we should have seen [+|-][NUMBERS][+|-]
									 * there is no need to keep this as part of the game show...
									 */
									while(1)
									{
										c = fgetc(fdin);
										ret[iter] = c;
										iter++;
										if(!issymdelim(c))
										{
											iter--;
											ungetc(c,fdin);
											break;
										}
										else if(!((c >= '0' && c <= '9') || c == '.' || c == 'i'))
										{
											state = 4;
											substate = 99;
											break;
										}
									}
									if(substate != 4)
										break;
									ret[iter] = nul;
									return TOK_COMPL;
	      							case 5: /* cyantific notation */
								  while(1)
								    {
								      c = fgetc(fdin);
								      ret[iter] = c;
								      iter++;
								      if(!issymdelim(c))
									{
									  iter--;
									  ungetc(c,fdin);
									  break;
									}
								      else if(!((c >= '0' && c <= '9') || c == '-' || c == '+'))
									{
									  state = 4;
									  substate = 99;
									  break;
									}
								    }
								  if(substate != 5)
								    break;
								  ret[iter] = nul;
								  return TOK_REAL;
								default: /* some other state, probably 99 */
									break;
							}
							if(substate > 5)
								break;
							/*ret[iter] = c;
							c = fgetc(fdin);				
							iter++;*/
						}
					}
					break;
			case 2: /* hash object, #t #s #f #u #\character #|named_char */
				tmp = fgetc(fdin);
				/* I wonder if the return results for #t, #f, #s & #u should
				 * be constants. I mean, they are constant values, so perhaps this
				 * should return TOK_TRUE, TOK_FALSE, TOK_SUCC, TOK_UNSUCC...
				 * Looks good, implement it.
				 */
				/* add Scheme vector test:
				 * #(vector) & #N(vector) (for a vector of size N)
				 */
				switch(tmp)
				{
					case 'f':
					case 'F': /* #f */
						return TOK_FALSE;
					case 't':
					case 'T': /* #t */
						return TOK_TRUE;
					case 's':
					case 'S': /* #s */
						return TOK_SUCC;
					case 'u':
					case 'U': /* #u */
						return TOK_UNSUCC;
					case 'e':
					case 'E': /* #e, EOF literal */
						return TOK_LEOF;
					case 'v':
					case 'V':
						return TOK_LVOID;
                    case ';':
                        return TOK_DATCOM;
					case '!':
						while(c != '\n') c = fgetc(fdin);
						state = 0;
						break;
					case '\\': /* character #\c */
						c = fgetc(fdin);
						if(c == ' ' || c == '\n' || c == '\t')
						{
							ret[0] = c;
							ret[1] = nul;
							return TOK_CHAR;
						}
						while(1)
						{
							ret[iter] = c;
							c = fgetc(fdin);
							iter++;
							if(c == '{' || c == '}' || c == ' ' || c == '(' || c == '\n' || c == '\t' || c == ')' || c == '[' || c == ']')
								break;
						}
						ungetc(c,fdin);
						ret[iter] = nul;
						return TOK_CHAR;
					case '|':
					  /* named character #|eof| */
						break;
					case ',':
						/* SRFI-10 */
						break;
					case 'x':
					case 'X':
						/* hex */
						while(1)
						{
							c = fgetc(fdin);
							ret[iter] = c;
							iter++;
							if(!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) 
							{
								iter--;
								ungetc(c,fdin);
								break;
							}
						}
						ret[iter] = nul;
						return TOK_HEX;
					case 'o':
					case 'O':
						/* octal */
						while(1)
						{
							c = fgetc(fdin);
							ret[iter] = c;
							iter++;
							if(!(c >= '0' && c <= '7'))
							{
								iter--;
								ungetc(c,fdin);
								break;
							}
						}
						ret[iter] = nul;
						return TOK_OCT;
					case 'b':
					case 'B':
						/* binary */
						while(1)
						{
							c = fgetc(fdin);
							ret[iter] = c;
							iter++;
							if(c != '0' && c != '1')
							{
								iter--;
								ungetc(c,fdin);
								break;
							}
						}
						ret[iter] = nul;
						return TOK_BIN;
					case EOF:
						return TOK_EOF;
					default: /* syntax error */
						return TOK_HERROR;
				}
				break;
			case 3: /* literal string */
				c = fgetc(fdin);
				while(c != '"')
				{
					if(feof(fdin))
						return TOK_SERROR;
					if(c == '\\') // make \" work :D
					{
						c = fgetc(fdin);
						if(feof(fdin))
							return TOK_SERROR;
						else if(c == 't')
							c = '\t';
						else if(c == 'n')
							c = '\n';
						else if(c == 'r')
							c = '\r';
						else if(c == 'v')
							c = '\v';
						else if(c == '0')
							c = '\0';
						else if(c == 'b')
							c = '\b';
						else if(c == 'a')
							c = '\a';
					}
					ret[iter] = c;
					c = fgetc(fdin);
					iter++;
				}
				ret[iter] = nul;
				return TOK_LITSTR;
			case 4:  /* symbol */
				c = fgetc(fdin);
				while(c != '"' && c != '\'' && c != '(' && c != ')' && c != '[' && c != ']' && c != '{' && c != '}' && c != ' ' && c != '\n' && c != '\t')
				{
					ret[iter] = c;
					c = fgetc(fdin);
					iter++;
				}
				ret[iter] = nul;
				ungetc(c,fdin);
				if(ret[0] == ':' || ret[iter - 1] == ':')
					return TOK_KEY;
				return TOK_SYMBOL;
		}
	}
	if(feof(fdin))
		return TOK_EOF;
}
SExp *
llread(FILE *fdin)
/* I'm wondering if I should split this into two (one for reading from PNETs, one from PFILEs) or modularise
   this. The second option will take quite a bit of time...
   */
{
	int tok = 0, iter = 0, itmp = 0, cur_state = 0, state_stack[64] = {0}, sp = -1;
	SExp *ret = snil, *holder = snil, *tmp0 = snil, *tmp1 = snil, *tmp2 = snil, *read_stack[64] = {snil};
	static char *buf = nil;
	if(buf == nil)
		buf = (char *)hmalloc(sizeof(char) * 2048);
	tok = lex(fdin,&buf);
	while(1)
	{
		//printf("tok == %d\n",tok);
		switch(tok)
		{
			case TOK_TRUE:
				_read_return(strue);
			case TOK_FALSE:
				_read_return(sfalse);
			case TOK_SUCC:
				_read_return(ssucc);
			case TOK_UNSUCC:
				_read_return(sunsucc);
			case TOK_CHAR:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = CHAR;
				/* To Do:
				 * o check for invalid named characters
				 * o test the length, and if's == 1, skip these strcmps
				 * o support Unicode (UTF-8) hex characters...
				 */
				if(!strncasecmp(buf,"space",5))
					ret->object.c = ' ';
				else if(!strncasecmp(buf,"newline",7))
					ret->object.c = '\n';
				else if(!strncasecmp(buf,"linefeed",8))
					ret->object.c = '\n';
				else if(!strncasecmp(buf,"carriage",8))
					ret->object.c = '\r';
				else if(!strncasecmp(buf,"bell",4))
					ret->object.c = 0x07;
				else if(!strncasecmp(buf,"nul",3))
					ret->object.c = nul;
				else if(!strncasecmp(buf,"tab",3))
					ret->object.c = '\t';
				else if(!strncasecmp(buf,"vtab",3))
					ret->object.c = '\v';
				else
					ret->object.c = buf[0];
				_read_return(ret);
			case TOK_KEY:
				ret = (SExp *) hmalloc(sizeof(SExp));
				if(buf[0] == ':')
				{
					ret->object.str = hstrdup(&buf[1]);
					ret->length = strlen(&buf[1]);
				}
				else
				{
					itmp = strlen(buf);
					buf[itmp - 1] = nul; /* key: */
					ret->object.str = hstrdup(buf);
					ret->length = itmp - 1;
				}
				ret->type = KEY;
				_read_return(ret);
			case TOK_LITSTR:
			case TOK_SYMBOL:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->object.str = hstrdup(buf);
				if(tok == TOK_LITSTR)
					ret->type = STRING;
				else
					ret->type = ATOM;
				ret->length = strlen(buf);
				_read_return(ret);
			case TOK_HEX:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = NUMBER;
				ret->object.n = (Number *) hmalloc(sizeof(Number));
				ret->object.n->nobject.z = atox(buf);
				ret->object.n->type = INTEGER;
				_read_return(ret);
			case TOK_OCT:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = NUMBER;
				ret->object.n = (Number *) hmalloc(sizeof(Number));
				ret->object.n->nobject.z = atoo(buf);
				ret->object.n->type = INTEGER;
				_read_return(ret);
			case TOK_BIN:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = NUMBER;
				ret->object.n = (Number *) hmalloc(sizeof(Number));
				ret->object.n->nobject.z = atob(buf);
				ret->object.n->type = INTEGER;
				_read_return(ret);
			case TOK_INT:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = NUMBER;
				ret->object.n = (Number *) hmalloc(sizeof(Number));
				ret->object.n->nobject.z = atoi(buf);
				ret->object.n->type = INTEGER;
				_read_return(ret);
			case TOK_REAL:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = NUMBER;
				ret->object.n = (Number *) hmalloc(sizeof(Number));
				ret->object.n->nobject.real = strtod(buf,nil);
				ret->object.n->type = REAL;
				_read_return(ret);
			case TOK_RATIO:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = NUMBER;
				ret->object.n = (Number *) hmalloc(sizeof(Number));
				ret->object.n->type = RATIONAL;
				while(buf[iter] != '/') iter++;
				ret->object.n->nobject.rational.num = atoi(buf);
				ret->object.n->nobject.rational.den = atoi(&buf[iter + 1]);
				/* rationals should always in lowest terms */
				if((itmp = _igcd(ret->object.n->nobject.rational.num,ret->object.n->nobject.rational.den)) != 1)
				{
					while(itmp != 1) 
					{
						ret->object.n->nobject.rational.num /= itmp;
						ret->object.n->nobject.rational.den /= itmp;
						itmp = _igcd(ret->object.n->nobject.rational.num,ret->object.n->nobject.rational.den);
					}
				}
				if(ret->object.n->nobject.rational.den == 1)
				{
					itmp = ret->object.n->nobject.rational.num;
					ret->object.n->type = INTEGER;
					ret->object.n->nobject.z = itmp;
				}
				_read_return(ret);
			case TOK_COMPL:
				ret = (SExp *) hmalloc(sizeof(SExp));
				ret->type = NUMBER;
				ret->object.n = (Number *) hmalloc(sizeof(Number));
				ret->object.n->type = COMPLEX;
				if(buf[0] == '-' || buf[0] == '+')
					iter = 1;
				while(buf[iter] != '+' && buf[iter] != '-') iter++;
				ret->object.n->nobject.complex.r = atof(buf);
				ret->object.n->nobject.complex.i = atof(&buf[iter]);
				/* parse a complex, similar to a rational */
				_read_return(ret);
            case TOK_DATCOM:
                tmp1 = llread(fdin);
                _read_return(llread(fdin));
			case TOK_LPAREN:
				tmp1 = llread(fdin);
				if(tmp1 == fake_rpar)
					return snil;
				holder = cons(nil,snil);
				ret = holder;
				while(1)
				{
					//printf("tmp1->type == %s\n",typenames[tmp1->type]);
					if(tmp1 == seof || tmp1->type == SEOF)
					  {
					    if(feof(fdin))
						return makeerror(0,0,"received #e before end of list!");
					  }
					if(tmp1 == fake_rpar) 
					{
						//printf("tmp1 == fake_rpar\n");
						ret->object.clist.rest = snil;
						break;
					}
					else if(tmp1->type == ATOM && !strcmp(tmp1->object.str,"."))
					{
						// dotted list
						ret->object.clist.rest = llread(fdin);
						tmp1 = llread(fdin);
						if(tmp1 != fake_rpar)
							return makeerror(0,0,"mal-formmated dotted list");
						break;
					}
					else
					{
						if(ret->object.clist.first == nil)
						{
							ret->object.clist.first = tmp1;
							//ret->object.clist.rest = cons(nil,snil);
						}
						else
						{
							ret->object.clist.rest = cons(tmp1,snil);
							ret = ret->object.clist.rest;
						}
					}
					tmp1 = llread(fdin);
				}
				_read_return(holder);
			case TOK_NQUOTE:
				tmp0 = llread(fdin);
				if(tmp0->type == ERROR)
					return tmp0;
				return cons(makeatom("quote"),cons(tmp0, snil));
			case TOK_MQUOTE:
				tmp0 = llread(fdin);
				if(tmp0->type == ERROR)
					return tmp0;
				return cons(makeatom("quasiquote"),cons(tmp0,snil));
			case TOK_UNQUOT:
				tmp0 = llread(fdin);
				if(tmp0->type == ERROR)
					return tmp0;
				return cons(makeatom("unquote"),cons(tmp0,snil));
			case TOK_SPLICE:
				tmp0 = llread(fdin);
				if(tmp0->type == ERROR)
					return tmp0;
				return cons(makeatom("unquote-splice"),cons(tmp0,snil));
			case TOK_RPAREN: 
				return fake_rpar;
			case TOK_LSQUAR: /* may be have to go fully recursive... */
				tmp1 = llread(fdin);
				if(tmp1 == fake_rsqr)
					return makevector(0,snil);
				holder = cons(nil,snil);
				ret = holder;
				while(1)
				{
					if(tmp1 == seof || tmp1->type == SEOF)
					  {
					    if(feof(fdin))
						return makeerror(0,0,"recieved #e before end of vector!");
					  }
					if(tmp1 == fake_rsqr) 
					{
						ret->object.clist.rest = snil;
						break;
					}
					else
					{
						/* NOTE this, and lparen above it,
						 * look like they are adding one extra
						 * cons to each list...
						 */
						if(ret->object.clist.first == nil)
						{
							ret->object.clist.first = tmp1;
							//ret->object.clist.rest = cons(nil,snil);
						}
						else
						{
							ret->object.clist.rest = cons(tmp1,snil);
							ret = ret->object.clist.rest;
						}
					}
					tmp1 = llread(fdin);
				}
				itmp = pairlength(holder);
				tmp0 = (SExp *)hmalloc(sizeof(SExp));
				tmp0->type = VECTOR;
				tmp0->object.vec = (SExp **)hmalloc(sizeof(SExp *) * itmp);
				tmp0->length = itmp;
				for(iter = 0;iter < itmp;iter++)
				{
					tmp0->object.vec[iter] = car(holder);
					holder = cdr(holder);
				}
				return tmp0;
			case TOK_RSQUAR:
				return fake_rsqr;
			case TOK_LCURLY:
				holder = makedict();
				while(1)
				{
					tmp0 = llread(fdin);
					if(tmp0 == seof)
						return makeerror(0,0,"#e received before end of dictionary literal!");
					if(tmp0 == fake_rcur)
						break;
					if(tmp0->type != STRING && tmp0->type != KEY && tmp0->type != ATOM)
						return makeerror(0,0,"invalid key type used in dict literal");
					tmp1 = llread(fdin);
					if(tmp1 == seof)
					  {
					    if(feof(fdin))
						return makeerror(0,0,"#e received before end of dictionary literal!");
					  }
					if(tmp1 == fake_rcur)
					{
						trie_put(tmp0->object.str,snil,holder->object.dict);
						break;
					}
					trie_put(tmp0->object.str,tmp1,holder->object.dict);
				}
				return holder;
			case TOK_RCURLY:
				return fake_rcur;
			case TOK_HERROR:
				return makeerror(0,0,"Syntax error: illegal hash object!");
				/* # error; return some S-Expression error type. */
			case TOK_SERROR:
				return makeerror(0,0,"reached #e before end of string");
			case TOK_LEOF:
				_read_return(seof);
			case TOK_LVOID:
				_read_return(svoid);
			case TOK_EOF:
				return seof;
			default:
				//printf("made it to default case: %d\n",tok);
				return snil;
		}
	}
	return snil;
}
char *
_itoa(char *b, int s, int *offset)
/* offset is a reach into b, so that
 * things like format & integer->string
 * can use _itoa on buffers that have already
 * been allocated...
 */
{
	int iter = *offset, rev_ptr = *offset, holder = 0;
	char c0 = ' ';
	if(b == nil)
		return nil;
	if(s < 0)
	{
		s *= -1;
		b[iter] = '-';
		iter++;
		rev_ptr++;
	}
	while(iter < MAX_STRING)
	{
		b[iter] = (s % 10) + '0';
		s /= 10;
		iter++;
		if(s == 0)
			break;
	}
	b[iter] = nul;
	holder = iter;
	for(iter--;rev_ptr < iter;rev_ptr++, iter--)
	{
		c0 = b[rev_ptr];
		b[rev_ptr] = b[iter];
		b[iter] = c0;
	}
	*offset = holder;
	return b;
}
char *
_itox(char *b, unsigned int s, int *offset)
{
	char *ret = b, tmp = 0;
	unsigned int iter = *offset, rev_iter = 0, holder = 0;
	if(b == nil)
		return nil;
	while(iter < MAX_STRING)
	{
		rev_iter = s & 0x0F;
		if(rev_iter >= 0 && rev_iter <= 9)
			b[iter] = rev_iter + '0';
		else if(rev_iter >= 0x0A && rev_iter <= 0x0F)
			b[iter] = (rev_iter - 0x0A) + 'A';
		s >>= 4;
		iter++;
		if(s == 0)
			break;	
	}
	b[iter] = nul;
	holder = iter;
	for(iter--, rev_iter = *offset; rev_iter <= iter;rev_iter++, iter--)
	{
		tmp = b[iter];
		b[iter] = b[rev_iter];
		b[rev_iter] = tmp;
	}
	*offset = holder;
	return b;
}
char *
_itoo(char *b, unsigned int s, int *offset)
{
	char *ret = b, tmp = 0;
	unsigned int iter = *offset, rev_iter = 0, holder = 0;
	if(b == nil)
		return nil;
	while(iter < MAX_STRING)
	{
		rev_iter = s & 07;
		if(rev_iter >= 0 && rev_iter <= 7)
			b[iter] = rev_iter + '0';
		s >>= 3;
		iter++;
		if(s == 0)
			break;	
	}
	b[iter] = nul;
	holder = iter;
	for(iter--, rev_iter = *offset; rev_iter <= iter;rev_iter++, iter--)
	{
		tmp = b[iter];
		b[iter] = b[rev_iter];
		b[rev_iter] = tmp;
	}
	*offset = holder;
	return b;
}
char *
_ftoa(char *b, double s, int *offset, int flag)
{
	char *tmp = nil;
	tmp = (char *)hmalloc(sizeof(char) * 64);
	if(flag)
		snprintf(tmp,64,"%E",s);
	else
		snprintf(tmp,64,"%g",s);
	return _strcpy(b,tmp,offset);
}
char *
_strcpy(char *dest, char *src, int *offset)
{
	int diter = *offset, siter = 0;
	if(dest == nil)
		return nil;
	while(src[siter] != nul)
		dest[diter++] = src[siter++];
	*offset = diter;
	return dest;
}
int
atox(char *a)
{
	int iter = 0, len = 0, ret = 0, sign = 1;
	if(a == nil)
		return 0;
	len = strlen(a);
	if(a[0] == '-')
	{
		sign = -1;
		iter++;
	}
	else if(a[0] == '+')
	{
		sign = 1;
		iter++;
	}
	for(;iter < len;iter++)
	{
		if(a[iter] >= 'a' && a[iter] <= 'f')
			ret += (a[iter] - 97 + 10);
		else if(a[iter] >= 'A' && a[iter] <= 'F')
			ret += (a[iter] - 55);
		else
			ret += (a[iter] - 48);
		if(iter < (len - 1))
			ret <<= 4;
	}
	return sign * ret;
}
int
atoo(char *a)
{
	int iter = 0, len = 0, ret = 0, sign = 1;
	if(a == nil)
		return 0;
	len = strlen(a);
	if(a[0] == '-')
	{
		sign = -1;
		iter++;
	}
	else if(a[0] == '+')
	{
		sign = 1;
		iter++;
	}
	for(;iter < len;iter++)
	{
		ret += (a[iter] - 48);
		if(iter < (len - 1))
			ret <<= 3;
	}
	return sign * ret;
}
int
atob(char *a)
{
	int iter = 0, len = 0, ret = 0, sign = 1;
	if(a == nil)
		return 0;
	len = strlen(a);
	if(a[0] == '-')
	{
		sign = -1;
		iter++;
	}
	else if(a[0] == '+')
	{
		sign = 1;
		iter++;
	}
	for(;iter < len;iter++)
	{
		ret <<= 1;
		ret += (a[iter] - 48);
	}
	return sign * ret;
}
int 
fmtlookup(char c)
{
	int rc = 0;
	char codes[] = {'A','a','s','S','x','X','o','O','~','f','F','i','%','R','m','M','C','c','l','L','b','B','g','G','n','N','v','V','t','T',0};
	while(codes[rc] != nul)
	{
		if(codes[rc] == c)
			return rc + 1;
		rc++;
	}
	return -1;
}
SExp *
format(SExp *rst, Symbol *e)
{
	int state = 0, bufiter = 0, iter = 0, cur_buf_size = 64, viter = 0;
	SExp *tmp = snil, *tmp1 = snil, *tmp2 = snil, *tmp3 = snil, *ret = snil, *src = snil;
	char *buf = nil, *str = nil;
	buf = (char *)hmalloc(sizeof(char) * cur_buf_size);
	src = car(rst);
	rst = cdr(rst);
	str = src->object.str;
__format_base:
	switch(state)
	{
		case 0: /* base case; basically, dispatch */
			if(iter >= src->length)
				break;
			if(bufiter >= (cur_buf_size - 2))
			{
				cur_buf_size += 64;
				buf = hrealloc(buf,sizeof(char) * cur_buf_size);
			}
			if(str[iter] == '~')
			{
				state = fmtlookup(str[iter + 1]);
				iter += 2;
			}
			else
			{
				buf[bufiter] = str[iter];
				bufiter++;
				iter++;
			}
			goto __format_base;
		case 1: /* A */
			// need to type dispatch here...
			tmp = car(rst);
			switch(tmp->type)
			{
				case KEY:
				case ATOM:
				case STRING:
					state = fmtlookup('S');
					goto __format_base;
				case BOOL:
					state = fmtlookup('B');
					goto __format_base;
				case GOAL:
					state = fmtlookup('G');
					goto __format_base;
				case VECTOR:
					state = fmtlookup('V');
					goto __format_base;
				case PAIR:
					state = fmtlookup('L');
					goto __format_base;
				case NUMBER:
					state = fmtlookup('N');
					goto __format_base;
				case CHAR:
					state = fmtlookup('C');
					goto __format_base;
				case NIL:
					rst = cdr(rst);
					buf[bufiter++] = '\'';
					buf[bufiter++] = '(';
					buf[bufiter++] = ')';
					state = 0;
					goto __format_base;
				case DICT:
					state = fmtlookup('T');
					goto __format_base;
				default: 
					break;
			}
			break;
		case 2: /* a */
			tmp = car(rst);
			switch(tmp->type)
			{
				case KEY:
				case ATOM:
				case STRING:
					state = fmtlookup('s');
					goto __format_base;
				case BOOL:
					state = fmtlookup('b');
					goto __format_base;
				case GOAL:
					state = fmtlookup('g');
					goto __format_base;
				case VECTOR:
					state = fmtlookup('v');
					goto __format_base;
				case PAIR:
					state = fmtlookup('l');
					goto __format_base;
				case NUMBER:
					state = fmtlookup('n');
					goto __format_base;
				case CHAR:
					state = fmtlookup('c');
					goto __format_base;
				case NIL:
					rst = cdr(rst);
					buf[bufiter++] = 'n';
					buf[bufiter++] = 'i';
					buf[bufiter++] = 'l';
					state = 0;
					goto __format_base;
				case DICT:
					state = fmtlookup('t');
					goto __format_base;
				default: 
					break;
			}
			break;
		case 3: /* s */
			tmp = car(rst);
			rst = cdr(rst);
			if((bufiter + tmp->length) >= cur_buf_size)
			{
				cur_buf_size += bufiter + tmp->length + 64;
				buf = hrealloc(buf,sizeof(char) * cur_buf_size);
			}
			if(tmp->type == STRING || tmp->type == ATOM || tmp->type == KEY)
				buf = _strcpy(buf,tmp->object.str,&bufiter);
			else
				buf[bufiter++] = '#';
			state = 0;
			goto __format_base;
		case 4: /* S */
			tmp = car(rst);
			rst = cdr(rst);
			if((bufiter + tmp->length + 2) >= cur_buf_size)
			{
				cur_buf_size += bufiter + tmp->length + 2 + 64;
				buf = hrealloc(buf,sizeof(char) * cur_buf_size);
			}
			if(tmp->type == STRING)
			{
				buf[bufiter++] = '"';
				buf = _strcpy(buf,tmp->object.str,&bufiter);
				buf[bufiter++] = '"';
			}
			else
				buf[bufiter++] = '#';
			state = 0;
			goto __format_base;
		case 5: /* x */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == NUMBER && NTYPE(tmp) == INTEGER)
				buf = _itox(buf,AINT(tmp),&bufiter);
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
			}
			state = 0;
			goto __format_base;
		case 6: /* X */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == NUMBER && NTYPE(tmp) == INTEGER)
			{
				buf[bufiter++] = '#';
				buf[bufiter++] = 'x';
				buf = _itox(buf,AINT(tmp),&bufiter);
			}
			else if(tmp->type == NUMBER && NTYPE(tmp) == REAL)
			{
				buf[bufiter++] = '#';
				buf[bufiter++] = 'x';
				buf = _itox(buf,(int)tmp3->object.n->nobject.real,&bufiter);
			}
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
			}
			state = 0;
			goto __format_base;
		case 7: /* o */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == NUMBER && NTYPE(tmp) == INTEGER)
				buf = _itoo(buf,AINT(tmp),&bufiter);
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
			}
			state = 0;
			goto __format_base;
		case 8: /* O */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == NUMBER && NTYPE(tmp) == INTEGER)
			{
				buf[bufiter++] = '#';
				buf[bufiter++] = 'o';
				buf = _itoo(buf,AINT(tmp),&bufiter);
			}
			else if(tmp->type == NUMBER && NTYPE(tmp) == REAL)
			{
				buf[bufiter++] = '#';
				buf[bufiter++] = 'o';
				buf = _itoo(buf,(int)tmp3->object.n->nobject.real,&bufiter);
			}
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
			}
			state = 0;
			goto __format_base;
		case 9: /* ~ */
			buf[bufiter++] = '~';
			state = 0;
			goto __format_base;
		case 10: /* f */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == NUMBER && NTYPE(tmp) == REAL)
				buf = _ftoa(buf,AREAL(tmp),&bufiter,0);
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
			}
			state = 0;
			goto __format_base;
		case 11: /* F */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == NUMBER && NTYPE(tmp) == REAL)
				buf = _ftoa(buf,AREAL(tmp),&bufiter,1);
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
			}
			state = 0;
			goto __format_base;
		case 12: /* i */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == NUMBER && NTYPE(tmp) == INTEGER)
				buf = _itoa(buf,AINT(tmp),&bufiter);
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
			}
			state = 0;
			goto __format_base;
		case 13: /* % */
			buf[bufiter++] = '\n';
			state = 0;
			goto __format_base;
		case 14: /* R */
		case 15: /* m */
		case 16: /* M */
		case 17: /* C */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != CHAR)
				buf[bufiter++] = '#';
			else
			{
				buf[bufiter++] = '#';
				buf[bufiter++] = '\\';
				buf[bufiter++] = tmp->object.c;
			}
			state = 0;
			goto __format_base;
		case 18: /* c */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != CHAR)
				buf[bufiter++] = '#';
			else
				buf[bufiter++] = tmp->object.c;
			state = 0;
			goto __format_base;
		case 19: /* l */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != PAIR)
			{
				buf[bufiter++] = '#';
				state = 0;
				goto __format_base;
			}
			while(tmp != snil)
			{
				tmp1 = car(tmp);
				tmp = cdr(tmp);
				tmp2 = format(cons(makestring("~a"),cons(tmp1, snil)),e);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64;
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				buf[bufiter++] = ' ';
			}
			state = 0;
			goto __format_base;
		case 20: /* L */
			/* (format "~L" (list (list "test0" "test1") "test2"))
			 * ("test0""test2")
			 */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != PAIR)
			{
				buf[bufiter++] = '#';
				state = 0;
				goto __format_base;
			}
			buf[bufiter++] = '(';
			while(tmp != snil)
			{
				tmp1 = car(tmp);
				tmp = cdr(tmp);
				tmp2 = format(cons(makestring("~A"),cons(tmp1, snil)),e);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64;
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				buf[bufiter++] = ' ';
			}
			buf[bufiter++] = ')';
			state = 0;
			goto __format_base;
		case 21: /* b */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == BOOL)
			{
				buf[bufiter++] = '#';
				buf[bufiter++] = tmp->object.c ? 't' : 'f';
			}
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'B';
			}
			state = 0;
			goto __format_base;
		case 22: /* B */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == BOOL)
			{
				if(tmp->object.c)
					buf = _strcpy(buf,"true",&bufiter);
				else
					buf = _strcpy(buf,"false",&bufiter);
			}
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'B';
			}
			state = 0;
			goto __format_base;
		case 23: /* g */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == GOAL)
			{
				buf[bufiter++] = '#';
				buf[bufiter++] = tmp->object.c ? 's' : 'u';
			}
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'G';
			}
			state = 0;
			goto __format_base;
		case 24: /* G */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type == BOOL)
			{
				if(tmp->object.c)
					buf = _strcpy(buf,"successful",&bufiter);
				else
					buf = _strcpy(buf,"unsuccessful",&bufiter);
			}
			else
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'G';
			}
			state = 0;
			goto __format_base;
		case 25: /* n */
		case 26: /* N */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != NUMBER)
			{
				buf[bufiter++] = 'N';
				buf[bufiter++] = 'a';
				buf[bufiter++] = 'N';
				state = 0;
				goto __format_base;
			}
			switch(NTYPE(tmp))
			{
				case INTEGER:
					buf = _itoa(buf,AINT(tmp),&bufiter);
					break;
				case REAL:
					buf = _ftoa(buf,AREAL(tmp),&bufiter,0);
					break;
				case RATIONAL:
					buf = _itoa(buf,NUM(tmp),&bufiter);
					buf[bufiter++] = '/';
					buf = _itoa(buf,DEN(tmp),&bufiter);
					break;
				case COMPLEX:
					buf = _ftoa(buf,CEREAL(tmp),&bufiter,0);
					if(IMMAG(tmp) >= 0.0)
						buf[bufiter++] = '+';
					buf = _ftoa(buf,IMMAG(tmp),&bufiter,0);
					buf[bufiter++] = 'i';
					break;
			}
			state = 0;
			goto __format_base;
		case 27: /* v */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != VECTOR)
			{
				buf[bufiter++] = '#';
				state = 0;
				goto __format_base;
			}
			for(viter = 0; viter < tmp->length; viter++)
			{
				tmp1 = tmp->object.vec[viter];
				tmp2 = format(cons(makestring("~a"),cons(tmp1, snil)),e);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64;
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				if(viter < (tmp->length - 1))
					buf[bufiter++] = ' ';
			}
			state = 0;
			goto __format_base;
		case 28: /* V */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != VECTOR)
			{
				buf[bufiter++] = '#';
				state = 0;
				goto __format_base;
			}
			buf[bufiter++] = '[';
			for(viter = 0; viter < tmp->length; viter++)
			{
				tmp1 = tmp->object.vec[viter];
				tmp2 = format(cons(makestring("~A"),cons(tmp1, snil)),e);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64; // way too much
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				if(viter < (tmp->length - 1))
					buf[bufiter++] = ' ';
			}
			buf[bufiter++] = ']';
			state = 0;
			goto __format_base;
		case 29: /* t */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != DICT)
			{
				buf[bufiter++] = '#';
				state = 0;
				goto __format_base;
			}
			tmp1 = mcar(trie_keys(tmp->object.dict,tconcify(snil)));
			while(tmp1 != e->snil)
			{
				tmp2 = car(tmp1);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64;
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				buf[bufiter++] = ':';
				buf[bufiter++] = ' ';
				tmp2 = format(list(2,makestring("~a"),trie_get(tmp2->object.str,tmp->object.dict)),e);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64;
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				tmp1 = cdr(tmp1);
				if(tmp1 != snil)
					buf[bufiter++] = ',';
			}	
			state = 0;
			goto __format_base;
		case 30: /* T */
			tmp = car(rst);
			rst = cdr(rst);
			if(tmp->type != DICT)
			{
				buf[bufiter++] = '#';
				state = 0;
				goto __format_base;
			}
			tmp1 = mcar(trie_keys(tmp->object.dict,tconcify(snil)));
			buf[bufiter++] = '{';
			while(tmp1 != e->snil)
			{
				tmp2 = car(tmp1);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64;
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf[bufiter++] = '"';
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				buf[bufiter++] = '"';
				buf[bufiter++] = ' ';
				tmp2 = format(list(2,makestring("~A"),trie_get(tmp2->object.str,tmp->object.dict)),e);
				if((bufiter + tmp2->length) >= cur_buf_size)
				{
					cur_buf_size += bufiter + tmp2->length + 64;
					buf = hrealloc(buf,sizeof(char) * cur_buf_size);
				}
				buf = _strcpy(buf,tmp2->object.str,&bufiter);
				tmp1 = cdr(tmp1);
				if(tmp1 != snil)
					buf[bufiter++] = ' ';
			}	
			buf[bufiter++] = '}';
			state = 0;
			goto __format_base;
		case 999: /* return case */
			break;
	}
	buf[bufiter] = nul;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = STRING;
	ret->object.str = buf;
	ret->length = bufiter;
	return ret;
}
SExp *
fcoerce(SExp *from, SExp *to)
{
	SExp *ret = nil, *tmp = nil;
	int iter = 0, len = 0;
	if(to->type != KEY && to->type != ATOM && to->type != STRING)
		return makeerror(1,0,"coerce: the to parameter *must* be a (KEYWORD | ATOM | STRING)");
	switch(from->type)
	{
		case NUMBER:
			switch(NTYPE(from))
			{
				case INTEGER:
					if(!strncasecmp(to->object.str,"char",4))
						return makechar(AINT(from) & 0xFF);
                    else if(!strncasecmp(to->object.str,"int",3))
                        return from;
					return makeerror(1,0,"coerce: illegal coercion.");
				default:
					return makeerror(1,0,"coerce: unknown coercion attempt");
			}
		case CHAR:
			if(!strncasecmp(to->object.str,"int",3))
			{
				iter = from->object.c;
				iter &= 255;
				return makeinteger(iter);
			}
            else if(!strncasecmp(to->object.str,"char",4))
                return from;
			else
				return makeerror(1,0,"coerce: unknown coercion attempt");
		case KEY:
			if(!strncasecmp(to->object.str,"string",6))
				return makestring(from->object.str);
			else if(!strncasecmp(to->object.str,"atom",4) || !strncasecmp(to->object.str,"symbol",6))
				return makeatom(from->object.str);
			else if(!strncasecmp(to->object.str,"vector",6))
			{
				ret = makevector(from->length,nil);
				for(;iter < from->length;iter++)
					ret->object.vec[iter] = makechar(from->object.str[iter]);
				return ret;
			}
			else if(!strncasecmp(to->object.str,"key",3))
                return from;
			else
				return makeerror(2,0,"coerce: unknown coercion attempt");
		case ATOM:
			if(!strncasecmp(to->object.str,"string",6))
				return makestring(from->object.str);
			else if(!strncasecmp(to->object.str,"key",3))
			{
				ret = makeatom(from->object.str);
				ret->type = KEY;
				return ret;
			}
			else if(!strncasecmp(to->object.str,"vector",6))
			{
				ret = makevector(from->length,nil);
				for(;iter < from->length;iter++)
					ret->object.vec[iter] = makechar(from->object.str[iter]);
				return ret;
			}
            else if(!strncasecmp(to->object.str,"symbol",6))
                return from;
			else
				return makeerror(2,0,"coerce: unknown coercion attempt");
		case VECTOR:
			if(!strncasecmp(to->object.str,"string",6))
			{
				ret = makestring_v(from->length,nul);
				for(;iter < from->length;iter++)
				{
					if(from->object.vec[iter]->type != CHAR)
						return makeerror(2,0,"coerce: illegal coercion attempt: vector containing non-chars to string");
					ret->object.str[iter] = from->object.vec[iter]->object.c;
				}
				return ret;
			}
			else if(!strncasecmp(to->object.str,"pair",4))
			{
				ret = snil;
				for(iter = from->length - 1;iter >= 0;iter--)
					ret = cons(from->object.vec[iter],ret);
				return ret;
			}
            else if(!strncasecmp(to->object.str,"vector",6))
                return from;
			else
				return makeerror(2,0,"coerce: unknown coercion attempt");
		case PAIR:
			if(!strncasecmp(to->object.str,"vector",6))
			{
				ret = makevector(pairlength(from),nil);
				for(;iter < ret->length;iter++)
				{
					ret->object.vec[iter] = car(from);
					from = cdr(from);
				}
				return ret;
			}
            else if(!strncasecmp(to->object.str,"string",6))
			{
                len = pairlength(from);
				ret = makestring_v(len,nul);
                for(; iter < len; iter++)
				{
                    tmp = car(from);
					if(tmp->type != CHAR)
						return makeerror(2,0,"coerce: illegal coercion attempt: vector containing non-chars to string");
					ret->object.str[iter] = tmp->object.c;
                    from = cdr(from);
				}
				return ret;
            }
            else if(!strncasecmp(to->object.str,"pair",4))
                return from;
			else
				return makeerror(2,0,"coerce: unknown coercion attempt");
		case STRING:
			if(!strncasecmp(to->object.str,"key",3))
			{
				ret = makeatom(from->object.str);
				ret->type = KEY;
				return ret;
			}
			else if(!strncasecmp(to->object.str,"atom",4))
				return makeatom(from->object.str);
			else if(!strncasecmp(to->object.str,"vector",6))
			{
				ret = makevector(from->length,nil);
				for(;iter < from->length;iter++)
					ret->object.vec[iter] = makechar(from->object.str[iter]);
				return ret;
			}
			else if(!strncasecmp(to->object.str,"pair",4))
			{
				ret = cons(snil,snil);
				tmp = ret;
				for(;;iter++)
				{
					mcar(tmp) = makechar(from->object.str[iter]);
                    if((iter + 1) >= from->length)
                        break;
					mcdr(tmp) = cons(snil,snil);
					tmp = mcdr(tmp);
				}
				return ret;
			}
            else if(!strncasecmp(to->object.str,"string",5))
                return from;
			else if(!strncasecmp(to->object.str,"int",3))
				return makeinteger(strtol(from->object.str,nil,10));
			else if(!strncasecmp(to->object.str,"real",4))
				return makereal(strtod(from->object.str,nil));
			else
				return makeerror(1,0,"coerce: unknown coercion attempt");
		default:
			return makeerror(1,0,"coerce: unknown coercion attempt");
	}
	return makeerror(1,0,"coerce: unknown coercion attempt");
}

/* math functions */
SExp *
fsqrt(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"sqrt operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = sqrt(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = sqrt(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = sqrt((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fden(SExp *tmp0)
{
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && tmp0->object.n->type != RATIONAL))
		return makeerror(1,0,"type clash: denomenator expects a rational...");
	return makeinteger(tmp0->object.n->nobject.rational.den);
}
SExp *
fnum(SExp *tmp0)
{
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && tmp0->object.n->type != RATIONAL))
		return makeerror(1,0,"type clash: numerator expects a rational...");
	return makeinteger(tmp0->object.n->nobject.rational.num);
}
SExp *
inc_i(SExp *n, int i)
{
    switch(NTYPE(n))
    {
        case INTEGER:
            AINT(n) += i;
            break;
        case REAL:
            AREAL(n) += (i * 1.0);
            break;
        case RATIONAL:
            NUM(n) += (i * DEN(n));
            break;
        case COMPLEX:
            CEREAL(n) += (i * 1.0);
            break;
    }
    return n;
}
SExp *
fplus_in(int i, SExp *n)
{
    SExp *ret = nil;
    if(n->type != NUMBER)
        return makeerror(1,0,"type class: + operates only on numbers...");
    switch(NTYPE(n))
    {
        case INTEGER:
            return makeinteger(i + AINT(n));
        case REAL:
            return makereal(i + AREAL(n));
        case RATIONAL:
            return makerational((i * DEN(n) + NUM(n)), DEN(n));
        case COMPLEX:
            return makecomplex(i + CEREAL(n), IMMAG(n));
    }
}
SExp *
fplus_nn(SExp *i, SExp *n)
{
    SExp *ret = nil;
    if(n->type != NUMBER)
        return makeerror(1,0,"type class: + operates only on numbers...");
    switch(NTYPE(i))
    {
        case INTEGER:
            switch(NTYPE(n))
            {
                case INTEGER:
                    return makeinteger(AINT(i) + AINT(n));
                case REAL:
                    return makereal(AINT(i) + AREAL(n));
                case RATIONAL:
                    /*
                       a      c
                      --- +- ---
                       b      d
                          =
                       ad +- bc
                       --------
                          bd
                    */
                    return makerational((AINT(i) * DEN(n) + NUM(n)), DEN(n)); // b == 1 for ints
                case COMPLEX: /* nothing for now */
                    return makecomplex(AINT(i) + CEREAL(n), IMMAG(n));
            }
        case REAL:
            switch(NTYPE(n))
            {
                case INTEGER:
                    return makereal(AREAL(i) + AINT(n));
                case REAL:
                    return makereal(AREAL(i) + AREAL(n));
                case RATIONAL:
                    return makereal(AREAL(i) + ((NUM(n) * 1.0) / (DEN(n) * 1.0)));
                case COMPLEX:
                    return makecomplex(AREAL(i) + CEREAL(n), IMMAG(n));
            }
        case COMPLEX:
            switch(NTYPE(n))
            {
                case INTEGER:
                    return makecomplex(CEREAL(i) + AINT(n), IMMAG(i));
                case REAL:
                    return makecomplex(CEREAL(i) + AREAL(n), IMMAG(i));
                case RATIONAL:
                    return makecomplex(CEREAL(i) + ((NUM(n) * 1.0) / (DEN(n) * 1.0)), IMMAG(i));
                case COMPLEX:
                    return makecomplex(CEREAL(i) + CEREAL(n), IMMAG(i) + IMMAG(n));
            }
        case RATIONAL:
            switch(NTYPE(n))
            {
                case INTEGER:
                    return makerational((AINT(n) * DEN(i) + NUM(i)), DEN(i));
                case REAL:
                    return makereal(AREAL(i) + AREAL(n));
                case RATIONAL:
                    return makerational((NUM(i) * DEN(n)) + (NUM(n) * DEN(i)), DEN(i) * DEN(n));
                case COMPLEX:
                    return makecomplex(AREAL(i) + CEREAL(n), IMMAG(n));
            }
    }
}
SExp *
fplus(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil;
	int itmp = 0;
#ifdef DEBUG
    LINE_DEBUG;
    printf("rst == ");
    princ(rst);
    printf("\n");
#endif 
	tmp1 = makenumber(INTEGER);
	tmp1->object.n->nobject.z = pairlength(rst);
	if(tmp1->object.n->nobject.z == 0)
		return tmp1;
	if(tmp1->object.n->nobject.z == 1)
	{
		if(mcar(rst)->type == NUMBER)
			return car(rst);
		else
			return makeerror(1,0,"type clash: + works only on numbers...");
	}
	else
	{
		tmp0 = makenumber(INTEGER);
		tmp1 = car(rst);
		if(tmp1->type != NUMBER)
			return makeerror(1,0,"type clash: + only operates on numbers...");
		while(rst != snil)
		{
			switch(tmp1->object.n->type)
			{
				case INTEGER:
					switch(tmp0->object.n->type)
					{
						case INTEGER:
							tmp0->object.n->nobject.z += tmp1->object.n->nobject.z;
							break;
						case REAL:
							tmp0->object.n->nobject.real += tmp1->object.n->nobject.z;
							break;
						case RATIONAL:
    						tmp0->object.n->nobject.rational.num += (tmp1->object.n->nobject.z * tmp0->object.n->nobject.rational.den); 
							break;
						case COMPLEX: /* nothing for now */
							tmp0->object.n->nobject.complex.r += tmp1->object.n->nobject.z;
							break;
					}
					break;
				case REAL:
					switch(tmp0->object.n->type)
					{
						case INTEGER:
							/*
							tmp1->object.n->nobject.real += tmp0->object.n->nobject.z;
							tmp0 = tmp1; */
							tmp0->object.n->nobject.real = tmp0->object.n->nobject.z * 1.0;
							tmp0->object.n->nobject.real += tmp1->object.n->nobject.real;
							tmp0->object.n->type = REAL;
							break;
						case REAL:
							tmp0->object.n->nobject.real += tmp1->object.n->nobject.real;
							break;
						case RATIONAL:
							/*
							printf("[!] I *should* be adding a rational & a real (tmp0 & tmp1 resp.): ");
							princ(tmp0);
							printf(" , ");
							princ(tmp1);
							printf("\n");
							printf("%f\n",(tmp0->object.n->nobject.rational.num * 1.0) / (tmp0->object.n->nobject.rational.den * 1.0));
							*/
							tmp0->object.n->nobject.real = (tmp0->object.n->nobject.rational.num * 1.0) / (tmp0->object.n->nobject.rational.den * 1.0);
							tmp0->object.n->type = REAL;
							/*
							princ(tmp0);
							printf("\n");
							*/
							tmp0->object.n->nobject.real += tmp1->object.n->nobject.real;
							/*
							princ(tmp0);
							printf("\n");
							*/
							break;
						case COMPLEX:
							tmp0->object.n->nobject.complex.r += tmp1->object.n->nobject.real;
					}
					break;
				case COMPLEX:
					switch(tmp0->object.n->type)
					{
						case INTEGER:
							/*
							tmp1->object.n->nobject.real += tmp0->object.n->nobject.z;
							tmp0 = tmp1; */
							tmp0->object.n->nobject.complex.r = tmp0->object.n->nobject.z * 1.0;
							tmp0->object.n->nobject.complex.r += tmp1->object.n->nobject.complex.r;
							tmp0->object.n->nobject.complex.i = tmp1->object.n->nobject.complex.i;
							tmp0->object.n->type = COMPLEX;
							break;
						case REAL:
							//tmp0->object.n->nobject.real += tmp1->object.n->nobject.real;
							tmp0->object.n->nobject.complex.r = tmp0->object.n->nobject.real;
							tmp0->object.n->nobject.complex.r += tmp1->object.n->nobject.complex.r;
							tmp0->object.n->nobject.complex.i = tmp1->object.n->nobject.complex.i;
							tmp0->object.n->type = COMPLEX;
							break;
						case RATIONAL:
							tmp0->object.n->nobject.complex.r = (tmp0->object.n->nobject.rational.num * 1.0) / (tmp0->object.n->nobject.rational.den * 1.0);
							tmp0->object.n->nobject.complex.r += tmp1->object.n->nobject.complex.r;
							tmp0->object.n->nobject.complex.i = tmp1->object.n->nobject.complex.i;
							tmp0->object.n->type = COMPLEX;
							break;
						case COMPLEX:
							tmp0->object.n->nobject.complex.r += tmp1->object.n->nobject.complex.r;
							tmp0->object.n->nobject.complex.i += tmp1->object.n->nobject.complex.i;
					}
					break;
				case RATIONAL:
					switch(tmp0->object.n->type)
					{
						case INTEGER:
							tmp0->object.n->nobject.rational.num = tmp0->object.n->nobject.z * tmp1->object.n->nobject.rational.den;
							tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.den;
							tmp0->object.n->nobject.rational.num += tmp1->object.n->nobject.rational.num;
							tmp0->object.n->type = RATIONAL;
							break;
						case REAL:
							tmp0->object.n->nobject.real += (tmp1->object.n->nobject.rational.num * 1.0) / (tmp1->object.n->nobject.rational.den * 1.0);
							break;
						case RATIONAL:
							tmp0->object.n->nobject.rational.num *= tmp1->object.n->nobject.rational.den;
							tmp0->object.n->nobject.rational.num += (tmp1->object.n->nobject.rational.num * tmp0->object.n->nobject.rational.den);
							tmp0->object.n->nobject.rational.den *= tmp1->object.n->nobject.rational.den;
							itmp = _igcd(tmp0->object.n->nobject.rational.num,tmp0->object.n->nobject.rational.den);
							if(itmp != 1)
							{
								tmp0->object.n->nobject.rational.num /= itmp;
								tmp0->object.n->nobject.rational.den /= itmp;
							}
							break;
						case COMPLEX:
							tmp0->object.n->nobject.complex.r += (tmp1->object.n->nobject.rational.num * 1.0) / (tmp1->object.n->nobject.rational.den * 1.0);
							break;
					}
					break;
			}
			rst = cdr(rst);
			tmp1 = car(rst);
		}
	}
	return tmp0;
}
SExp *
fmult(SExp *rst)
{
	int itmp = 0;
	SExp *tmp0 = nil, *tmp1 = nil, *tmp2 = nil;
	double ftmp = 0.0;
	itmp = pairlength(rst);
	if(itmp == 0)
		return makenumber(INTEGER);
	if(itmp == 1)
	{
		if(mcar(rst)->type == NUMBER)
			return car(rst);
		else
			return makeerror(1,0,"type clash: * works only on numbers...");
	}
	else
	{
		tmp0 = makenumber(INTEGER);
		tmp0->object.n->nobject.z = 1;
		tmp1 = car(rst);
		if(tmp1->type != NUMBER)
			return makeerror(1,0,"type clash: + only operates on numbers...");
		//printf("[!] at loop level for #\\*\n");
		while(rst != snil)
		{
			switch(tmp1->object.n->type)
			{
				case INTEGER:
					switch(tmp0->object.n->type)
					{
						case INTEGER:
							//printf("\t[!] Integer * Integer?\n");
							tmp0->object.n->nobject.z *= tmp1->object.n->nobject.z;
							break;
						case REAL:
							//printf("\t[!] Integer * Real?\n");
							tmp0->object.n->nobject.real *= tmp1->object.n->nobject.z;
							break;
						case RATIONAL:
	   									tmp0->object.n->nobject.rational.num *= tmp1->object.n->nobject.z; 
							break;
						case COMPLEX: /* nothing for now */
							CEREAL(tmp0) = CEREAL(tmp0) * tmp1->object.n->nobject.z;
							IMMAG(tmp0) = IMMAG(tmp0) * tmp1->object.n->nobject.z;
							break;
					}
					break;
				case REAL:
					switch(tmp0->object.n->type)
					{
						case INTEGER:
							tmp0->object.n->nobject.real = tmp0->object.n->nobject.z * 1.0;
							tmp0->object.n->nobject.real *= tmp1->object.n->nobject.real;
							tmp0->object.n->type = REAL;
							break;
						case REAL:
							tmp0->object.n->nobject.real *= tmp1->object.n->nobject.real;
							break;
						case RATIONAL:
							tmp0->object.n->nobject.real = (tmp0->object.n->nobject.rational.num * 1.0) / (tmp0->object.n->nobject.rational.den * 1.0);
							tmp0->object.n->type = REAL;
							tmp0->object.n->nobject.real *= tmp1->object.n->nobject.real;
							break;
						case COMPLEX:
							CEREAL(tmp0) = tmp1->object.n->nobject.real * CEREAL(tmp0);
							IMMAG(tmp0) = tmp1->object.n->nobject.real * IMMAG(tmp0);
							break;
					}
					break;
				case RATIONAL:
					switch(tmp0->object.n->type)
					{
						case INTEGER:
							tmp0->object.n->nobject.rational.num = tmp0->object.n->nobject.z * tmp1->object.n->nobject.rational.num;
							tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.den;
							tmp0->object.n->type = RATIONAL;
							break;
						case REAL:
							tmp0->object.n->nobject.real *= (tmp1->object.n->nobject.rational.num * 1.0) / (tmp1->object.n->nobject.rational.den * 1.0);
							break;
						case RATIONAL:
							tmp0->object.n->nobject.rational.num *= tmp1->object.n->nobject.rational.num;
							tmp0->object.n->nobject.rational.den *= tmp1->object.n->nobject.rational.den;
							itmp = _igcd(tmp0->object.n->nobject.rational.num,tmp0->object.n->nobject.rational.den);
							if(itmp != 1)
							{
								tmp0->object.n->nobject.rational.num /= itmp;
								tmp0->object.n->nobject.rational.den /= itmp;
							}
							break;
						case COMPLEX:
							ftmp = ((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
							CEREAL(tmp0) = ftmp * CEREAL(tmp0);
							IMMAG(tmp0) = ftmp * IMMAG(tmp0);
							break;
					}
					break;
				case COMPLEX:
					switch(NTYPE(tmp0))
					{
						case INTEGER:
							/*printf("made it to integer * complex\n");
							printf("%s * %s\n", numtypes[NTYPE(tmp0)], numtypes[NTYPE(tmp1)]);
							printf("tmp0 => ");
							princ(tmp0);
							printf(", tmp1 => ");
							princ(tmp1);
							printf("\n");
							printf("#C(%f %f)\n",CEREAL(tmp1),IMMAG(tmp1));
							printf("#C(%f %f)\n",tmp0->object.n->nobject.z * CEREAL(tmp1), tmp0->object.n->nobject.z * IMMAG(tmp1));*/
							ftmp = tmp0->object.n->nobject.z * IMMAG(tmp1);
							/* upgrade to complex; (int + 0i) * (c + d) -> [int * c] + [int * d] */ 
							CEREAL(tmp0) = tmp0->object.n->nobject.z * CEREAL(tmp1);
							IMMAG(tmp0) = ftmp;
							NTYPE(tmp0) = COMPLEX;
							break;
						case RATIONAL:
							ftmp = ((NUM(tmp0) * 1.0) / (DEN(tmp0) * 1.0));
							CEREAL(tmp0) = ftmp * CEREAL(tmp1);
							IMMAG(tmp0) = ftmp * IMMAG(tmp1);
							NTYPE(tmp0) = COMPLEX;
							break;
						case REAL:
							/*printf("%s * %s\n", numtypes[NTYPE(tmp0)], numtypes[NTYPE(tmp1)]);
							printf("tmp0 => ");
							princ(tmp0);
							printf(", tmp1 => ");
							princ(tmp1);
							printf("\n");
							printf("#C(%f %f)\n",CEREAL(tmp1),IMMAG(tmp1));
							printf("#C(%f %f)\n",tmp0->object.n->nobject.real * CEREAL(tmp1), tmp0->object.n->nobject.real * IMMAG(tmp1));*/
							ftmp = tmp0->object.n->nobject.real;
							CEREAL(tmp0) = ftmp * CEREAL(tmp1);
							IMMAG(tmp0) = ftmp * IMMAG(tmp1);
							NTYPE(tmp0) = COMPLEX;
							break;
						case COMPLEX:
							/*printf("%s * %s\n", numtypes[NTYPE(tmp0)], numtypes[NTYPE(tmp1)]);
							printf("tmp0 => ");
							princ(tmp0);
							printf(", tmp1 => ");
							princ(tmp1);
							printf("\n");
							printf("#C(%f %f)\n",CEREAL(tmp1),IMMAG(tmp1));*/
							ftmp = (IMMAG(tmp0) * CEREAL(tmp1)) + (CEREAL(tmp0) * IMMAG(tmp1));
							CEREAL(tmp0) = (CEREAL(tmp0) * CEREAL(tmp1)) - (IMMAG(tmp0) * IMMAG(tmp1));
							IMMAG(tmp0) = ftmp;
							break;
					}
			}
			rst = cdr(rst);
			tmp1 = car(rst);
		}
	}
	return tmp0;
}
SExp *
fsubt(SExp *rst)
{
	int itmp = 0;
	SExp *tmp0 = nil, *tmp1 = nil, *tmp2 = nil;
	itmp = pairlength(rst);
	if(itmp == 0)
		return makeerror(1,0,"- expects at least one argument...");
	tmp0 = makenumber(INTEGER);
	tmp1 = car(rst);
	rst = cdr(rst);
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"- operates only on numbers...");
	switch(tmp1->object.n->type)
	{
		case INTEGER:
			if(itmp == 1)
				tmp0->object.n->nobject.z = - tmp1->object.n->nobject.z;
			else
				tmp0->object.n->nobject.z = tmp1->object.n->nobject.z;
			break;
		case REAL:
			tmp0->object.n->type = REAL;
			if(itmp == 1)
				tmp0->object.n->nobject.real = -tmp1->object.n->nobject.real;
			else
				tmp0->object.n->nobject.real = tmp1->object.n->nobject.real;
			break;
		case RATIONAL:
			tmp0->object.n->type = RATIONAL;
			if(itmp == 1)
				tmp0->object.n->nobject.rational.num = -tmp1->object.n->nobject.rational.num;
			else
				tmp0->object.n->nobject.rational.num = tmp1->object.n->nobject.rational.num;
			tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.den;
			break;
		case COMPLEX:
			NTYPE(tmp0) = COMPLEX;
			if(itmp == 1)
			{
				CEREAL(tmp0) = -CEREAL(tmp1);
				IMMAG(tmp0) = -IMMAG(tmp1);
			}
			else
			{
				CEREAL(tmp0) = CEREAL(tmp1);
				IMMAG(tmp0) = IMMAG(tmp1);
			}
			break;
	}
	if(itmp == 1)
		return tmp0;
	tmp1 = car(rst);
	while(rst != snil)
	{
		switch(tmp1->object.n->type)
		{
			case INTEGER:
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						tmp0->object.n->nobject.z -= tmp1->object.n->nobject.z;
						break;
					case REAL:
						tmp0->object.n->nobject.real -= tmp1->object.n->nobject.z;
						break;
					case RATIONAL:
						tmp0->object.n->nobject.rational.num -= (tmp1->object.n->nobject.z * tmp0->object.n->nobject.rational.den); 
						break;
					case COMPLEX: /* nothing for now */
						CEREAL(tmp0) -= tmp1->object.n->nobject.z;
						break;
				}
				break;
			case REAL:
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						/*
						tmp1->object.n->nobject.real += tmp0->object.n->nobject.z;
						tmp0 = tmp1; */
						tmp0->object.n->nobject.real = tmp0->object.n->nobject.z * 1.0;
						tmp0->object.n->nobject.real -= tmp1->object.n->nobject.real;
						tmp0->object.n->type = REAL;
						break;
					case REAL:
						tmp0->object.n->nobject.real -= tmp1->object.n->nobject.real;
						break;
					case RATIONAL:
						tmp0->object.n->nobject.real = (tmp0->object.n->nobject.rational.num * 1.0) / (tmp0->object.n->nobject.rational.den * 1.0);
						tmp0->object.n->type = REAL;
						tmp0->object.n->nobject.real -= tmp1->object.n->nobject.real;
						break;
					case COMPLEX:
						CEREAL(tmp0) -= tmp1->object.n->nobject.real;
						break;
				}
				break;
			case RATIONAL:
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						tmp0->object.n->nobject.rational.num = tmp0->object.n->nobject.z * tmp1->object.n->nobject.rational.den;
						tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.den;
						tmp0->object.n->nobject.rational.num -= tmp1->object.n->nobject.rational.num;
						tmp0->object.n->type = RATIONAL;
						break;
					case REAL:
						tmp0->object.n->nobject.real -= (tmp1->object.n->nobject.rational.num * 1.0) / (tmp1->object.n->nobject.rational.den * 1.0);
						break;
					case RATIONAL:
						tmp0->object.n->nobject.rational.num *= tmp1->object.n->nobject.rational.den;
						tmp0->object.n->nobject.rational.num -= (tmp1->object.n->nobject.rational.num * tmp0->object.n->nobject.rational.den);
						tmp0->object.n->nobject.rational.den *= tmp1->object.n->nobject.rational.den;
						itmp = _igcd(tmp0->object.n->nobject.rational.num,tmp0->object.n->nobject.rational.den);
						if(itmp != 1)
						{
							tmp0->object.n->nobject.rational.num /= itmp;
							tmp0->object.n->nobject.rational.den /= itmp;
						}
						break;
					case COMPLEX:
						CEREAL(tmp0) -= (tmp1->object.n->nobject.rational.num * 1.0) / (tmp1->object.n->nobject.rational.den * 1.0);
				}
				break;
			case COMPLEX:
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						CEREAL(tmp0) = tmp0->object.n->nobject.z - CEREAL(tmp1);
						IMMAG(tmp0) = 0.0 - IMMAG(tmp1);
						NTYPE(tmp0) = COMPLEX;
						break;
					case RATIONAL:
						CEREAL(tmp0) = ((NUM(tmp0) * 1.0) / (DEN(tmp0) * 1.0)) - CEREAL(tmp1);
						IMMAG(tmp0) = 0.0 - IMMAG(tmp1);
						NTYPE(tmp0) = COMPLEX;
						break;
					case REAL:
						CEREAL(tmp0) = tmp0->object.n->nobject.real - CEREAL(tmp1);
						IMMAG(tmp0) = 0.0 - IMMAG(tmp1);
						NTYPE(tmp0) = COMPLEX;
						break;
					case COMPLEX:
						CEREAL(tmp0) -= CEREAL(tmp1);
						IMMAG(tmp0) -= IMMAG(tmp1);
						break;
				}
				break;
		}
		rst = cdr(rst);
		tmp1 = car(rst);
	}
	return tmp0;
}
SExp *
fdivd(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil, *tmp2 = nil;
	int itmp = 0;
	double ftmp = 0.0, f0 = 0.0, f1 = 0.0, f2 = 0.0;
	tmp0 = makenumber(INTEGER);
	itmp = pairlength(rst);
	if(itmp == 0)
		return makeerror(1,0,"/ needs at least one argument...");
	else if(itmp == 1)
	{
		if(NTYPE(mcar(rst)) == COMPLEX)
		{
			tmp1 = car(rst);
			ftmp = (CEREAL(tmp1) * CEREAL(tmp1)) + (IMMAG(tmp1) * IMMAG(tmp1));
			CEREAL(tmp0) = CEREAL(tmp1) / ftmp;
			IMMAG(tmp0) = ( - IMMAG(tmp1)) / ftmp;
			NTYPE(tmp0) = COMPLEX;
			return tmp0;
		}
		else
			tmp0->object.n->nobject.z = 1;
	}	
	else
	{
		tmp1 = car(rst);
		rst = cdr(rst);
		if(tmp1->type != NUMBER)
			return makeerror(1,0,"/ operates on numbers only...");
		switch(tmp1->object.n->type)
		{
			case INTEGER:
				tmp0->object.n->nobject.z = tmp1->object.n->nobject.z;
				break;
			case REAL:
				tmp0->object.n->type = REAL;
				tmp0->object.n->nobject.real = tmp1->object.n->nobject.real;
				break;
			case RATIONAL:
				tmp0->object.n->type = RATIONAL;
				tmp0->object.n->nobject.rational.num = tmp1->object.n->nobject.rational.num;
				tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.den;
				break;
			case COMPLEX:
				CEREAL(tmp0) = CEREAL(tmp1);
				IMMAG(tmp0) = IMMAG(tmp1);
				NTYPE(tmp0) = COMPLEX;
				break;
		}
	}
	while(rst != snil)
	{
		tmp1 = car(rst);
		if(tmp1->type != NUMBER)
			return makeerror(1,0,"/ operates on numbers only...");
		switch(tmp1->object.n->type)
		{
			case INTEGER:
				if(tmp1->object.n->nobject.z == 0)
					return makeerror(1,0,"division by zero (& infinity isnt currently a number)");
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						if((tmp0->object.n->nobject.z % tmp1->object.n->nobject.z) == 0)
							tmp0->object.n->nobject.z /= tmp1->object.n->nobject.z;
						else
						{
							tmp0->object.n->nobject.rational.num = tmp0->object.n->nobject.z;
							tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.z;
							tmp0->object.n->type = RATIONAL;
						}
						break;
					case REAL:
						tmp0->object.n->nobject.real /= tmp1->object.n->nobject.z * 1.0;
						break;
					case RATIONAL:
						tmp0->object.n->nobject.rational.den *= tmp1->object.n->nobject.z;
						break;
					case COMPLEX:
						ftmp = (tmp1->object.n->nobject.z * 1.0) * (tmp1->object.n->nobject.z * 1.0);
						CEREAL(tmp0) = (CEREAL(tmp0) * tmp1->object.n->nobject.z) / ftmp;
						IMMAG(tmp0) = (IMMAG(tmp0) * tmp1->object.n->nobject.z) / ftmp;
						break;
				}
				break;
			case REAL:
				if(tmp1->object.n->nobject.real == 0.0)
					return makeerror(1,0,"division by zero (& inexact infinity is not currently a number...)");
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						tmp0->object.n->nobject.real = (tmp0->object.n->nobject.z * 1.0) / tmp1->object.n->nobject.real;
						tmp0->object.n->type = REAL;
						break;
					case REAL:
						tmp0->object.n->nobject.real /= tmp1->object.n->nobject.real;
						break;
					case RATIONAL:
						tmp0->object.n->nobject.real = ((tmp0->object.n->nobject.rational.num * 1.0) / (tmp0->object.n->nobject.rational.den * 1.0)) / tmp1->object.n->nobject.real;
						tmp0->object.n->type = REAL;
						break;
					case COMPLEX:
						ftmp = tmp1->object.n->nobject.real * tmp1->object.n->nobject.real;
						CEREAL(tmp0) = (CEREAL(tmp0) * tmp1->object.n->nobject.real) / ftmp;
						IMMAG(tmp0) = (IMMAG(tmp0) * tmp1->object.n->nobject.real) / ftmp;
						break;
				}
				break;
			case RATIONAL:
				if(tmp1->object.n->nobject.rational.num == 0 || tmp1->object.n->nobject.rational.den == 0)
					return makeerror(1,0,"division by zero (& exact infinity isn't currently a number...)");
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						tmp0->object.n->nobject.rational.num = tmp0->object.n->nobject.z * tmp1->object.n->nobject.rational.den;
						tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.num;
						tmp0->object.n->type = RATIONAL;
						break;
					case REAL:
						tmp0->object.n->nobject.real = ((tmp1->object.n->nobject.rational.num * 1.0) / (tmp1->object.n->nobject.rational.den * 1.0)) / tmp0->object.n->nobject.real;
						break;
					case RATIONAL:
						tmp0->object.n->nobject.rational.num *= tmp1->object.n->nobject.rational.den;
						tmp0->object.n->nobject.rational.den *= tmp1->object.n->nobject.rational.num;
						break;
					case COMPLEX:
						ftmp = (NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0);
						ftmp *= ftmp;
						CEREAL(tmp0) = (CEREAL(tmp0) * (NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0)) / ftmp;
						IMMAG(tmp0) = (IMMAG(tmp0) * (NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0)) / ftmp;
						break;
				}
			case COMPLEX:
				switch(NTYPE(tmp0))
				{
					case INTEGER:
						ftmp = (CEREAL(tmp1) * CEREAL(tmp1)) + (IMMAG(tmp1) * IMMAG(tmp1));
						f0 = tmp0->object.n->nobject.z * 1.0;
						CEREAL(tmp0) = (f0 * CEREAL(tmp1)) / ftmp;
						IMMAG(tmp0) = (f0 * IMMAG(tmp1)) / ftmp;
						NTYPE(tmp0) = COMPLEX;
						break;
					case REAL:
						ftmp = (CEREAL(tmp1) * CEREAL(tmp1)) + (IMMAG(tmp1) * IMMAG(tmp1));
						f0 = tmp0->object.n->nobject.real;
						CEREAL(tmp0) = (f0 * CEREAL(tmp1)) / ftmp;
						IMMAG(tmp0) = (f0 * IMMAG(tmp1)) / ftmp;
						NTYPE(tmp0) = COMPLEX;
						break;
					case RATIONAL:
						ftmp = (CEREAL(tmp1) * CEREAL(tmp1)) + (IMMAG(tmp1) * IMMAG(tmp1));
						f0 = (NUM(tmp0) * 1.0) / (DEN(tmp0) * 1.0);
						CEREAL(tmp0) = (f0 * CEREAL(tmp1)) / ftmp;
						IMMAG(tmp0) = (f0 * IMMAG(tmp1)) / ftmp;
						NTYPE(tmp0) = COMPLEX;
						break;
					case COMPLEX:
						/* (a + b) ÷ (c + d) -> [(ac + bd) ÷ (c^2 + d^2)] + [(bc - ad) ÷ (c ^ 2 + d ^ 2)] */
						f0 = (CEREAL(tmp1) * CEREAL(tmp1)) + (IMMAG(tmp1) * IMMAG(tmp1));
						ftmp = ((CEREAL(tmp0) * CEREAL(tmp1)) + (IMMAG(tmp0) * IMMAG(tmp1))) / f0;
						f1 = CEREAL(tmp0);
						f2 = IMMAG(tmp0);
						/*printf("%f %f %f\n",f0,f1,f2);
						printf("#C(%f %f)\n",CEREAL(tmp0), IMMAG(tmp0));
						printf("#C(%f %f)\n",CEREAL(tmp1), IMMAG(tmp1));*/
						IMMAG(tmp0) = ((IMMAG(tmp0) * CEREAL(tmp1)) - (CEREAL(tmp0) * IMMAG(tmp1))) / f0; 
						CEREAL(tmp0) = ftmp;
						/*printf("#C(%f %f) %f\n",CEREAL(tmp0), IMMAG(tmp0), ftmp);*/
				}
				break;
		}
		rst = cdr(rst);
	}
	if(tmp0->object.n->type == RATIONAL)
	{
		itmp = _igcd(tmp0->object.n->nobject.rational.num,tmp0->object.n->nobject.rational.den);
		if(itmp != 1 && itmp != 0)
		{
			tmp0->object.n->nobject.rational.num /= itmp;
			tmp0->object.n->nobject.rational.den /= itmp;
		}
	}
	return tmp0;
}
SExp *
freal_part(SExp *tmp1)
{
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"real-part operates on numbers alone...");
	switch(NTYPE(tmp1))
	{
		case INTEGER:
		case REAL:
		case RATIONAL:
			return tmp1;
		case COMPLEX:
			return makereal(CEREAL(tmp1));
	}
}
SExp *
fimag_part(SExp *tmp1)
{
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"real-part operates on numbers alone...");
	switch(NTYPE(tmp1))
	{
		case INTEGER:
		case REAL:
		case RATIONAL:
			return tmp1;
		case COMPLEX:
			return makereal(IMMAG(tmp1));
	}	
}
SExp *
fmake_rect(SExp *tmp0, SExp *tmp1)
{
	SExp *tmp2 = nil;
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) == COMPLEX))
		return makeerror(1,0,"make-rectangular accepts only real arguments");
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) == COMPLEX))
		return makeerror(1,0,"make-rectangular accepts only real arguments");
	tmp2 = makenumber(INTEGER);
	NTYPE(tmp2) = COMPLEX;
	switch(NTYPE(tmp0))
	{
		case INTEGER:
			CEREAL(tmp2) = tmp0->object.n->nobject.z * 1.0;
			break;
		case REAL:
			CEREAL(tmp2) = tmp0->object.n->nobject.real;
			break;
		case RATIONAL:
			CEREAL(tmp2) = (NUM(tmp0) * 1.0) / (DEN(tmp0) / 1.0);
			break;
        default: // should never get here, with type check above to disallow COMPLEX
            break;
	}
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			IMMAG(tmp2) = tmp1->object.n->nobject.z * 1.0;
			break;
		case REAL:
			IMMAG(tmp2) = tmp1->object.n->nobject.real;
			break;
		case RATIONAL:
			IMMAG(tmp2) = (NUM(tmp1) * 1.0) / (DEN(tmp1) / 1.0);
			break;
        default:
            break;
	}
	return tmp2;
}
SExp *
fmake_pole(SExp *tmp0, SExp *tmp1)
{
	SExp *tmp2 = nil;
	double f0 = 0.0, f1 = 0.0;
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) == COMPLEX))
		return makeerror(1,0,"make-polar accepts only real arguments");
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) == COMPLEX))
		return makeerror(1,0,"make-polar accepts only real arguments");
	tmp2 = makenumber(INTEGER);
	NTYPE(tmp2) = COMPLEX;
	switch(NTYPE(tmp0))
	{
		case INTEGER:
			f0 = tmp0->object.n->nobject.z * 1.0 ;
			break;
		case REAL:
			f0 = tmp0->object.n->nobject.real;
			break;
		case RATIONAL:
			f0 = (NUM(tmp0) * 1.0) / (DEN(tmp0) / 1.0);
			break;
        default: // should never get here, COMPLEX type check above
            break;
	}
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			f1 = tmp1->object.n->nobject.z * 1.0;
			break;
		case REAL:
			f1 = tmp1->object.n->nobject.real;
			break;
		case RATIONAL:
			f1 = (NUM(tmp1) * 1.0) / (DEN(tmp1) / 1.0);
			break;
        default:
            break;
	}
	CEREAL(tmp2) = f0 * cos(f1);
	IMMAG(tmp2) = f0 * sin(f1);
	return tmp2;
}
SExp *
fconjugate(SExp *tmp0)
{
	SExp *tmp1 = nil;
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) != COMPLEX))
		return makeerror(1,0,"conjugate accepts only complex numbers");
	tmp1 = makenumber(INTEGER);
	NTYPE(tmp1) = COMPLEX;
	CEREAL(tmp1) = CEREAL(tmp0);
	IMMAG(tmp1) = - IMMAG(tmp0);
	return tmp1;
}
SExp *
fconjugate_bang(SExp *tmp0)
{
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) != COMPLEX))
		return makeerror(1,0,"conjugate! accepts only complex numbers");
	IMMAG(tmp0) = - IMMAG(tmp0);
	return tmp0;
}
SExp *
fpol2rect(SExp *tmp0)
{
	SExp *tmp1 = nil;
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) != COMPLEX))
		return makeerror(1,0,"polar->rectangular's sole argument *must* be a complex number in polar format");
	tmp1 = makenumber(COMPLEX);
	CEREAL(tmp1) = (CEREAL(tmp0) * cosf(IMMAG(tmp0)));
	IMMAG(tmp1) = (CEREAL(tmp0) * sinf(IMMAG(tmp0)));
	return tmp1;
}
SExp *
frect2pol(SExp *tmp0)
{
	double f0 = 0.0, f1 = 0.0;
	SExp *tmp1 = nil;
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) != COMPLEX))
		return makeerror(1,0,"rectangular->polar's sole argument *must* be a complex number in polar format");
	tmp1 = makenumber(COMPLEX);
	f0 = CEREAL(tmp0);
	f0 *= f0;
	f1 = IMMAG(tmp0);
	f1 *= f1;
	CEREAL(tmp1) = sqrtf(f0 + f1);
	f0 = CEREAL(tmp0);
	f1 = IMMAG(tmp0);
	IMMAG(tmp1) = atanf(f1 / f0);
	return tmp1;
}
SExp *
fgcd(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil;
	int itmp = 0;
	if(pairlength(rst) <= 1)
		return makeerror(1,0,"gcd expects 2 or more arguments... ");
	tmp0 = makenumber(INTEGER);
	tmp1 = car(rst);
	rst = cdr(rst);
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && (tmp1->object.n->type != INTEGER && tmp1->object.n->type != RATIONAL)))
		return makeerror(1,0,"gcd only operates on rationals...");
	switch(tmp1->object.n->type)
	{
		case INTEGER:
			tmp0->object.n->nobject.z = tmp1->object.n->nobject.z;
			break;
		case RATIONAL:
			tmp0->object.n->nobject.rational.num = tmp1->object.n->nobject.rational.num;
			tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.den;
			tmp0->object.n->type = RATIONAL;
			break;
        default:
            break;
	}	
	while(rst != snil)
	{
		tmp1 = car(rst);
		if(tmp1->type != NUMBER || (tmp1->type == NUMBER && (tmp1->object.n->type != INTEGER && tmp1->object.n->type != RATIONAL)))
			return makeerror(1,0,"gcd only operates on rationals...");
		switch(tmp1->object.n->type)
		{
			case INTEGER:
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						tmp0->object.n->nobject.z = _igcd(tmp0->object.n->nobject.z, tmp1->object.n->nobject.z);
						break;
					case RATIONAL:
						tmp0->object.n->nobject.rational.num = _igcd(tmp0->object.n->nobject.rational.num, tmp1->object.n->nobject.z);
						itmp = tmp0->object.n->nobject.rational.den;
						tmp0->object.n->nobject.rational.den = (itmp / _igcd(1,itmp));
						break;
                    default:
                        break;
				}
				break;
			case RATIONAL:
				switch(tmp0->object.n->type)
				{
					case INTEGER: /* convert tmp0 to rational, fall through */
						tmp0->object.n->nobject.rational.num = tmp0->object.n->nobject.z;
						tmp0->object.n->nobject.rational.den = 1;
						tmp0->object.n->type = RATIONAL;
					case RATIONAL:
						tmp0->object.n->nobject.rational.num = _igcd(tmp0->object.n->nobject.rational.num,tmp1->object.n->nobject.rational.num);
						itmp = tmp1->object.n->nobject.rational.den;
						tmp0->object.n->nobject.rational.den = (itmp / _igcd(itmp,tmp0->object.n->nobject.rational.den)) * tmp0->object.n->nobject.rational.den;
						break;
                    default:
                        break;
				}
            default:
                break;
		}
		rst = cdr(rst);
	}
	return tmp0;
}
SExp *
flcm(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil;
	int itmp = 0;
	/* instead of having a dedicated lcm function, I use the fact that
	 * lcm(a,b) := (|a| / gcd(a,b)) * |b| ;
	 */
	if(pairlength(rst) <= 1)
		return makeerror(1,0,"gcd expects 2 or more arguments... ");
	tmp0 = makenumber(INTEGER);
	tmp1 = car(rst);
	rst = cdr(rst);
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && (tmp1->object.n->type != INTEGER && tmp1->object.n->type != RATIONAL)))
		return makeerror(1,0,"lcm only operates on rationals...");
	switch(tmp1->object.n->type)
	{
		case INTEGER:
			tmp0->object.n->nobject.z = tmp1->object.n->nobject.z;
			break;
		case RATIONAL:
			tmp0->object.n->nobject.rational.num = tmp1->object.n->nobject.rational.num;
			tmp0->object.n->nobject.rational.den = tmp1->object.n->nobject.rational.den;
			tmp0->object.n->type = RATIONAL;
			break;
        default:
            break;
	}	
	while(rst != snil)
	{
		tmp1 = car(rst);
		if(tmp1->type != NUMBER || (tmp1->type == NUMBER && (tmp1->object.n->type != INTEGER && tmp1->object.n->type != RATIONAL)))
			return makeerror(1,0,"lcm only operates on rationals...");
		switch(tmp1->object.n->type)
		{
			case INTEGER:
				switch(tmp0->object.n->type)
				{
					case INTEGER:
						tmp0->object.n->nobject.z = (tmp0->object.n->nobject.z / _igcd(tmp0->object.n->nobject.z, tmp1->object.n->nobject.z)) * tmp1->object.n->nobject.z;
						break;
					case RATIONAL:
						tmp0->object.n->nobject.rational.num = (tmp0->object.n->nobject.rational.num / _igcd(tmp1->object.n->nobject.z,tmp0->object.n->nobject.rational.num)) * tmp1->object.n->nobject.z;
						tmp0->object.n->nobject.rational.den = _igcd(1,tmp0->object.n->nobject.rational.den);
						break;
                    default:
                        break;
				}
				break;
			case RATIONAL:
				switch(tmp0->object.n->type)
				{
					case INTEGER: /* convert tmp0 to rational, fall through */
						tmp0->object.n->nobject.rational.num = tmp0->object.n->nobject.z;
						tmp0->object.n->nobject.rational.den = 1;
						tmp0->object.n->type = RATIONAL;
					case RATIONAL:
						tmp0->object.n->nobject.rational.den = _igcd(tmp0->object.n->nobject.rational.den,tmp1->object.n->nobject.rational.den);
						itmp = tmp1->object.n->nobject.rational.num;
						tmp0->object.n->nobject.rational.num = (itmp / _igcd(itmp,tmp0->object.n->nobject.rational.num)) * tmp0->object.n->nobject.rational.num;
						break;
                    default:
                        break;
				}
            default:
                break;
		}
		rst = cdr(rst);
	}
	return tmp0;
}
SExp *
fquotient(SExp *tmp, SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp->type != NUMBER || (tmp->type == NUMBER && NTYPE(tmp) == COMPLEX))
		return makeerror(1,0,"quotient (x0 : NUMBER) (x1 : NUMBER) => NUMBER ; neither x0 nor x1 may be complex");
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) == COMPLEX))
		return makeerror(1,0,"quotient (x0 : NUMBER) (x1 : NUMBER) => NUMBER ; neither x0 nor x1 may be complex");
	tmp0 = makenumber(INTEGER);
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			switch(NTYPE(tmp))
			{
				case INTEGER:
					tmp0->object.n->nobject.z = tmp->object.n->nobject.z / tmp1->object.n->nobject.z;
					return tmp0;
				case RATIONAL:
					return tmp0;
                default:
                    break;
			}
		case RATIONAL:
			return tmp0;
		case REAL:
			return tmp0;
        default:
            break;
	}
	return tmp0;
}
SExp *
fmodulo(SExp *tmp, SExp *tmp1)
{
	int itmp = 0;
	SExp *tmp0 = nil;
	if(tmp->type != NUMBER || (tmp->type == NUMBER && NTYPE(tmp) == COMPLEX))
		return makeerror(1,0,"modulo (x0 : NUMBER) (x1 : NUMBER) => NUMBER ; note: x0 nor x1 may be complex");
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) == COMPLEX))
		return makeerror(1,0,"modulo (x0 : NUMBER) (x1 : NUMBER) => NUMBER ; note: x0 nor x1 may be complex");
	tmp0 = makenumber(INTEGER);
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			switch(NTYPE(tmp))
			{
				case INTEGER:
					tmp0->object.n->nobject.z = tmp->object.n->nobject.z % tmp1->object.n->nobject.z;
					return tmp0;
				case RATIONAL:
					itmp = (DEN(tmp0) / _igcd(1,DEN(tmp0)));
					printf("Floating after this?\n");
					NUM(tmp0) = (itmp * (NUM(tmp0) / DEN(tmp0))) % (itmp * tmp1->object.n->nobject.z);
					DEN(tmp0) = itmp;
					return tmp0;
                default:
                    break;
			}
		case RATIONAL:
			return tmp0;
        default:
            break;
	}
	return tmp0;
}
SExp *
fremainder(SExp *tmp, SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp->type != NUMBER || (tmp->type == NUMBER && NTYPE(tmp) == COMPLEX))
		return makeerror(1,0,"remainder (x0 : NUMBER) (x1 : NUMBER) => NUMBER ; note: x0 nor x1 may be complex");
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) == COMPLEX))
		return makeerror(1,0,"remainder (x0 : NUMBER) (x1 : NUMBER) => NUMBER ; note: x0 nor x1 may be complex");
	tmp0 = makenumber(INTEGER);
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			switch(NTYPE(tmp))
			{
				case INTEGER:
					tmp0->object.n->nobject.z = tmp->object.n->nobject.z % tmp1->object.n->nobject.z;
					return tmp0;
				case RATIONAL:
					return tmp0;
                default:
                    break;
			}
        default:
            break;
	}
	return tmp0;
}
SExp *
fsin(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"sin operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = sin(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = sin(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = sin((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fcos(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"cos operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = cos(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = cos(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = cos((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
ftan(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"tan operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = tan(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = tan(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = tan((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fasin(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"asin operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = asin(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = asin(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = asin((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
facos(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"acos operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = acos(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = acos(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = acos((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fatan(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"atan operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = atan(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = atan(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = atan((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fatan2(SExp *tmp1, SExp *tmp2)
{
	SExp *tmp0 = nil;
	double f0 = 0.0, f1 = 0.0;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"atan2 operates only on numbers...");
	if(tmp2->type != NUMBER)
		return makeerror(1,0,"atan2 operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			f0 = tmp1->object.n->nobject.z * 1.0;
			break;
		case REAL:
			f0 = tmp1->object.n->nobject.real;
			break;
		case RATIONAL:
			f0 = (NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0);
			break;
		case COMPLEX:
			break;
	}
	switch(NTYPE(tmp2))
	{
		case INTEGER:
			f1 = tmp2->object.n->nobject.z * 1.0;
			break;
		case REAL:
			f1 = tmp2->object.n->nobject.real;
			break;
		case RATIONAL:
			f1 = (NUM(tmp2) * 1.0) / (DEN(tmp2) * 1.0);
			break;
		case COMPLEX:
			break;
	}
	tmp0->object.n->nobject.real = atan2(f0,f1);
	return tmp0;
}
SExp *
fcosh(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"cosh operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = cosh(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = cosh(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = cosh((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fsinh(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"sinh operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = sinh(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = sinh(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = sinh((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
ftanh(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"tanh operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = tanh(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = tanh(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = tanh((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fexp(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"exp operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = exp(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = exp(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = exp((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fexp2(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"exp2 operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER: /* this should return an exact... */
			NTYPE(tmp0) = INTEGER;
			tmp0->object.n->nobject.z = 1 << tmp1->object.n->nobject.z;
			//tmp0->object.n->nobject.real = exp2f(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = exp2(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = exp2((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fexpm1(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"expm1 operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = expm1(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = expm1(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = expm1((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fln(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"ln operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = log(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = log(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = log((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
flog2(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"log2 operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = log2(tmp1->object.n->nobject.z * 1.0l);
			break;
		case REAL:
			tmp0->object.n->nobject.real = log2(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = log2((NUM(tmp1) * 1.0l) / (DEN(tmp1) * 1.0l));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
flog10(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"log10 operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	NTYPE(tmp0) = REAL;
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.real = log10(tmp1->object.n->nobject.z * 1.0);
			break;
		case REAL:
			tmp0->object.n->nobject.real = log10(tmp1->object.n->nobject.real);
			break;
		case RATIONAL:
			tmp0->object.n->nobject.real = log10((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0));
			break;
		case COMPLEX:
			break;
	}
	return tmp0;
}
SExp *
fnabs(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"abs operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.z = tmp1->object.n->nobject.z < 0 ? - tmp1->object.n->nobject.z : tmp1->object.n->nobject.z;
			NTYPE(tmp0) = INTEGER;
			break;
		case REAL:
			tmp0->object.n->nobject.real = tmp1->object.n->nobject.real < 0 ? - tmp1->object.n->nobject.real : tmp1->object.n->nobject.real;
			NTYPE(tmp0) = REAL;
			break;
		case RATIONAL:
			NUM(tmp0) = NUM(tmp1) < 0 ? - NUM(tmp1) : NUM(tmp1);
			DEN(tmp0) = DEN(tmp1) < 0 ? - DEN(tmp1) : DEN(tmp1);
			NTYPE(tmp0) = RATIONAL;
			break;
		case COMPLEX: /* should this do something? magnitude is different... */
			return tmp1;
	}
	return tmp0;
}
SExp *
fmag(SExp *tmp1)
{
	SExp *tmp0 = nil;
	double f0 = 0.0, f1 = 0.0;
	if(tmp1->type != NUMBER)
		return makeerror(1,0,"abs operates only on numbers...");
	tmp0 = makenumber(INTEGER);
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			tmp0->object.n->nobject.z = tmp1->object.n->nobject.z < 0 ? - tmp1->object.n->nobject.z : tmp1->object.n->nobject.z;
			NTYPE(tmp0) = INTEGER;
			break;
		case REAL:
			tmp0->object.n->nobject.real = tmp1->object.n->nobject.real < 0 ? - tmp1->object.n->nobject.real : tmp1->object.n->nobject.real;
			NTYPE(tmp0) = REAL;
			break;
		case RATIONAL:
			NUM(tmp0) = NUM(tmp1) < 0 ? - NUM(tmp1) : NUM(tmp1);
			DEN(tmp0) = DEN(tmp1) < 0 ? - DEN(tmp1) : DEN(tmp1);
			NTYPE(tmp0) = RATIONAL;
			break;
		case COMPLEX:
			NTYPE(tmp0) = REAL;
			f0 = CEREAL(tmp1);
			f0 *= f0;
			f1 = IMMAG(tmp1);
			f1 *= f1;
			tmp0->object.n->nobject.real = sqrtf(f0 + f1);
			break;
	}
	return tmp0;
}
SExp *
flt_in(int i, SExp *n)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"< operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            if(i < AINT(n))
                return strue;
            break;
        case REAL:
            if(i < AREAL(n))
                return strue;
            break;
        case RATIONAL:
            if(i < (NUM(n) / DEN(n)))
                return strue;
            break;
        case COMPLEX:
            if(IMMAG(n) != 0)
                return sfalse;
            if(i < CEREAL(n))
                return strue;
            break;
    }
    return sfalse;
}
SExp *
flt_ni(SExp *n, int i)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"< operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            if(AINT(n) < i)
                return strue;
            break;
        case REAL:
            if(AREAL(n) < i)
                return strue;
            break;
        case RATIONAL:
            if(i < (NUM(n) / DEN(n)))
                return strue;
            break;
        case COMPLEX:
            if(IMMAG(n) != 0)
                return sfalse;
            if(CEREAL(n) < i)
                return strue;
            break;
    }
    return sfalse;
}
SExp *
fgt_ni(SExp *n, int i)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"> operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            if(AINT(n) > i)
                return strue;
            break;
        case REAL:
            if(AREAL(n) > i)
                return strue;
            break;
        case RATIONAL:
            if(i > (NUM(n) / DEN(n)))
                return strue;
            break;
        case COMPLEX:
            if(IMMAG(n) != 0)
                return sfalse;
            if(CEREAL(n) > i)
                return strue;
            break;
    }
    return sfalse;
}
SExp *
fnumeq_ni(SExp *n, int i)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"= operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            if(AINT(n) == i)
                return strue;
            break;
        case REAL:
            if(AREAL(n) == i)
                return strue;
            break;
        case RATIONAL:
            if(i == (NUM(n) / DEN(n)))
                return strue;
            break;
        case COMPLEX:
            if(IMMAG(n) != 0)
                return sfalse;
            if(CEREAL(n) == i)
                return strue;
            break;
    }
    return sfalse;
}
SExp *
fnumeq_nn(SExp *n, SExp *p)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"= operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AINT(n) == AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(AINT(n) == AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AINT(n) == (NUM(p) / DEN(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AINT(n) == CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case REAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AREAL(n) == (AINT(p) * 1.0))
                        return strue;
                    break;
                case REAL:
                    if(AREAL(n) == AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AREAL(n) == ((NUM(p) / DEN(p)) * 1.0))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AREAL(n) == CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case RATIONAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if((NUM(n) / DEN(p)) == AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(((NUM(n) / DEN(p)) * 1.0) == AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    // WRONG! but quick for now :D
                    if((NUM(n) == NUM(p)) && (NUM(n) == NUM(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(((NUM(n) / DEN(p)) * 1.0) == CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case COMPLEX:
            if(IMMAG(n) != 0.0 && NTYPE(p) != COMPLEX)
                break;
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(CEREAL(n) == (1.0 * AINT(p)))
                        return strue;
                    break;
                case REAL:
                    if(CEREAL(n) == AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(CEREAL(n) == (1.0 * (NUM(p) / DEN(p))))
                        return strue;
                    break;
                case COMPLEX:
                    if(CEREAL(n) == CEREAL(p) && IMMAG(n) == IMMAG(p))
                        return strue;
                    break;
            }
            break;
    }
    return sfalse;
}
SExp *
fgte_nn(SExp *n, SExp *p)
{
    if(n->type != NUMBER)
        return makeerror(1,0,">= operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AINT(n) >= AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(AINT(n) >= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AINT(n) >= (NUM(p) / DEN(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AINT(n) >= CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case REAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AREAL(n) >= (AINT(p) * 1.0))
                        return strue;
                    break;
                case REAL:
                    if(AREAL(n) >= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AREAL(n) >= ((NUM(p) / DEN(p)) * 1.0))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AREAL(n) >= CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case RATIONAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if((NUM(n) / DEN(p)) >= AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(((NUM(n) / DEN(p)) * 1.0) >= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    // WRONG! but quick for now :D
                    if((NUM(n) >= NUM(p)) && (NUM(n) >= NUM(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(((NUM(n) / DEN(p)) * 1.0) >= CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case COMPLEX:
            if(IMMAG(n) != 0.0 && NTYPE(p) != COMPLEX)
                break;
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(CEREAL(n) >= (1.0 * AINT(p)))
                        return strue;
                    break;
                case REAL:
                    if(CEREAL(n) >= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(CEREAL(n) >= (1.0 * (NUM(p) / DEN(p))))
                        return strue;
                    break;
                case COMPLEX:
                    if(CEREAL(n) >= CEREAL(p) && IMMAG(n) >= IMMAG(p))
                        return strue;
                    break;
            }
            break;
    }
    return sfalse;
}
SExp *
fgt_nn(SExp *n, SExp *p)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"> operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AINT(n) > AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(AINT(n) > AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AINT(n) > (NUM(p) / DEN(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AINT(n) > CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case REAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AREAL(n) > (AINT(p) * 1.0))
                        return strue;
                    break;
                case REAL:
                    if(AREAL(n) > AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AREAL(n) > ((NUM(p) / DEN(p)) * 1.0))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AREAL(n) > CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case RATIONAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if((NUM(n) / DEN(p)) > AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(((NUM(n) / DEN(p)) * 1.0) > AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    // WRONG! but quick for now :D
                    if((NUM(n) > NUM(p)) && (NUM(n) > NUM(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(((NUM(n) / DEN(p)) * 1.0) > CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case COMPLEX:
            if(IMMAG(n) != 0.0 && NTYPE(p) != COMPLEX)
                break;
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(CEREAL(n) > (1.0 * AINT(p)))
                        return strue;
                    break;
                case REAL:
                    if(CEREAL(n) > AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(CEREAL(n) > (1.0 * (NUM(p) / DEN(p))))
                        return strue;
                    break;
                case COMPLEX:
                    if(CEREAL(n) > CEREAL(p) && IMMAG(n) > IMMAG(p))
                        return strue;
                    break;
            }
            break;
    }
    return sfalse;
}
SExp *
flte_nn(SExp *n, SExp *p)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"<= operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AINT(n) <= AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(AINT(n) <= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AINT(n) <= (NUM(p) / DEN(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AINT(n) <= CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case REAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AREAL(n) <= (AINT(p) * 1.0))
                        return strue;
                    break;
                case REAL:
                    if(AREAL(n) <= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AREAL(n) <= ((NUM(p) / DEN(p)) * 1.0))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AREAL(n) <= CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case RATIONAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if((NUM(n) / DEN(p)) <= AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(((NUM(n) / DEN(p)) * 1.0) <= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    // WRONG! but quick for now :D
                    if((NUM(n) <= NUM(p)) && (NUM(n) <= NUM(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(((NUM(n) / DEN(p)) * 1.0) <= CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case COMPLEX:
            if(IMMAG(n) != 0.0 && NTYPE(p) != COMPLEX)
                break;
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(CEREAL(n) <= (1.0 * AINT(p)))
                        return strue;
                    break;
                case REAL:
                    if(CEREAL(n) <= AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(CEREAL(n) <= (1.0 * (NUM(p) / DEN(p))))
                        return strue;
                    break;
                case COMPLEX:
                    if(CEREAL(n) <= CEREAL(p) && IMMAG(n) <= IMMAG(p))
                        return strue;
                    break;
            }
            break;
    }
    return sfalse;
}
SExp *
flt_nn(SExp *n, SExp *p)
{
    if(n->type != NUMBER)
        return makeerror(1,0,"< operates on numbers only...");
    switch(n->object.n->type)
    {
        case INTEGER:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AINT(n) < AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(AINT(n) < AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AINT(n) < (NUM(p) / DEN(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AINT(n) < CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case REAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(AREAL(n) < (AINT(p) * 1.0))
                        return strue;
                    break;
                case REAL:
                    if(AREAL(n) < AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(AREAL(n) < ((NUM(p) / DEN(p)) * 1.0))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(AREAL(n) < CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case RATIONAL:
            switch(NTYPE(p))
            {
                case INTEGER:
                    if((NUM(n) / DEN(p)) < AINT(p))
                        return strue;
                    break;
                case REAL:
                    if(((NUM(n) / DEN(p)) * 1.0) < AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    // WRONG! but quick for now :D
                    if((NUM(n) < NUM(p)) && (NUM(n) < NUM(p)))
                        return strue;
                    break;
                case COMPLEX:
                    if(IMMAG(p) != 0)
                        return sfalse;
                    if(((NUM(n) / DEN(p)) * 1.0) < CEREAL(p))
                        return strue;
                    break;
            }
            break;
        case COMPLEX:
            if(IMMAG(n) != 0.0 && NTYPE(p) != COMPLEX)
                break;
            switch(NTYPE(p))
            {
                case INTEGER:
                    if(CEREAL(n) < (1.0 * AINT(p)))
                        return strue;
                    break;
                case REAL:
                    if(CEREAL(n) < AREAL(p))
                        return strue;
                    break;
                case RATIONAL:
                    if(CEREAL(n) < (1.0 * (NUM(p) / DEN(p))))
                        return strue;
                    break;
                case COMPLEX:
                    if(CEREAL(n) < CEREAL(p) && IMMAG(n) < IMMAG(p))
                        return strue;
                    break;
            }
            break;
    }
    return sfalse;
}
/* collection functions */
SExp *
ffirst(SExp *tmp0)
{
	switch(tmp0->type)
	{
		case VECTOR:
			if(tmp0->length < 1)
				return snil;
			return tmp0->object.vec[0];
		case STRING:
			if(tmp0->length >= 1)
				return makechar(tmp0->object.str[0]);
			return snil;
		case PAIR:
			if(tmp0 != snil)
				return car(tmp0);
			return snil;
		case DICT:
			/* nothing for now... */
			return snil;
		default:
			return makeerror(1,0,"first operates on collections only...");
	}
}
SExp *
frest(SExp *tmp0)
{
	int itmp = 0, iter = 0;
	SExp *tmp1 = nil;
	switch(tmp0->type)
	{
		case VECTOR:
			tmp1 = makevector(tmp0->length - 1,snil);
			itmp = tmp0->length;
			for(iter = 1;iter < itmp;iter++)
				tmp1->object.vec[iter - 1] = tmp0->object.vec[iter];
			return tmp1;
		case STRING:
			return makestring(&tmp0->object.str[1]);
		case PAIR:
			return cdr(tmp0);
		case DICT:
			return snil;
		default:
			return makeerror(1,0,"rest operates on collections only...");
	}
}
SExp *
fnth(SExp *tmp0, SExp *tmp1, SExp *dvalue)
{
	int iter = 0, itmp = 0;
	SExp *tmp2 = snil;
    char errbuf[512] = {0};
	if(tmp0->type != DICT)
	{
		if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
			return makeerror(1,0,"nth: idx must be an INTEGER");
	}
	else
		if(tmp1->type != STRING && tmp1->type != KEY && tmp1->type != ATOM)
			return makeerror(1,0,"nth: idx for dicts must be bound to (STRING | KEYOBJ | SYMBOL)");
    
	switch(tmp0->type)
	{
		case VECTOR:
			if(tmp1->object.n->nobject.z >= tmp0->length)
            {
                if(dvalue != nil)
                    return dvalue;            
				return makeerror(1,0,"nth: index out of range");
            }
			return tmp0->object.vec[tmp1->object.n->nobject.z];
		case STRING:
			if(tmp1->object.n->nobject.z >= tmp0->length)
            {
                if(dvalue != nil)
                    return dvalue;
				return makeerror(1,0,"nth: index out of range");
            }
			return makechar(tmp0->object.str[tmp1->object.n->nobject.z]);
		case PAIR:
			itmp = pairlength(tmp0);
			if(tmp1->object.n->nobject.z > itmp)
            {
                if(dvalue != nil)
                    return dvalue;
				return makeerror(1,0,"nth: index out of range");
            }
			for(iter = 0; tmp0 != snil; iter++)
			{
				if(iter == tmp1->object.n->nobject.z)
					return car(tmp0);
				tmp0 = cdr(tmp0);
			}
			return snil;
		case DICT:
			tmp2 = trie_get(tmp1->object.str,tmp0->object.dict);
			if(tmp2 == nil)
            {
                //Aprintf("key == %s\n",tmp1->object.str);
                if(dvalue != nil)
                    return dvalue;
                snprintf(errbuf,512,"No such key: %s", tmp1->object.str);
				return makeerror(1,0,errbuf);
            }
			return tmp2;
		default: 
			return makeerror(1,0,"nth operates on collections only...");
	}
}
SExp *
fcset(SExp *col, SExp *offset, SExp *new)
{
	int iter = 0, itmp = 0;
	SExp *tmp = nil;
	switch(col->type)
	{
		case STRING:
			if(offset->type != NUMBER || (offset->type == NUMBER && NTYPE(offset) != INTEGER))
				return makeerror(1,0,"cset!: strings are indexed by FIXNUM integers only.");
			if(AINT(offset) > col->length)
				return makeerror(1,0,"cset!: string index out of range.");
			if(new->type != CHAR)
				return makeerror(1,0,"cset!: strings may only be updated with characters.");
			col->object.str[AINT(offset)] = new->object.c;
			break;
		case VECTOR:
			if(offset->type != NUMBER || (offset->type == NUMBER && NTYPE(offset) != INTEGER))
				return makeerror(1,0,"cset!: vectors are indexed by FIXNUM integers only.");
			if(AINT(offset) > col->length)
				return makeerror(1,0,"cset!: vector index out of range.");
			col->object.vec[AINT(offset)] = new;
			break;
		case DICT:
			if(offset->type != STRING && offset->type != ATOM && offset->type != KEY)
				return makeerror(1,0,"cset!: dictionary keys must be (STRING | ATOM | KEY)");
			trie_put(offset->object.str,new,col->object.dict);
			break;
		case PAIR:
			tmp = col;
			itmp = pairlength(tmp);
			if(offset->type != NUMBER || (offset->type == NUMBER && NTYPE(offset) != INTEGER))
				return makeerror(1,0,"cset!: pairs are indexed by FIXNUM integers only.");
			if(AINT(offset) > itmp)
				return makeerror(1,0,"cset!: pair index out of range");
			for(itmp = AINT(offset);iter < itmp;iter++)
				tmp = cdr(tmp);
			mcar(tmp) = new;
			break;
        default:
            return makeerror(1,0,"cset! operates on COLLECTIONS only!");
	}
	return svoid;
}
SExp *
fccons(SExp *tmp0, SExp *tmp1)
{
	SExp *tmp2 = nil;
	int iter = 0, itmp = 0;
	switch(tmp1->type)
	{
		case VECTOR:
			tmp2 = makevector(tmp1->length + 1,snil);
			for(iter = 0;iter < tmp1->length; iter++)
				tmp2->object.vec[iter + 1] = tmp1->object.vec[iter];
			tmp2->object.vec[0] = tmp0;
			return tmp2;
		case STRING:
			if(tmp0->type != CHAR)
				return makeerror(1,0,"type clash: ccons may only add chars to strings...");
			tmp2 = makestring_v(tmp1->length + 1,' ');
			for(iter = 0; iter < tmp1->length; iter++)
				tmp2->object.str[iter + 1] = tmp1->object.str[iter];
			tmp2->object.str[0] = tmp0->object.c;
			return tmp2;
		case NIL:
		case PAIR:
			return cons(tmp0,tmp1);
		case DICT:
			return snil;
		default:
			return makeerror(1,0,"ccons operates on collections only...");
	}
}
SExp *
flength(SExp *tmp1)
{
	SExp *tmp0 = makenumber(INTEGER);
	if(tmp1->type == VECTOR || tmp1->type == STRING)
	{
		tmp0->object.n->nobject.z = tmp1->length;
		return tmp0;
	}
	else if(tmp1->type == PAIR)
	{
		tmp0->object.n->nobject.z = pairlength(tmp1);
		return tmp0;
	}
	else if(tmp1->type == NIL)
		return makeinteger(0);
	/* would it be meaningful to return 0 here?
	 * technically, scalars have a zero length...
	 */
	return makeerror(1,0,"length: type clash: length operates on STRING | VECTOR | PAIR");
}
SExp *
fempty(SExp *tmp0)
{
	Trie *hd = nil;
	if(tmp0->type == VECTOR || tmp0->type == STRING)
	{
		if(tmp0->length > 0)
			return sfalse;
		return strue;
	}
	else if(tmp0->type == PAIR)
		return sfalse;
	else if(tmp0 == snil)
		return strue;
	else if(tmp0->type == DICT)
	{
		hd = tmp0->object.dict;
		if(hd->n_cur == 0)
			return strue;
		return sfalse;
	}
	return sfalse;
}
SExp *
fcupdate(SExp *col, SExp *index, SExp *nuval)
{
	SExp *ret = nil;
	int i = 0;
	/* should start testing this in every subroutine... */
	if(col == nil)
		return nil;
	if(col->type == STRING)
	{
		if(index->type != NUMBER || (index->type == NUMBER && NTYPE(index) != INTEGER))
			return makeerror(1,0,"cupdate: index error: strings *must* be indexed by integers");
		if(AINT(index) > col->length)
			return makeerror(1,0,"cupdate: index out of range");
		if(nuval->type != CHAR)
			return makeerror(1,0,"cupdate: new-value error: the new vaule *must* be a character");
		ret = makestring(col->object.str);
		ret->object.str[AINT(index)] = nuval->object.c;
		return ret;
	}
	else if(col->type == VECTOR)
	{
		if(index->type != NUMBER || (index->type == NUMBER && NTYPE(index) != INTEGER))
			return makeerror(1,0,"cupdate: index error: strings *must* be indexed by integers");
		if(AINT(index) > col->length)
			return makeerror(1,0,"cupdate: index out of range");
		ret = makevector(col->length,nil);
		for(i = 0; i < col->length; i++)
		{
			if(i == AINT(index))
				ret->object.vec[i] = nuval;
			else
				ret->object.vec[i] = col->object.vec[i];
		}
		return ret;
	}
	else if(col->type == DICT)
	{
		return snil;
	}
	else if(col->type == PAIR)
	{
		return snil;
	}
	return makeerror(1,0,"cupdate: col *must* be bound to a collexion");
}
SExp *
fcslice(SExp *col, SExp *start, SExp *end)
{
	SExp *ret = snil,*tmp = snil;
	int i = 0, j = 0, base = 0;
	if(col->type != VECTOR && col->type != DICT && col->type != STRING && col->type != PAIR)
		return makeerror(1,0,"cslice's col argument *must* be bound to a collexion");
	if(start->type != NUMBER || (start->type == NUMBER && NTYPE(start) != INTEGER))
		return makeerror(1,0,"cslice's start argument *must* be bound to an integer");
	if(end->type != NUMBER || (end->type == NUMBER && NTYPE(end) != INTEGER))
		return makeerror(1,0,"cslice's end argument *must* be bound to an integer");
	switch(col->type)
	{
		case STRING:
			j = AINT(end);
			if(j < 0)
				j = col->length + j + 1;
			else if(j > col->length)
				return makeerror(1,0,"cslice's END parameter is longer than the collexion it is operating on");
			i = AINT(start);
			if(i < 0)
				i = col->length + i;
			/* should probably have it where if you say cslice(col,-1,4), it copies the 
			 * end & wraps to 4, but this seems to be an edge-case that won' be oft used...
			 */
			 if(i < 0)
			 	return makeerror(1,0,"cslice's start argument *must* be greater than 0");
			 ret = makestring_v((j - i),' ');
			 for(;i < j;i++, base++)
			 	ret->object.str[base] = col->object.str[i];
			 ret->object.str[base+1] = nul;
			 break;	
		case VECTOR:
			j = AINT(end);
			if(j < 0)
				j = col->length + j + 1;
			else if(j > col->length)
				return makeerror(1,0,"cslice's END parameter is longer than the collexion it is operating on");
			i = AINT(start);
			if(i < 0)
				i = col->length + i;
			/* should probably have it where if you say cslice(col,-1,4), it copies the 
			 * end & wraps to 4, but this seems to be an edge-case that won' be oft used...
			 */
			 if(i < 0)
			 	return makeerror(1,0,"cslice's start argument *must* be greater than 0");
			ret = makevector((j - i),snil);
			for(; i < j; i++, base++)
				ret->object.vec[base] = col->object.vec[i];
			break;
		case PAIR:
			i = AINT(start);
			j = AINT(end);
			if(j < 0)
				j = col->length + j + 1;
			else if(j > pairlength(col))
				return makeerror(1,0,"cslice's END parameter is longer than the collexion it is operating on");
            else if(j == pairlength(col) && i == j)
                return snil;
			if(i < 0)
				i = col->length + i;
			/* should probably have it where if you say cslice(col,-1,4), it copies the 
			 * end & wraps to 4, but this seems to be an edge-case that won' be oft used...
			 */
			 if(i < 0)
			 	return makeerror(1,0,"cslice's start argument *must* be greater than 0");
			ret = cons(snil,snil);
			tmp = ret;
			for(;i < j; i++, base++)
			{
				mcar(tmp) = fnth(col,makeinteger(i),nil);
				if(i < (j - 1))
				{
					mcdr(tmp) = cons(snil,snil);
					tmp = mcdr(tmp);
				}
			}
			break;
		case DICT:
			/* slice off based on list of keys? */
			break;
        default:
            return makeerror(1,0,"cslice operates on COLLECTIONS only");
	}
	return ret;
}
SExp *
fvector(SExp *rst)
{
	SExp *tmp0 = nil;
	int itmp = pairlength(rst), iter = 0;
	if(itmp < 1)
		return makevector(0,snil);
	tmp0 = makevector(itmp,nil);
	for(iter = 0; iter < itmp; iter++)
	{
		tmp0->object.vec[iter] = car(rst);
		rst = cdr(rst);
	}
	return tmp0;
}
SExp *
fmkvector(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil, *tmp2 = nil;
	int itmp = pairlength(rst);
	if(itmp < 1 || itmp > 2)
		return makeerror(1,0,"make-vector requires at least one argument (and at most two)");
	if(itmp == 1)
	{
		tmp1 = car(rst);
		if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
			return makeerror(1,0,"make-vector's first argument must be an integer...");
		tmp0 = makevector(tmp1->object.n->nobject.z,snil);
	}
	else if(itmp == 2)
	{
		tmp1 = car(rst);
		if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
			return makeerror(1,0,"make-vector's first argument must be an integer...");
		tmp2 = car(cdr(rst));
		if(tmp2->type == KEY && !strncasecmp(tmp2->object.str,"no-init",7))
			tmp0 = makevector(tmp1->object.n->nobject.z,nil); /* DO NOT INIT; pretty dangerous, but useful for "pure speed"*/
		else
			tmp0 = makevector(tmp1->object.n->nobject.z,tmp2);
	}
	return tmp0;
}
SExp *
fstringappend(SExp *rst)
{
	/* O(scary) */
	SExp *tmp0 = nil, *tmp1 = nil, *tmp2 = nil;
	int itmp = pairlength(rst), stmp = 0, iter = 0;
	if(itmp == 0)
		return makestring_v(0,' ');
	tmp0 = rst;
	itmp = 0;
	while(tmp0 != snil)
	{
		tmp1 = car(tmp0);
		if(tmp1->type != STRING)
			return makeerror(1,0,"string-append operates on STRINGs only!");
		itmp += tmp1->length;
		tmp0 = cdr(tmp0);
	}
	tmp0 = rst;
	tmp1 = makestring_v(itmp,nul); /* don't fill! */
	iter = 0;
	while(tmp0 != snil)
	{
		tmp2 = car(tmp0);
		stmp = iter;
		itmp = tmp2->length;
		for(;(iter - stmp) < itmp; iter++)
			tmp1->object.str[iter] = tmp2->object.str[iter - stmp];
		tmp0 = cdr(tmp0);
	}
	tmp1->object.str[iter] = nul;
	return tmp1;
}
SExp *
fdict(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil, *tmp2 = nil;
	tmp0 = makedict();
  	while(rst != snil)
  	{
  		tmp1 = car(rst);
  		if(tmp1->type != STRING && tmp1->type != ATOM && tmp1->type != KEY)
  			return makeerror(1,0,"keys to dict *must* be (STRING | SYMBOL | KEYOBJ)");
  		if(cdr(rst) != snil)
  			tmp2 = car(cdr(rst));
  		else
  			tmp2 = snil;
  		trie_put(tmp1->object.str,tmp2,tmp0->object.dict);
  		rst = cdr(cdr(rst));
  	}	
	return tmp0;
}
SExp *
fdicthas(SExp *tmp0, SExp *tmp1)
{
	SExp *tmp2 = nil;
	if(tmp0->type != DICT)
		return makeerror(1,0,"dict-has?'s d argument *must* be bound to a dictionary");
	if(tmp1->type != STRING && tmp1->type != KEY && tmp1->type != ATOM)
		return makeerror(1,0,"dict-has?'s k object *must* be bound to (KEYOBJ | STRING | ATOM)");
	tmp2 = trie_get(tmp1->object.str,tmp0->object.dict);
	if(tmp2 == nil)
		return sfalse;
	return strue;
}

SExp *
fkeys(SExp *tmp)
{
    if(tmp->type != DICT)
        return makeerror(1,0,"keys sole argument *must* be a dictionary");
    return mcar(trie_keys(tmp->object.dict,tconcify(snil)));
}

SExp *
fpartial_key(SExp *d, SExp *k)
{
    if(d->type != DICT)
        return makeerror(1,0,"partial-key?'s first argument *must* be a dictionary");
    if(k->type != STRING)
        return makeerror(1,0,"partial-key?'s second argument *must* be a string");
    return trie_partial(d->object.dict,k->object.str,0);
}

SExp *
fstring(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil;
	int itmp = pairlength(rst), iter = 0;
	if(itmp == 0)
		return makestring_v(0,nul);
	tmp0 = makestring_v(itmp,' ');
	while(rst != snil)
	{
		tmp1 = car(rst);
		if(tmp1->type != CHAR)
			return makeerror(1,0,"string (c* : CHARACTER) => string");
		tmp0->object.str[iter] = tmp1->object.c;
		iter++;
		rst = cdr(rst);
	}
	return tmp0;
}
SExp *
fmakestring(SExp *rst)
{
	SExp *tmp0 = nil, *tmp1 = nil;
	int itmp = pairlength(rst);
	switch(itmp)
	{
		case 1:
			tmp1 = car(rst);
			if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
				return makeerror(1,0,"makestring (length : INTEGER) [(fill : CHARACTER)] => string");
			return makestring_v(tmp1->object.n->nobject.z,' ');
		case 2:
			tmp1 = car(rst);
			if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
				return makeerror(1,0,"makestring (length : INTEGER) [(fill : CHARACTER)] => string");
			tmp0 = car(cdr(rst));
			if(tmp0->type != CHAR)
				return makeerror(1,0,"makestring (length : INTEGER) [(fill : CHARACTER)] => string");
			return makestring_v(tmp1->object.n->nobject.z,tmp0->object.c);
		default:
			return makeerror(1,0,"makestring (length : INTEGER) [(fill : CHARACTER)] => string");
	}
}
/* generic internal functions */
SExp *
fgensym(SExp *tmp0)
{
	SExp *tmp1 = nil;
	char *buf = nil;
	if(tmp0 == snil)
	{
		buf = (char *)hmalloc(sizeof(char) * 16);
		snprintf(buf,16,"g%d",gensymglobal);
	}
	else
	{
		/* couldn't this just as easily be a key or a string? */
		if(tmp0->type != ATOM && tmp0->type != STRING)
			return makeerror(1,0,"gensym [a : (ATOM | STRING)] => symbol");
		buf = (char *)hmalloc(sizeof(char) * (tmp0->length + 16));
		snprintf(buf,tmp0->length + 16,"%s%d",tmp0->object.str,gensymglobal);
	}
	gensymglobal++;
	tmp1 = (SExp *)hmalloc(sizeof(SExp));
	tmp1->object.str = buf;
	tmp1->type = ATOM;
	return tmp1;
}
SExp *
fbitand(SExp *tmp1, SExp *tmp2)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
		return makeerror(1,0,"& expects exactly two integer arguments");
	if(tmp2->type != NUMBER || (tmp2->type == NUMBER && NTYPE(tmp2) != INTEGER))
		return makeerror(1,0,"& expects exactly two integer arguments");
	tmp0 = makenumber(INTEGER);
	tmp0->object.n->nobject.z = tmp1->object.n->nobject.z & tmp2->object.n->nobject.z;
	return tmp0;
}
SExp *
fbitor(SExp *tmp1, SExp *tmp2)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
		return makeerror(1,0,"| expects exactly two integer arguments");
	if(tmp2->type != NUMBER || (tmp2->type == NUMBER && NTYPE(tmp2) != INTEGER))
		return makeerror(1,0,"& expects exactly two integer arguments");
	tmp0 = makenumber(INTEGER);
	tmp0->object.n->nobject.z = tmp1->object.n->nobject.z | tmp2->object.n->nobject.z;
	return tmp0;
}
SExp *fbitxor(SExp *tmp1, SExp *tmp2)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
		return makeerror(1,0,"& expects exactly two integer arguments");
	if(tmp2->type != NUMBER || (tmp2->type == NUMBER && NTYPE(tmp2) != INTEGER))
		return makeerror(1,0,"& expects exactly two integer arguments");
	tmp0 = makenumber(INTEGER);
	tmp0->object.n->nobject.z = tmp1->object.n->nobject.z ^ tmp2->object.n->nobject.z;
	return tmp0;
}
SExp *
fbitnot(SExp *tmp1)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
		return makeerror(1,0,"& expects exactly one integer arguments");
	tmp0 = makenumber(INTEGER);
	tmp0->object.n->nobject.z = ~ tmp1->object.n->nobject.z;
	return tmp0;
}
SExp *
fbitshl(SExp *tmp1, SExp *tmp2)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
		return makeerror(1,0,"<< expects exactly two integer arguments");
	if(tmp2->type != NUMBER || (tmp2->type == NUMBER && NTYPE(tmp2) != INTEGER))
		return makeerror(1,0,"<< expects exactly two integer arguments");
	tmp0 = makenumber(INTEGER);
	tmp0->object.n->nobject.z = (unsigned int) tmp1->object.n->nobject.z << (unsigned int)tmp2->object.n->nobject.z;
	return tmp0;
}
SExp *
fbitshr(SExp *tmp1, SExp *tmp2)
{
	SExp *tmp0 = nil;
	if(tmp1->type != NUMBER || (tmp1->type == NUMBER && NTYPE(tmp1) != INTEGER))
		return makeerror(1,0,">> expects exactly two integer arguments");
	if(tmp2->type != NUMBER || (tmp2->type == NUMBER && NTYPE(tmp2) != INTEGER))
		return makeerror(1,0,">> expects exactly two integer arguments");
	tmp0 = makenumber(INTEGER);
	tmp0->object.n->nobject.z = (unsigned int)tmp1->object.n->nobject.z >> (unsigned int)tmp2->object.n->nobject.z;
	return tmp0;
}
SExp *
fceil(SExp *tmp0)
{
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) == COMPLEX))
		return makeerror(1,0,"ceil's r argument must be bound to a Number in the set or REALs");
	switch(NTYPE(tmp0))
	{
		case INTEGER:
			return tmp0;
		case REAL:
			return makereal(ceil(AREAL(tmp0)));
		case RATIONAL:
			return makeinteger(1);
        default:
            return makeerror(1,0,"ceil's argument must be REAL");
	}
}
SExp *
ffloor(SExp *tmp0)
{
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) == COMPLEX))
		return makeerror(1,0,"floor's r argument must be bound to a Number in the set or REALs");
	switch(NTYPE(tmp0))
	{
		case INTEGER:
			return tmp0;
		case REAL:
			return makereal(floor(AREAL(tmp0)));
		case RATIONAL:
			return makeinteger(0);
        default:
            return makeerror(1,0,"floor's argument must be REAL");
	}
}
SExp *
ftrunc(SExp *tmp0)
{
	double iptr = 0.0, val = 0.0, tmp = 0.0;
	if(tmp0->type != NUMBER)
		return makeerror(1,0,"inexact->exact's r argument must be bound to a Number in the set or REALs");
	if(NTYPE(tmp0) == REAL)
	{
		val = modf(AREAL(tmp0), &iptr);
		if(val < 0.5)
			return makeinteger((int)floor(AREAL(tmp0)));
		else
			return makeinteger((int)ceil(AREAL(tmp0)));
	}
	else if(NTYPE(tmp0) == RATIONAL)
	{
		tmp = (NUM(tmp0) * 1.0) / (DEN(tmp0) * 1.0);
		val = modf(tmp,&iptr);
		if(val < 0.5)
			return makeinteger((int)floor(tmp));
		else
			return makeinteger((int)ceil(tmp));	
	}
	else if(NTYPE(tmp0) == COMPLEX)
			return makeinteger(0);
	return tmp0;
}
SExp *fround(SExp *tmp0)
{
	double iptr = 0.0, val = 0.0, tmp = 0.0;
	if(tmp0->type != NUMBER)
		return makeerror(1,0,"inexact->exact's r argument must be bound to a Number in the set or REALs");
	if(NTYPE(tmp0) == REAL)
	{
		val = modf(AREAL(tmp0), &iptr);
		if(val < 0.5)
			return makereal(floor(AREAL(tmp0)));
		else
			return makereal(ceil(AREAL(tmp0)));
	}
	else if(NTYPE(tmp0) == RATIONAL)
	{
		tmp = (NUM(tmp0) * 1.0) / (DEN(tmp0) * 1.0);
		val = modf(tmp,&iptr);
		if(val < 0.5)
			return makereal(floor(tmp));
		else
			return makereal(ceil(tmp));	
	}
	else if(NTYPE(tmp0) == COMPLEX)
			return makereal(0);
	return tmp0;
}
SExp *
fin2ex(SExp *tmp0)
{
	double iptr = 0.0, val = 0.0;
	if(tmp0->type != NUMBER)
		return makeerror(1,0,"inexact->exact's r argument must be bound to a Number in the set or REALs");
	if(NTYPE(tmp0) == REAL)
	{
		val = modf(AREAL(tmp0), &iptr);
		if(val < 0.5)
			return makeinteger((int)floor(AREAL(tmp0)));
		else
			return makeinteger((int)ceil(AREAL(tmp0)));
	}
	else if(NTYPE(tmp0) == COMPLEX)
			return makeinteger(0);
	return tmp0;
}
SExp *
fexactp(SExp *tmp0)
{
	if(tmp0->type != NUMBER)
		return sfalse;
	if(tmp0->object.n->type == INTEGER || tmp0->object.n->type == RATIONAL)
		return strue;
	return sfalse;
}
SExp *
finexactp(SExp *tmp0)
{
	if(tmp0->type != NUMBER)
		return sfalse;
	if(tmp0->object.n->type == REAL || tmp0->object.n->type == COMPLEX)
		return strue;
	return sfalse;
}
SExp *
frealp(SExp *tmp)
{
	if(tmp->type != NUMBER)
		return sfalse;
	if(NTYPE(tmp) == REAL || NTYPE(tmp) == INTEGER || NTYPE(tmp) == RATIONAL)
		return strue;
	return sfalse;
}
SExp *
fcomplexp(SExp *tmp)
{
	if(tmp->type != NUMBER)
		return sfalse;
	if(NTYPE(tmp) == COMPLEX || NTYPE(tmp) == REAL || NTYPE(tmp) == INTEGER || NTYPE(tmp) == RATIONAL)
		return strue;
	return sfalse;
}
SExp *
frationalp(SExp *tmp)
{
	if(tmp->type != NUMBER)
		return sfalse;
	if(NTYPE(tmp) == INTEGER || NTYPE(tmp) == RATIONAL)
		return strue;
	return sfalse;
}
SExp *
fintegerp(SExp *tmp)
{
	if(tmp->type != NUMBER)
		return sfalse;
	if(NTYPE(tmp) == INTEGER)
		return strue;
	return sfalse;
}

SExp *
fmeta(SExp *o)
{
	SExp *ret = nil, *tmp = nil;
	if(o == nil || o == snil)
		return makeerror(1,0,"meta! o : SEXP [k : (STRING|KEYBOJ|ATOM) [v : SEXP]] => SEXP"); 
	switch(pairlength(o))
	{
		case 1:
			tmp = car(o);
			if(tmp == snil)
				return snil;
			else if(tmp->metadata == nil)
				return snil;
			ret = (SExp *)hmalloc(sizeof(SExp));
			ret->type = DICT;
			ret->object.dict = tmp->metadata;
			return ret;
		case 2:
			tmp = car(o);
			ret = car(cdr(o));
			if(tmp == snil)
				return snil;
			else if(tmp->metadata == nil)
				return snil;
			if(ret == snil)
				return snil;
			if(ret->type != STRING && ret->type != KEY && ret->type != ATOM)
				return makeerror(1,0,"meta!'s k parameter *must be of type STRING|KEYOBJ|ATOM");
			ret = trie_get(ret->object.str,tmp->metadata);
			if(ret == nil)
				return makeerror(1,0,"meta: no such key");
			return ret;
		case 3:
			tmp = car(o);
			ret = car(cdr(o));
			if(tmp == snil)
				return snil;
			else if(ret == snil)
				return snil;
			if(ret->type != STRING && ret->type != KEY && ret->type != ATOM)
				return makeerror(1,0,"meta!'s k parameter *must be of type STRING|KEYOBJ|ATOM");
			if(tmp->metadata == nil)
			{
				tmp->metadata = (Trie *)hmalloc(sizeof(Trie));
				tmp->metadata->key = nul;
				tmp->metadata->n_len = 0;
				tmp->metadata->n_cur = 0;
				tmp->metadata->nodes = nil;
			}
			trie_put(ret->object.str,car(cdr(cdr(o))),tmp->metadata);
			return svoid;
	}
	return makeerror(1,0,"meta o : SEXP [k : (STRING|KEYBOJ|ATOM) [v : SEXP]] => SEXP"); 
}
SExp *
cloneenv(SExp *e)
{
	return snil;
}
SExp *
fappend(SExp *rst)
/* wrapped the append switch in a function so that it can be easily compiled with Eris */
{
	switch(pairlength(rst))
	{
		case 0:
			return snil;
		case 1:
			return car(rst);
		case 2:
			return bappend(car(rst),car(cdr(rst)));
		default:
			return append(rst);
	}
}
SExp *
ftype(SExp *r)
{
    if(r != nil)
    {
        return makestring(typenames[r->type]);
    }
    return nil;
}

/* @(#) Internal definitions for Vesta
 */
#ifndef __VESTA_H
#define __VESTA_H

#define nil NULL
#define nul '\0'
#define MAX_STRING 2048

#define hmalloc GC_MALLOC
#define hrealloc GC_REALLOC
/* high-level information */
#define RELEASE "6.7-alpha"

/* accessor macros */
#define mcar(x) (x)->object.clist.first
#define mcdr(x) (x)->object.clist.rest
#define mnconc(x) cons((x),snil)
#define set_int(x,y) (x)->object.n->nobject.z = (y)
#define get_int(x) (x)->object.n->nobject.z
#define set_str(x,y) (x)->object.str = hstrdup(y)
#define AINT(x) (x)->object.n->nobject.z
#define AREAL(x) (x)->object.n->nobject.real
#define ASTRING(x) (x)->object.str
#define NUM(x) (x)->object.n->nobject.rational.num
#define DEN(x) (x)->object.n->nobject.rational.den
#define CEREAL(x) (x)->object.n->nobject.complex.r
#define IMMAG(x) (x)->object.n->nobject.complex.i
#define TYPE(x) (x)->type
#define TYPEP(x, y) ((x)->type == (y))
#define NTYPE(x) (x)->object.n->type
#define PORT(x) (x)->object.p
#define PTYPE(x) (x)->object.p->type
#define FILEPORT(x) (x)->object.p->pobject.f
#define FILEADDRESS(x) (x)->object.p->fileaddress
#define PROTONUMBER(x) (x)->object.p->protocol_number
#define NETBIND(x) (x)->object.p->bind
#define FILEMODE(x) (x)->object.p->mode
#define SOCKINFO(x) (x)->object.p->sock_info

#ifdef DEBUG
#define LINE_DEBUG printf("Made it to %d in %s\n", __LINE__, __FUNCTION__)
#else
#define LINE_DEBUG
#endif

/* def macros */
#define INTERNDEF(x) SExp *x(SExp *, Symbol *)

#define isnumeric(x) ((x >= '0' && x <= '9') || x == '+' || x == '-' || x == '.' || x == 'i' || x == '/')
#define issymdelim(x) ((x) != '"' && (x) != '\'' && (x) != '(' && (x) != ')' && (x) != '[' && (x) != ']' && (x) != '{' && (x) != '}' && (x) != ' ' && (x) != '\n' && (x) != '\t')

/* returns from lex, one or two per type */
#define TOK_LPAREN 0 /* #\( */
#define TOK_RPAREN 1 /* #\) */
#define TOK_LSQUAR 2 /* #\[ */
#define TOK_RSQUAR 3 /* #\] */
#define TOK_LCURLY 4 /* #\{ */
#define TOK_RCURLY 5 /* #\} */
#define TOK_INT    6 
#define TOK_LITSTR 7 /* "literal string" */
#define TOK_NQUOTE 8 /* quote #\' */
#define TOK_MQUOTE 9 /* meta-quote #\` */
#define TOK_UNQUOT 10 /* unquote #\, */
#define TOK_SPLICE 11 /* unquote-splicing (#\, #\@) */
#define TOK_SYMBOL 12
#define TOK_CHAR   13
#define TOK_TRUE   14
#define TOK_FALSE  15
#define TOK_SUCC   16
#define TOK_UNSUCC 17
#define TOK_NAME   18 /* named characters, i.e. #|space */
#define TOK_REAL   19
#define TOK_RATIO  20
#define TOK_COMPL  21
#define TOK_KEY    22 /* :key-object */
#define TOK_HEX    23 /* #xFF */
#define TOK_OCT    24 /* #o77 */
#define TOK_BIN    25 /* #b10 */
#define TOK_LEOF   26 /* #e */
#define TOK_LVOID  27 /* #v */
#define TOK_DATCOM 28 /* #;OBJECT */
#define TOK_SRFI10 29 /* SRFI-10 style #, */
#define TOK_TREE   30 /* #{ tree } */
#define TOK_SERROR 97 /* EOF before end of string */
#define TOK_HERROR 98 /* syntax error on hash object */
#define TOK_EOF    99

/* llread states */
#define START_STATE 0
#define LIST_STATE 1
#define VECT_STATE 2
#define DICT_STATE 3
#define ERRO_STATE 99

#define DEBUG_PRI printf("Made it to %s %d\n", __FUNCTION__, __LINE__)

#define _read_return(x) if(sp >= 0) \
	{ \
		cons(x,cons(ret,snil));\
	} \
	else \
		return x;

/* for now; this should be something better... */
#define __return(x) __val = (x); \
		state = __INTERNAL_RETURN; \
		goto __base

#define lleval __seval
	 
typedef enum
{
 ATOM, NUMBER, CHAR, BOOL, GOAL, VECTOR, PAIR, STRING, PROCEDURE, 
 CLOSURE, FOREIGN, NIL, ERROR, PORT, MACRO, USER, TCONC, PRIM, DICT, 
 KEY, SYNTAX, SEOF, SVOID, ENVIRONMENT, USFOREIGN, CONTINUATION
} SExpType;

typedef enum
{
 INTEGER, REAL, RATIONAL, COMPLEX
} NumType;

typedef enum
{
	PSTRING, PNET, PFILE
} PortType;

typedef enum 
{
	OPCAR, OPCDR, OPCONS, OPLAMBDA, OPDEF, OPLENGTH, OPDEFMACRO, OPDEFSYN, OPQUOTE, OPPLUS,
	OPMULT, OPSUB, OPDIV, OPLIST, OPVECTOR, OPDICT, OPMKSTRING, OPMKVEC, OPMKDICT, OPEVAL, 
	OPAPPLY, OPSTRING, OPCCONS, OPFIRST, OPREST, OPCSET,OPCUPDATE, OPCSLICE, OPNTH, OPKEYS, OPPARTIAL,
	OPSET, OPGENSYM, OPAPPEND, OPEQ, OPTYPE, OPCALLCC, OPLT, OPGT, OPLTE, OPGTE, OPIF, OPUNWIND, 
	OPEXACT, OPINEXACT, OPCOMPLEX, OPREAL, OPINTEGER, OPRATIONAL, OPBEGIN, OPNUM, OPDEN, OPWITHEXCEPT,
	OPAND, OPOR, OPXOR, OPNEG, OPGCD, OPLCM, OPNUMEQ, OPMOD, OPQUOTIENT, OPREMAINDER, OPERROR, 
	OPMKPOL, OPMKRECT, OPIMAG, OPREALP, OPARG, OPMAG, OPSQRT, OPABS, OPEXP, OPLN, OPCONJBANG, OPPOLREC, OPRECPOL,
	OPSIN,OPCOS,OPTAN,OPACOS, OPASIN,OPATAN, OPATAN2, OPCOSH, OPSINH, OPTANH, OPEXP2, OPEXPM1, OPSHR, OPSHL,
	OPLOG2, OPLOG10, OPCONJ, OPSTRAP, OPASSQ, OPCOERCE, OPMKTCONC, OPTCONC, OPTCONCL, OPT2P, OPTCONCSPLICE,
	OPDEFREC, OPSETREC, OPSETCURIN, OPSETCUROUT, OPSETCURERR, OPCURIN, OPCUROUT, OPCURERR, OPMEMQ,
	OPQQUOTE, OPUNQUOTE, OPUNQSPLICE, OPDICHAS, OPEMPTY, OPCEIL, OPFLOOR, OPTRUNCATE, OPROUND,
	OPIN2EX, OPCLONENV, OPDEFENV, OPSETENV, OPSTDENV, OPFROMENV, OPMETA,OPRESET, OPSHIFT, OPCURTICK, 
	OPDEFAULTENV, OPNULLENV, __INTERNAL_RETURN, __PRE_APPLY, __POST_APPLY, __INT_CLOSURE, __PROC,
    __POST_POST_APPLY, __CONT
} InternalOps;

typedef struct _NUM
{
	NumType type;
	union
	{
		int z;
		double real;
		struct
		{
			int num;
			int den;
		} rational;
		struct
		{
			double r;
			double i;
		} complex;
	} nobject;
} Number;

typedef struct
{
	/* fileaddress: port-filename
	 * mode: port-mode
	 * protocol_number: port-protocol-number
	 */
	char state; /* open, closed */
	PortType type;
	char *fileaddress; /* file name or address */
	int protocol_number; /* from struct protoent... */
	int bind; /* addres bound to... */
	char mode[3]; /* r,r+,w,w+,a,a+ */
	union
	{
		char *s;
		FILE *f;
		int fd; /* socket... need more than just this, no? */
	} pobject;
	void *sock_info; /* struct saddr_in pointer really... */
} Port;

/* Dictionaries in Vesta are backed by tries */
typedef struct __TRIE
{
	char key;
	int n_len;
	int n_cur;
	void *data; /* SExp */
	struct __TRIE **nodes;
} Trie ;

typedef struct _SEXP
{
	SExpType type;
	int length; /* length of aggregated s-expressions, such as strings or vectors */
	Trie *metadata;
	union
	{
		char c;
		char *str;
		struct _SEXP **vec;
		struct
		{
			int procnum;
			int arity;
			struct _SEXP *params;
			struct _SEXP *data;
			void *env; /* environment frame pointer */
		} closure;
		struct
		{
			struct _SEXP *first;
			struct _SEXP *rest;
		} clist;
		void *foreign;
		//struct _SEXP *(*procedure)(struct _SEXP *, void *); 
		void *procedure;
		Number *n;
		Port *p;
		struct
		{
			char source;
			int level;
			char *message;
		} error;
		struct
		{
			char evalp;
			int num;
			char *name;
		}primitive;
		Trie *dict;
	} object;
} SExp;

typedef struct _NODE
{
    int key;
    SExp *data;
    struct _NODE *parent;
    struct _NODE *left;
    struct _NODE *right;
} AVLNode;

/* A hybrid trie/cons-list based approach... */
/* Eventually, this should be a vector w/ copying... */
typedef struct _WINDOW
{
	Trie *env;
	struct _WINDOW *next;
} Window;

/* the basic outline of the GCObject. Used for
 * the allocation list. Instead of maintaining
 * a free list, keep *one* single list, and change
 * a "direction" bit that represents if something
 * is free/used. Basically, mark-don't-sweep
 */

typedef struct GCO
{
    unsigned char mark;
    size_t length;
    void *ptr;
    struct GCO *next;
    struct GCO *prev;
} GCObject;

typedef struct _SYM
{
    unsigned char mark_direction;
	int cur_size;
	int cur_offset;
	int tick;
	Window *data;
	SExp *curstdin;
	SExp *curstdout;
	SExp *curstderr;
	SExp *snil;
	SExp *strue;
	SExp *sfalse;
	SExp *svoid;
	SExp *ssucc;
	SExp *sunsucc;
	SExp *fake_rsqr;
	SExp *fake_rcur;
	SExp *fake_rpar;
    SExp *guards; /* stack of error handlers from with-exception-handler */
    SExp *qnan;
    SExp *snan;
	SExp *seof;
    GCObject *head;
    GCObject *lptr;
} Symbol;

SExp *eqp(SExp *,SExp *); /* eq? */
SExp *eqp_atom(SExp *, char *);
SExp *assq(SExp *, SExp *); /* standard assq */
SExp *memq(SExp *, SExp *); /* standard memq */

/* the code functions of the GC system.
 * - init_gc_ring initializes the initial allocation
 * node list. 
 * - gc kicks off the Garbage collection process
 * - gcalloc is the drop in replacement for malloc
 *   (though, not really, since it will most likely
 *    have to take an environment parameter too)
 * - mark strolls through the environment & checks
 * out what can be marked as free.
 */

GCObject *init_gc_ring(int, Symbol *);
void gc(Symbol *);
void *gcalloc(size_t, Symbol *);
void mark_sexp(SExp *);
void mark_literal(void *);
void mark(GCObject);

/* make functions */
SExp *makenumber(int);
SExp *makeinteger(int);
SExp *makereal(double);
SExp *makerational(int,int);
SExp *makecomplex(double,double);
SExp *makeerror(int,int,char*);
SExp *makeprimitive(int, char *,int);
SExp *makechar(char);
SExp *makeport(FILE *,char *, int, int, char *);
SExp *makevector(int, SExp *);
SExp *makedict();
SExp *makestring_v(int, char);
SExp *makestring(const char *);
SExp *makeatom(char *);
SExp *makekey(char *);
SExp *makeenv(Symbol *); /* wrap an Environment in an SEXP */

/* memory functions */
int gc_init();
Symbol *init_env();
void clean_env();

/* low-level core functions */
int lex(FILE *,char **);
void llprinc(SExp *, FILE *, int); /* int is mode: display or write */
SExp *llread(FILE *);
SExp *__seval(SExp *, Symbol *);
SExp *macro_expand(SExp *, Symbol *);
SExp *syntax_expand(SExp *, Symbol *);
char *hstrdup(const char *);

/* math functions */
int _igcd(int,int);

/* environment functions */
Symbol *add_env(Symbol *, char *, SExp *);
int new_window(Symbol *);
int close_window(Symbol *);
Symbol *shallow_clone_env(Symbol *);
SExp *symlookup(char*,Symbol *);
void register_procedure(SExp *(*)(SExp *, Symbol *),char *, int, Symbol *); /* register a C function; int is arity */

/* string & formatting functions */
char *_itoa(char *,int, int *);
char *_itox(char *,unsigned int, int *);
char *_itoo(char *,unsigned int, int *);
char *_ftoa(char *,double,int *,int); /* last int: flag -> 0,'%g'; 1,'%G' */
char *_strcpy(char *,char *,int *);
int atox(char *);
int atoo(char *);
int atob(char *);
SExp *format(SExp *, Symbol *); /* CL/SRFI-28-style format; the C-backer of F's display* (in the CoreScheme early editions) or format in F */
SExp *fcoerce(SExp *,SExp *);

/* trie functions */
SExp *trie_put(char *,SExp *, Trie *); /* returns strue or sfalse */
SExp *trie_get(char *, Trie *);
SExp *trie_keys(Trie *,SExp *); /* returns a list of keys */
SExp *trie_vals(Trie *);
SExp *trie_pairs(Trie *);
Trie *trie_alloc(char *,SExp *);
Trie *trie_clone(Trie *); /* useful for things like structs where trie should be inherited... */
void trie_walk(Trie *,int);
SExp *trie_partial(Trie *, char *, int); /* partial-key? */

/* AVL tree functions */
AVLNode *makeavlnode(int);
int avl_containsp(AVLNode *,int);
int avl_insert(AVLNode *, int, SExp *);
int avl_insert_f(AVLNode *, SExp *, SExp *);
SExp *avl_get(AVLNode *, int);
SExp *avl_get(AVLNode *, SExp *);
int weight(AVLNode *);
AVLNode *balance(AVLNode *);
AVLNode *rotate_left(AVLNode *);
AVLNode *rotate_right(AVLNode *);
int fnv1a_s(SExp *);
int number_bytes(SExp *, char *);

/* wrapper around llprinc */
void princ(SExp *); /* for history's sake */

/* sexpression functions */
SExp *car(SExp *);
SExp *cdr(SExp *);
SExp *cons(SExp *, SExp *);
SExp *snoc(SExp *, SExp *);
SExp *tconc(SExp *, SExp *);
SExp *tconc_splice(SExp *, SExp *);
SExp *tconcify(SExp *);
SExp *bappend(SExp *, SExp *);
SExp *append(SExp *);
int pairlength(SExp *);
SExp *eqp(SExp *, SExp *);
SExp *assq(SExp *, SExp *);
SExp *list(int, ...);
SExp *vector(int, ...);
SExp *list_copy(SExp *, int, int);

/* math primitives */
SExp *fsqrt(SExp *);
SExp *fden(SExp *);
SExp *fnum(SExp *);
SExp *fplus(SExp *);
SExp *fplus_in(int, SExp *);
SExp *fplus_nn(SExp *, SExp *);
SExp *inc_i(SExp *, int);
SExp *fmult(SExp *);
SExp *fdivd(SExp *);
SExp *fsubt(SExp *);
SExp *freal_part(SExp *);
SExp *fimag_part(SExp *);
SExp *fmake_rect(SExp *, SExp *);
SExp *fmake_pole(SExp *, SExp *);
SExp *fconjugate(SExp *);
SExp *fconjugate_bang(SExp *);
SExp *fpol2rect(SExp *);
SExp *frect2pol(SExp *);
SExp *fgcd(SExp *);
SExp *flcm(SExp *);
SExp *fquotient(SExp *, SExp *);
SExp *fmodulo(SExp *, SExp *);
SExp *fremainder(SExp *, SExp *);
SExp *fsin(SExp *);
SExp *fcos(SExp *);
SExp *ftan(SExp *);
SExp *fasin(SExp *);
SExp *facos(SExp *);
SExp *fatan(SExp *);
SExp *fatan2(SExp *, SExp *);
SExp *fcosh(SExp *);
SExp *fsinh(SExp *);
SExp *ftanh(SExp *);
SExp *fexp(SExp *);
SExp *fexp2(SExp *);
SExp *fexpm1(SExp *);
SExp *fln(SExp *);
SExp *flog2(SExp *);
SExp *flog10(SExp *);
SExp *fnabs(SExp *);
SExp *fmag(SExp *);
SExp *flt(SExp *);
SExp *flte(SExp *);
SExp *fgt(SExp *);
SExp *fgte(SExp *);
SExp *fnumeq(SExp *);
SExp *flt_ni(SExp *, int);
SExp *flte_ni(SExp *, int);
SExp *fgt_ni(SExp *, int);
SExp *fgte_ni(SExp *, int);
SExp *fnumeq_ni(SExp *, int);
SExp *flt_nn(SExp *, SExp *);
SExp *flte_nn(SExp *, SExp *);
SExp *fgt_nn(SExp *, SExp *);
SExp *fgte_nn(SExp *, SExp *);
SExp *fnumeq_nn(SExp *, SExp *);
SExp *fbitand(SExp *,SExp *);
SExp *fbitor(SExp *,SExp *);
SExp *fbitxor(SExp *,SExp *);
SExp *fbitnot(SExp *);
SExp *fbitshl(SExp *, SExp *);
SExp *fbitshr(SExp *, SExp *);
SExp *fceil(SExp *);
SExp *ffloor(SExp *);
SExp *fround(SExp *);
SExp *ftrunc(SExp *);
SExp *fin2ex(SExp *);
SExp *fexactp(SExp *);
SExp *finexactp(SExp *);
SExp *frealp(SExp *);
SExp *fcomplexp(SExp *);
SExp *frationalp(SExp *);
SExp *fintegerp(SExp *);

/* syntactic primitives */
SExp *fset(SExp *, SExp *, Symbol *);
SExp *fdef(SExp *, SExp *, Symbol *);
SExp *fcond(SExp *, Symbol *);
SExp *fdefmacro(SExp *, SExp *, SExp *, Symbol *);
SExp *fdefsyntax(SExp *, SExp *, Symbol *);
SExp *ffn(SExp *, Symbol *);

/* collection primitives */
SExp *ffirst(SExp *);
SExp *frest(SExp *);
SExp *fnth(SExp *, SExp *,SExp *);
SExp *fcset(SExp *, SExp *, SExp *); // collection set!
SExp *fccons(SExp *, SExp *);
SExp *flength(SExp *);
SExp *fempty(SExp *);
SExp *fcupdate(SExp *,SExp *, SExp *);
SExp *fcslice(SExp *, SExp *, SExp *);
SExp *fvector(SExp *);
SExp *fmkvector(SExp *);
SExp *fstringappend(SExp *);
SExp *fdict(SExp *);
SExp *fdicthas(SExp *, SExp *);
SExp *fkeys(SExp *);
SExp *fpartial_key(SExp *, SExp *);
SExp *fstring(SExp *);
SExp *fmakestring(SExp *);
SExp *fappend(SExp *);
SExp *ftype(SExp *);
/* generic functions */
SExp *fgensym(SExp *);
SExp *fmeta(SExp *);
SExp *cloneenv(SExp *);
/* Desktop-level foreign procedures */
INTERNDEF(f_princ);
INTERNDEF(f_load);
INTERNDEF(newline); /* Scheme newline/CL terpri */
INTERNDEF(interrogate); /* debugger, of sorts */
INTERNDEF(f_system); /* system(3) wrapper */
INTERNDEF(f_execve); /* execve(3) wrapper */
INTERNDEF(f_fork);
INTERNDEF(f_waitpid);
INTERNDEF(f_sysread);
INTERNDEF(f_syswrite);
INTERNDEF(f_sysopen);
INTERNDEF(f_sysclose);
INTERNDEF(f_syspipe);
INTERNDEF(f_sysfcntl);
INTERNDEF(f_sysfcntlconst);
INTERNDEF(f_popen);
INTERNDEF(f_pclose);
INTERNDEF(f_chdir);
INTERNDEF(f_ls);
INTERNDEF(f_pwd);
INTERNDEF(f_stat);
INTERNDEF(f_pwd); 
INTERNDEF(f_quit);
INTERNDEF(f_vfork);
INTERNDEF(f_kill);
INTERNDEF(f_remote_openp);
INTERNDEF(f_syn_openp);
INTERNDEF(f_fin_openp);
INTERNDEF(f_udp_openp);
INTERNDEF(f_icmp_responsep);
INTERNDEF(f_bannerp);
INTERNDEF(f_ack_openp);
INTERNDEF(f_rete);
INTERNDEF(f_nil_openp);
INTERNDEF(f_xmas_openp);
INTERNDEF(f_send_rst);
INTERNDEF(f_gethostbyname);
INTERNDEF(f_gethostbyaddr);
INTERNDEF(f_open);
INTERNDEF(f_close);
INTERNDEF(f_read);
INTERNDEF(f_write);
INTERNDEF(f_read_char);
INTERNDEF(f_read_string);
INTERNDEF(f_write_char);
INTERNDEF(f_dial);
INTERNDEF(f_announce);
INTERNDEF(f_listen);
INTERNDEF(f_accept);
INTERNDEF(f_hangup);
INTERNDEF(f_getuid);
INTERNDEF(f_geteuid);
INTERNDEF(f_getgid);
INTERNDEF(f_getegid);
INTERNDEF(f_setsid);
INTERNDEF(f_write_buf);
INTERNDEF(f_read_buf);
INTERNDEF(f_gettimeofday);
INTERNDEF(f_ssockopt);
INTERNDEF(f_peekchar);
INTERNDEF(f_char_ready);
INTERNDEF(f_chown);
INTERNDEF(f_chmod);
INTERNDEF(f_chroot);
INTERNDEF(f_umask);
INTERNDEF(f_port_filename);
INTERNDEF(f_port_mode);
INTERNDEF(f_port_bind);
INTERNDEF(f_port_proto);
INTERNDEF(f_port_state);
INTERNDEF(f_port_type);
INTERNDEF(f_end_of_portp);
INTERNDEF(f_set_input);
INTERNDEF(f_set_output);
INTERNDEF(f_set_error);
INTERNDEF(f_cur_input);
INTERNDEF(f_cur_output);
INTERNDEF(f_cur_error);
INTERNDEF(f_random);
INTERNDEF(f_seed_random);
INTERNDEF(f_time);
INTERNDEF(f_getenv);
INTERNDEF(f_setenv);
INTERNDEF(f_cd);
INTERNDEF(f_syssleep);
INTERNDEF(f_sysusleep);
INTERNDEF(f_sysnanosleep);
INTERNDEF(f_sysselect);
INTERNDEF(f_sysgetpid);
INTERNDEF(f_sysgetppid);
#endif

#ifndef __MURT_H__
#define __MURT_H__

/* tiny runtime header for enyalios output... */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <gc/gc.h>
#include "vesta.h"

extern SExp *snil, *sfalse, *strue, *ssucc, *sunsucc,*mem_err, *pinf, *ninf, *qnan, *snan;
extern SExp *fake_rpar, *fake_rsqr, *fake_rcur; /* returns for llread, but should never really mean anything */
extern SExp *seof, *svoid; /* #e, for use with read*, & eof-object?, #v for void */

SExp *typhon_main(SExp *args);

#define SNIL snil
#define SFALSE sfalse
#define STRUE strue
#define SSUCC ssucc
#define SUNSUCC sunsucc
#define SEOF seof
#define SVOID svoid

#define f_error(x) ((x)->type == STRING ? makeerror(2,0,x->object.str) : makeerror(2,1,"something happened"))
#endif

#ifndef __MURT_H__
#define __MURT_H__

/* tiny runtime header for enyalios output... */

#include "vesta.h"

extern SExp *snil, *sfalse, *strue, *ssucc, *sunsucc,*mem_err, *pinf, *ninf, *qnan, *snan;
extern SExp *fake_rpar, *fake_rsqr, *fake_rcur; /* returns for llread, but should never really mean anything */
extern SExp *seof, *svoid; /* #e, for use with read*, & eof-object?, #v for void */

#endif

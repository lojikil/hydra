#include "vesta.h"

extern SExp *snil, *sfalse, *strue, *ssucc, *sunsucc,*mem_err, *pinf, *ninf, *qnan, *snan;
extern SExp *fake_rpar, *fake_rsqr, *fake_rcur; /* returns for llread, but should never really mean anything */
extern SExp *seof, *svoid; /* #e, for use with read*, & eof-object?, #v for void */

SExp *caar(SExp *);
SExp *caaar(SExp *);
SExp *caaaar(SExp *);
SExp *caaadr(SExp *);
SExp *caadr(SExp *);
SExp *caadar(SExp *);
SExp *caaddr(SExp *);
SExp *cadr(SExp *);
SExp *cadar(SExp *);
SExp *cadaar(SExp *);
SExp *cadadr(SExp *);
SExp *caddr(SExp *);
SExp *caddar(SExp *);
SExp *cadddr(SExp *);
SExp *cadddar(SExp *);
SExp *cdar(SExp *);
SExp *cdaar(SExp *);
SExp *cdaaar(SExp *);
SExp *cdaadr(SExp *);
SExp *cdadr(SExp *);
SExp *cdadar(SExp *);
SExp *cdaddr(SExp *);
SExp *cddr(SExp *);
SExp *cddar(SExp *);
SExp *cddaar(SExp *);
SExp *cddadr(SExp *);
SExp *cdddr(SExp *);
SExp *cdddar(SExp *);
SExp *cddddr(SExp *);
SExp *copy_code(SExp *, SExp *, SExp *);
SExp *compile_lambda(SExp *, SExp *);
SExp *nullp(SExp *);
SExp *numberp(SExp *);
SExp *not(SExp *);
SExp *pairp(SExp *);
SExp *vectorp(SExp *);
SExp *voidp(SExp *);
SExp *dictp(SExp *);
SExp *symbolp(SExp *);
SExp *stringp(SExp *);
SExp *show(SExp *);
SExp *keyp(SExp *);
SExp *boolp(SExp *);
SExp *build_environment(SExp *, SExp *, SExp *);
SExp *goalp(SExp *);
SExp *zerop(SExp *);
SExp *zip(SExp *, SExp *);
SExp *eof_objectp(SExp *);
SExp *list_copy(SExp *);
SExp *map(SExp *, SExp *);
SExp *foreach(SExp *, SExp *);
SExp *fun_97(SExp *);
SExp *fun_106(SExp *);
SExp *fun_115(SExp *);
SExp *fun_117(SExp *);
SExp *fun_120(SExp *);
SExp *fun_123(SExp *);
SExp *fun_130(SExp *);
SExp *fun_132(SExp *);
SExp *fun_134(SExp *);
SExp *fun_136(SExp *);
SExp *fun_138(SExp *);
SExp *append_map(SExp *, SExp *);
SExp *hydra_instruction(SExp *);
SExp *hydra_operand(SExp *);
SExp *hydra_vm(SExp *, SExp *, SExp *, SExp *, SExp *);
SExp *hydra_lookup(SExp *, SExp *);
SExp *hydra_load(SExp *, SExp *);
SExp *hydra_lambdap(SExp *);
SExp *hydra_primitivep(SExp *);
SExp *hydra_syntaxp(SExp *);
SExp *hydra_set_env_(SExp *, SExp *, SExp *);
SExp *hydra_error(SExp *);
SExp *hydra_errorp(SExp *);
SExp *hydra_eval(SExp *, SExp *);
SExp *hydra_continuationp(SExp *);
SExp *hydra_compile(SExp *, SExp *);
SExp *hydra_compile_help(SExp *, SExp *, SExp *);
SExp *hydra_add_env_(SExp *, SExp *, SExp *);
SExp *hydra_repl();
SExp *hydra_main();
SExp *reverse_append(SExp *);
SExp *top_level_print(SExp *);
Symbol *enyalios93;

SExp *
fun_138(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_136(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_134(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_132(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_130(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_123(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_120(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_117(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_115(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_106(SExp *x){
    return hydra_compile(x, env);
}
SExp *
fun_97(SExp *x){
    return fcset(tmp96, car(x), cadr(x));
}
SExp *
caar(SExp *x){
    return car(car(x));
}
SExp *
cadr(SExp *x){
    return car(cdr(x));
}
SExp *
cdar(SExp *x){
    return cdr(car(x));
}
SExp *
cddr(SExp *x){
    return cdr(cdr(x));
}
SExp *
caaar(SExp *x){
    return car(car(car(x)));
}
SExp *
caadr(SExp *x){
    return car(car(cdr(x)));
}
SExp *
cadar(SExp *x){
    return car(cdr(car(x)));
}
SExp *
caddr(SExp *x){
    return car(cdr(cdr(x)));
}
SExp *
cdaar(SExp *x){
    return cdr(car(car(x)));
}
SExp *
cdadr(SExp *x){
    return cdr(car(cdr(x)));
}
SExp *
cddar(SExp *x){
    return cdr(cdr(car(x)));
}
SExp *
cdddr(SExp *x){
    return cdr(cdr(cdr(x)));
}
SExp *
caaaar(SExp *x){
    return car(car(car(car(x))));
}
SExp *
caaadr(SExp *x){
    return car(car(car(cdr(x))));
}
SExp *
caadar(SExp *x){
    return car(car(cdr(car(x))));
}
SExp *
caaddr(SExp *x){
    return car(car(cdr(cdr(x))));
}
SExp *
cadaar(SExp *x){
    return car(cdr(car(car(x))));
}
SExp *
cadadr(SExp *x){
    return car(cdr(car(cdr(x))));
}
SExp *
caddar(SExp *x){
    return car(cdr(cdr(car(x))));
}
SExp *
cadddr(SExp *x){
    return car(cdr(cdr(cdr(x))));
}
SExp *
cdaaar(SExp *x){
    return cdr(car(car(car(x))));
}
SExp *
cdaadr(SExp *x){
    return cdr(car(car(cdr(x))));
}
SExp *
cdadar(SExp *x){
    return cdr(car(cdr(car(x))));
}
SExp *
cdaddr(SExp *x){
    return cdr(car(cdr(cdr(x))));
}
SExp *
cddaar(SExp *x){
    return cdr(cdr(car(car(x))));
}
SExp *
cddadr(SExp *x){
    return cdr(cdr(car(cdr(x))));
}
SExp *
cdddar(SExp *x){
    return cdr(cdr(cdr(car(x))));
}
SExp *
cddddr(SExp *x){
    return cdr(cdr(cdr(cdr(x))));
}
SExp *
nullp(SExp *n){
    return eqp(n, SNIL);
}
SExp *
pairp(SExp *n){
    return eqp(ftype(n), makestring("Pair"));
}
SExp *
vectorp(SExp *n){
    return eqp(ftype(n), makestring("Vector"));
}
SExp *
dictp(SExp *n){
    return eqp(ftype(n), makestring("Dictionary"));
}
SExp *
symbolp(SExp *n){
    return eqp(ftype(n), makestring("Symbol"));
}
SExp *
keyp(SExp *n){
    return eqp(ftype(n), makestring("Key"));
}
SExp *
numberp(SExp *n){
    return eqp(ftype(n), makestring("Number"));
}
SExp *
stringp(SExp *n){
    return eqp(ftype(n), makestring("String"));
}
SExp *
boolp(SExp *n){
    return eqp(ftype(n), makestring("Boolean"));
}
SExp *
goalp(SExp *n){
    return eqp(ftype(n), makestring("Goal"));
}
SExp *
not(SExp *x){
    if((eqp(x, SSUCC) == STRUE)){
        return SUNSUCC;
    }
    else if((eqp(x, SFALSE) == STRUE)){
        return STRUE;
    }
    else if((eqp(x, SUNSUCC) == STRUE)){
        return SSUCC;
    }
    else {
        return SFALSE;
    }
}
SExp *
zerop(SExp *n){
    return fnumeq(list(2, n, makeinteger(0)));
}
SExp *
eof_objectp(SExp *n){
    return eqp(n, SEOF);
}
SExp *
voidp(SExp *x){
    return eqp(x, SVOID);
}
SExp *
zip(SExp *xs, SExp *ys){
    if((nullp(xs) == STRUE)){
        return SNIL;
    }
    else {
        return cons(cons(car(xs), cons(car(ys), SNIL)), zip(cdr(xs), cdr(ys)));
    }
}
SExp *
list_copy(SExp *l){
    /*  really, should be included from SRFI-1, but this simply makes a copy
      of the spine of a pair
      
     */
    if((nullp(l) == STRUE)){
        return l;
    }
    else {
        return cons(car(l), list_copy(cdr(l)));
    }
}
SExp *
cadddar(SExp *x){
    return car(cdddar(x));
}
SExp *
map(SExp *proc, SExp *c){
    if((fempty(c) == STRUE)){
        return c;
    }
    else {
        return fccons(proc(ffirst(c)), map(proc, frest(c)));
    }
}
SExp *
foreach(SExp *proc, SExp *c){
        while(1) {
        if((fempty(c) == STRUE)){
            return SVOID;
        }
        else {
            proc(ffirst(c));
            proc48 = proc;
            c49 = frest(c);
            proc = proc48; 
            c = c49; 
        }
    }
}
SExp *
append_map(SExp *f, SExp *x){
    if((nullp(x) == STRUE)){
        return x;
    }
    else {
        return fappend(list(2, f(car(x)), append_map(f, cdr(x))));
    }
}
SExp *
hydra_instruction(SExp *c){
    return car(c);
}
SExp *
hydra_operand(SExp *c){
    return cadr(c);
}
SExp *
build_environment(SExp *environment, SExp *stack, SExp *params){
    /* Adds a new window to the environment, removes |params| items from the stack
     and binds those values in the new window. It returns a list of environment and
     the new stack.
     */
    SExp *tmp94 = flength(stack);
    SExp *tmp95 = flength(params);
    SExp *tmp96 = makedict();
    if((flt(list(2, tmp94, tmp95)) == STRUE)){
        return ferror(makestring("non-optional parameters are not statisfied by stack items in build-environment"));
    }
    else {
        if((fnumeq(list(2, tmp95, makeinteger(0))) == STRUE)){
            return list(2, cons(tmp96, environment), cdr(stack));
        }
        else {
            foreach(fun_97, zip(params, fcslice(stack, makeinteger(0), tmp95)));
            return list(2, cons(tmp96, environment), fcslice(stack, tmp95, tmp94));
        }
    }
}
SExp *
copy_code(SExp *code, SExp *ip, SExp *offset){
    /*  copies the spine of code, but at ip & ip+1, insert %nop instructions
      instead, over-writing the call/cc & load-lambda instructions therein.
      
     */
    if((nullp(code) == STRUE)){
        return SNIL;
    }
    else if((fnumeq(list(2, offset, fsubt(list(2, ip, makeinteger(1))))) == STRUE)){
        return fappend(list(2, list(2, list(1,makeinteger(107)), list(1,makeinteger(107))), copy_code(cddr(code), ip, fplus(list(2, offset, makeinteger(2))))));
    }
    else {
        return fappend(list(2, list(1, car(code)), copy_code(cdr(code), ip, fplus(list(2, offset, makeinteger(1))))));
    }
}
SExp *
hydra_vm(SExp *code, SExp *env, SExp *ip, SExp *stack, SExp *dump){
                    while(1) {
        /*  process the actual instructions of a code object; the basic idea is that
       the user enters:
       h; (car (cdr (cons 1 (cons 2 '()))))
       which is compiled by hydra@eval into:
       (4)   ;; nil
       (3 2) ;; load 2
       (2)   ;; cons
       (3 1) ;; load 1
       (2)   ;; cons
       (1)   ;; cdr
       (0)   ;; car
       
       which hydra@vm can then interpret in a tail-call fashion.
       There might be better ways to store the actual VM codes themselves
       other than pairs of pairs (even vector of pairs would be more efficient really)
       and it might be worth it to add two collexion-neutral primitives, cappend & 
       cappend!, to the collexion API. Also, adding named-enumerations to the language,
       even if at the syntactic level, would be helpful. If (enumerate OPCAR OPCDR ...)
       even compiled to
       #define OPCAR 0
       #define OPCDR 1
       // ...
       
       It would be more useful, and the names could be used throughout (and checked!).
         */
        if((fgte(list(2, ip, flength(code))) == STRUE)){
            if((nullp(dump) == STRUE)){
                return car(stack);
            }
            else {
                code60 = caar(dump);
                env61 = cadar(dump);
                ip62 = fplus(list(2, caddar(dump), makeinteger(1)));
                stack63 = cons(car(stack), cadddar(dump));
                dump64 = cdr(dump);
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
        }
        else {
            SExp *tmp99 = fnth(list(2, code, ip));
            SExp *tmp100 = hydra_instruction(c);
            if((eqp(tmp100, makeinteger(0)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(car(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(1)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(cdr(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(2)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(cons(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(3)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(hydra_operand(tmp99), stack);
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(4)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(SNIL, stack);
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(5)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fsubt(list(2, cadr(stack), car(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(6)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fplus(list(2, car(stack), cadr(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(7)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fmult(list(2, car(stack), cadr(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(8)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fdivi(list(2, cadr(stack), car(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(9)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(flt(list(2, cadr(stack), car(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(10)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fgt(list(2, cadr(stack), car(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(11)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(flte(list(2, cadr(stack), car(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(12)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fgte(list(2, cadr(stack), car(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(13)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(flength(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(14)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexactp(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(15)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(finexactp(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(16)) == STRUE)){
                f_princ(list(1, car(stack)), enyalios93);
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(SVOID, cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(18)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(frealp(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(19)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fintegerp(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(20)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcomplexp(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(21)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(frationalp(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(22)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fgcd(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(23)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(flcm(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(24)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fnum(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(25)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fden(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(26)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fnumeq(list(2, car(stack), cadr(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(27)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(eqp(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(28)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, hydra_operand(tmp99)));
                stack63 = stack;
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(29)) == STRUE)){
                if((car(stack) == STRUE)){
                    code60 = code;
                    env61 = env;
                    ip62 = fplus(list(2, ip, makeinteger(1)));
                    stack63 = cdr(stack);
                    dump64 = dump;
                    code = code60; 
                    env = env61; 
                    ip = ip62; 
                    stack = stack63; 
                    dump = dump64; 
                }
                else {
                    code60 = code;
                    env61 = env;
                    ip62 = fplus(list(2, ip, hydra_operand(tmp99)));
                    stack63 = cdr(stack);
                    dump64 = dump;
                    code = code60; 
                    env = env61; 
                    ip = ip62; 
                    stack = stack63; 
                    dump = dump64; 
                }
            }
            else if((eqp(tmp100, makeinteger(30)) == STRUE)){
                if((hydra_lambdap(car(stack)) == STRUE)){
                    SExp *tmp101 = build_environment(fnth(list(2, cadar(stack), makeinteger(0))), cdr(stack), fnth(list(2, cadar(stack), makeinteger(2))));
                    code60 = fnth(list(2, cadar(stack), makeinteger(1)));
                    env61 = car(tmp101);
                    ip62 = makeinteger(0);
                    stack63 = SNIL;
                    dump64 = cons(list(4, code, env, ip, cadr(tmp101)), dump);
                    code = code60; 
                    env = env61; 
                    ip = ip62; 
                    stack = stack63; 
                    dump = dump64; 
                }
                else if((hydra_primitivep(car(stack)) == STRUE)){
                    f_princ(list(1, makestring("in hydra@primitive\n\t")), enyalios93);
                    f_princ(list(1, car(stack)), enyalios93);
                    f_princ(list(1, makestring("\n")), enyalios93);
                    return STRUE;
                }
                else {
                    f_princ(list(1, makestring("in <else> of CALL\n")), enyalios93);
                    return SFALSE;
                }
            }
            else if((eqp(tmp100, makeinteger(31)) == STRUE)){
                SExp *tmp102 = hydra_lookup(hydra_operand(tmp99), env);
                if((eqp(tmp102, SFALSE) == STRUE)){
                    return SFALSE;
                }
                else {
                    code60 = code;
                    env61 = env;
                    ip62 = fplus(list(2, ip, makeinteger(1)));
                    stack63 = cons(tmp102, stack);
                    dump64 = dump;
                    code = code60; 
                    env = env61; 
                    ip = ip62; 
                    stack = stack63; 
                    dump = dump64; 
                }
            }
            else if((eqp(tmp100, makeinteger(32)) == STRUE)){
                if((not(nullp(stack)) == STRUE) && (eqp(caar(stack), makeatom("compiled-lambda")) == STRUE)){
                    code60 = fnth(list(2, cdar(stack), makeinteger(0)));
                    env61 = fnth(list(2, cdar(stack), makeinteger(1)));
                    ip62 = makeinteger(0);
                    stack63 = SNIL;
                    dump64 = dump;
                    code = code60; 
                    env = env61; 
                    ip = ip62; 
                    stack = stack63; 
                    dump = dump64; 
                }
                else {
                    return SFALSE;
                }
            }
            else if((eqp(tmp100, makeinteger(33)) == STRUE)){
                hydra_add_env_(car(stack), cadr(stack), env);
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(SVOID, stack);
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(34)) == STRUE)){
                hydra_set_env_(car(stack), cadr(stack), env);
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(SVOID, stack);
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(35)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fceil(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(36)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(ffloor(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(37)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(ftruncate(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(38)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fround(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(39)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fin2ex(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(40)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fquotient(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(41)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fmodulo(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(42)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fbitand(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(43)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fbitor(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(44)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fbitxor(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(45)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fbitnot(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(46)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcslice(cdr(stack), makeinteger(0), car(stack)), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(47)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcoerce(fcslice(cdr(stack), makeinteger(0), car(stack)), makeatom("vector")), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(48)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fmkvector(list(2, car(stack), cadr(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(49)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fmakestring(list(2, car(stack), cadr(stack))), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(50)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fstring(fcslice(cdr(stack), makeinteger(0), car(stack))), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(51)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fappend(fcslice(cdr(stack), makeinteger(0), car(stack))), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(52)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(ffirst(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(53)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(frest(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(54)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fccons(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(55)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fnth(list(3, caddr(stack), cadr(stack), car(stack))), cdddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(56)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fkeys(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(57)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fpartial_key(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(58)) == STRUE)){
                fcset(caddr(stack), cadr(stack), car(stack));
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(SVOID, cdddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(59)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fempty(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(60)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(gensym(list(1, car(stack))), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(61)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fimag_part(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(62)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(freal_part(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(63)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fmake_rect(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(64)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fmake_pole(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(65)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fmag(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(66)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(farg(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(67)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fconjugate(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(68)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fconjugate_bang(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(69)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fpol2rect(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(70)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(frect2pol(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(71)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fsin(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(72)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcos(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(73)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(ftan(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(74)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fasin(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(75)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(facos(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(76)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fatan(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(77)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fatan2(cadr(stack), car(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(78)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fsinh(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(79)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcosh(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(80)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(ftanh(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(81)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(82)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fln(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(83)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fnabs(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(84)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fsqrt(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(85)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(86)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexpm1(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(87)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(flog2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(88)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(flog10(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(89)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fbitshl(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(90)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fbitshr(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(91)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(92)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(assq(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(93)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(memq(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(94)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(95)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fdict(SNIL), stack);
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(96)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fdicthas(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(97)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcoerce(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(98)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcupdate(car(stack), cadr(stack), caddr(stack)), cdddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(99)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fcslice(car(stack), cadr(stack), caddr(stack)), cdddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(100)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(101)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(102)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(103)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(104)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(fexp2(car(stack)), cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(105)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(frationalize(car(stack), cadr(stack)), cddr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(106)) == STRUE)){
                SExp *tmp103 = hydra_vm(cons(list(2, makeinteger(3), car(stack)), list(1, list(1, makeinteger(30)))), env, makeinteger(0), cons(list(6, makeatom("continuation"), copy_code(code, ip, makeinteger(0)), ip, env, stack, dump), SNIL), SNIL);
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = cons(tmp103, cdr(stack));
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(107)) == STRUE)){
                code60 = code;
                env61 = env;
                ip62 = fplus(list(2, ip, makeinteger(1)));
                stack63 = stack;
                dump64 = dump;
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
            else if((eqp(tmp100, makeinteger(108)) == STRUE)){
                SExp *tmp104 = car(stack);
                SExp *tmp105 = cadr(stack);
                code60 = fnth(list(2, tmp104, makeinteger(1)));
                env61 = fnth(list(2, tmp104, makeinteger(3)));
                ip62 = makeinteger(0);
                stack63 = cons(tmp105, fnth(list(2, tmp104, makeinteger(4))));
                dump64 = fnth(list(2, tmp104, makeinteger(5)));
                code = code60; 
                env = env61; 
                ip = ip62; 
                stack = stack63; 
                dump = dump64; 
            }
        }
    }
}
SExp *_tlenv_ = list(1,fdict("car", cons(makeatom("primitive"), makeinteger(0)), "call/cc", cons(makeatom("primitive"), makeinteger(106)), "cdr", cons(makeatom("primitive"), makeinteger(1)), "cons", cons(makeatom("primitive"), makeinteger(2)), "conjugate", cons(makeatom("primitive"), makeinteger(67)), "conjugate!", cons(makeatom("primitive"), makeinteger(68)), "complex?", cons(makeatom("primitive"), makeinteger(20)), "cos", cons(makeatom("primitive"), makeinteger(72)), "cosh", cons(makeatom("primitive"), makeinteger(79)), "coerce", cons(makeatom("primitive"), makeinteger(97)), "ceil", cons(makeatom("primitive"), makeinteger(35)), "ccons", cons(makeatom("primitive"), makeinteger(54)), "cset!", cons(makeatom("primitive"), makeinteger(58)), "cslice", cons(makeatom("primitive"), makeinteger(99)), "cupdate", cons(makeatom("primitive"), makeinteger(98)), "%load", cons(makeatom("primitive"), makeinteger(3)), "%list", cons(makeatom("primitive"), makeinteger(46)), "%nil", cons(makeatom("primitive"), makeinteger(4)), "%nop", cons(makeatom("primitive"), makeinteger(107)), "%-", cons(makeatom("primitive"), makeinteger(5)), "%+", cons(makeatom("primitive"), makeinteger(6)), "%*", cons(makeatom("primitive"), makeinteger(7)), "%/", cons(makeatom("primitive"), makeinteger(8)), "%<", cons(makeatom("primitive"), makeinteger(9)), "%<=", cons(makeatom("primitive"), makeinteger(11)), "%>", cons(makeatom("primitive"), makeinteger(10)), "%>=", cons(makeatom("primitive"), makeinteger(12)), "%=", cons(makeatom("primitive"), makeinteger(26)), "%jmp", cons(makeatom("primitive"), makeinteger(28)), "%cmp", cons(makeatom("primitive"), makeinteger(29)), "%call", cons(makeatom("primitive"), makeinteger(30)), "%env-load", cons(makeatom("primitive"), makeinteger(31)), "%tail-call", cons(makeatom("primitive"), makeinteger(32)), "%define", cons(makeatom("primitive"), makeinteger(33)), "%set!", cons(makeatom("primitive"), makeinteger(34)), "%string", cons(makeatom("primitive"), makeinteger(50)), "%vector", cons(makeatom("primitive"), makeinteger(47)), "%make-vector", cons(makeatom("primitive"), makeinteger(48)), "%make-string", cons(makeatom("primitive"), makeinteger(49)), "%ap", cons(makeatom("primitive"), makeinteger(108)), "%append", cons(makeatom("primitive"), makeinteger(51)), "if", cons(makeatom("syntax"), makeatom("primitive-syntax-if")), "inexact?", cons(makeatom("primitive"), makeinteger(15)), "inexact->exact", cons(makeatom("primitive"), makeinteger(39)), "integer?", cons(makeatom("primitive"), makeinteger(19)), "imag-part", cons(makeatom("primitive"), makeinteger(61)), "fn", cons(makeatom("syntax"), makeatom("primitive-syntax-fn")), "floor", cons(makeatom("primitive"), makeinteger(36)), "first", cons(makeatom("primitive"), makeinteger(52)), "lambda", cons(makeatom("syntax"), makeatom("primitive-syntax-fn")), "length", cons(makeatom("primitive"), makeinteger(13)), "lcm", cons(makeatom("primitive"), makeinteger(23)), "list", cons(makeatom("syntax"), makeatom("primitive-syntax-list")), "ln", cons(makeatom("primitive"), makeinteger(82)), "log2", cons(makeatom("primitive"), makeinteger(87)), "log10", cons(makeatom("primitive"), makeinteger(88)), "quote", cons(makeatom("syntax"), makeatom("primitive-syntax-quote")), "quotient", cons(makeatom("primitive"), makeinteger(40)), "quasi-quote", cons(makeatom("syntax"), makeatom("primitive-syntax-qquote")), "unquote", cons(makeatom("syntax"), makeatom("primitve-syntax-unquote")), "unquote-splice", cons(makeatom("syntax"), makeatom("primitive-syntax-unqsplice")), "exact?", cons(makeatom("primitive"), makeinteger(14)), "exp", cons(makeatom("primitive"), makeinteger(81)), "exp2", cons(makeatom("primitive"), makeinteger(85)), "expm1", cons(makeatom("primitive"), makeinteger(86)), "eq?", cons(makeatom("primitive"), makeinteger(27)), "empty?", cons(makeatom("primitive"), makeinteger(59)), "display", cons(makeatom("primitive"), makeinteger(16)), "dict", cons(makeatom("primitive"), makeinteger(94)), "dict-has?", cons(makeatom("primitive"), makeinteger(96)), "denomenator", cons(makeatom("primitive"), makeinteger(25)), "define", cons(makeatom("syntax"), makeatom("primitive-syntax-define")), "define-syntax", cons(makeatom("syntax"), makeatom("primitive-syntax-defsyn")), "define-macro", cons(makeatom("syntax"), makeatom("primitive-syntax-defmac")), "apply", cons(makeatom("primitive"), makeinteger(17)), "append", cons(makeatom("syntax"), makeatom("primitive-syntax-append")), "argument", cons(makeatom("primitive"), makeinteger(66)), "asin", cons(makeatom("primitive"), makeinteger(74)), "assq", cons(makeatom("primitive"), makeinteger(92)), "acos", cons(makeatom("primitive"), makeinteger(75)), "atan", cons(makeatom("primitive"), makeinteger(76)), "atan2", cons(makeatom("primitive"), makeinteger(77)), "abs", cons(makeatom("primitive"), makeinteger(83)), "real?", cons(makeatom("primitive"), makeinteger(18)), "real-part", cons(makeatom("primitive"), makeinteger(62)), "rest", cons(makeatom("primitive"), makeinteger(53)), "rectangular->polar", cons(makeatom("primitive"), makeinteger(70)), "rational?", cons(makeatom("primitive"), makeinteger(21)), "rationalize", cons(makeatom("primitive"), makeinteger(105)), "round", cons(makeatom("primitive"), makeinteger(38)), "gcd", cons(makeatom("primitive"), makeinteger(22)), "gensym", cons(makeatom("primitive"), makeinteger(60)), "numerator", cons(makeatom("primitive"), makeinteger(24)), "nth", cons(makeatom("primitive"), makeinteger(55)), "+", cons(makeatom("syntax"), makeatom("primitive-syntax-plus")), "-", cons(makeatom("syntax"), makeatom("primitive-syntax-minus")), "*", cons(makeatom("syntax"), makeatom("primitive-syntax-mult")), "/", cons(makeatom("syntax"), makeatom("primitive-syntax-div")), "<", cons(makeatom("syntax"), makeatom("primitive-syntax-lt")), "<=", cons(makeatom("syntax"), makeatom("primitive-syntax-lte")), "<<", cons(makeatom("primitive"), makeinteger(89)), ">", cons(makeatom("syntax"), makeatom("primitive-syntax-gt")), ">=", cons(makeatom("syntax"), makeatom("primitive-syntax-gte")), ">>", cons(makeatom("primitive"), makeinteger(90)), "=", cons(makeatom("syntax"), makeatom("primitive-syntax-numeq")), "set!", cons(makeatom("syntax"), makeatom("primitive-syntax-set")), "string", cons(makeatom("syntax"), makeatom("primitive-syntax-string")), "string-append", cons(makeatom("primitive"), makeinteger(91)), "sin", cons(makeatom("primitive"), makeinteger(71)), "sinh", cons(makeatom("primitive"), makeinteger(78)), "sqrt", cons(makeatom("primitive"), makeinteger(84)), "truncate", cons(makeatom("primitive"), makeinteger(37)), "tan", cons(makeatom("primitive"), makeinteger(73)), "tanh", cons(makeatom("primitive"), makeinteger(80)), "tconc!", cons(makeatom("primitive"), makeinteger(100)), "tconc-list", cons(makeatom("primitive"), makeinteger(102)), "tconc->pair", cons(makeatom("primitive"), makeinteger(103)), "tconc-splice", cons(makeatom("primitive"), makeinteger(104)), "modulo", cons(makeatom("primitive"), makeinteger(41)), "make-vector", cons(makeatom("syntax"), makeatom("primitive-syntax-makevector")), "make-string", cons(makeatom("syntax"), makeatom("primitive-syntax-makestring")), "make-rectangular", cons(makeatom("primitive"), makeinteger(63)), "make-polar", cons(makeatom("primitive"), makeinteger(64)), "make-dict", cons(makeatom("primitive"), makeinteger(95)), "make-tconc", cons(makeatom("primitive"), makeinteger(101)), "magnitude", cons(makeatom("primitive"), makeinteger(65)), "memq", cons(makeatom("primitive"), makeinteger(93)), "&", cons(makeatom("primitive"), makeinteger(42)), "|", cons(makeatom("primitive"), makeinteger(43)), "^", cons(makeatom("primitive"), makeinteger(44)), "~", cons(makeatom("primitive"), makeinteger(45)), "vector", cons(makeatom("syntax"), makeatom("primitive-syntax-vector")), "keys", cons(makeatom("primitive"), makeinteger(56)), "partial-key?", cons(makeatom("primitive"), makeinteger(57)), "polar->rectangular", cons(makeatom("primitive"), makeinteger(69))));
SExp *
hydra_lookup(SExp *item, SExp *env){
    /*  look up item in the current environment, returning #f for not found
     */
    if((not(symbolp(item)) == STRUE)){
        return item;
    }
    else if((nullp(env) == STRUE)){
        return hydra_error(format(list(2, makestring("unbound variable: ~a"), item), enyalios93));
    }
    else if((fdicthas(car(env), item) == STRUE)){
        return fnth(list(2, car(env), item));
    }
    else {
        item65 = item;
        env66 = cdr(env);
        item = item65; 
        env = env66; 
    }
}
SExp *
compile_lambda(SExp *rst, SExp *env){
    return list(2, makeatom("compiled-lambda"), vector(3, list_copy(env), append_map(fun_106, cdr(rst)), car(rst)));
}
SExp *
hydra_lambdap(SExp *x){
    return (pairp(x) == STRUE) ? eqp(car(x), makeatom("compiled-lambda")) : SFALSE;
}
SExp *
hydra_primitivep(SExp *x){
    return (pairp(x) == STRUE) ? eqp(car(x), makeatom("primitive")) : SFALSE;
}
SExp *
hydra_syntaxp(SExp *x){
    return (pairp(x) == STRUE) ? eqp(car(x), makeatom("syntax")) : SFALSE;
}
SExp *
hydra_errorp(SExp *x){
    return (pairp(x) == STRUE) ? eqp(car(x), makeatom("error")) : SFALSE;
}
SExp *
hydra_continuationp(SExp *x){
    return (pairp(x) == STRUE) ? eqp(car(x), makeatom("continuation")) : SFALSE;
}
SExp *
hydra_add_env_(SExp *name, SExp *value, SExp *environment){
    /*  adds name to the environment, but also returns
      (load #v), so that the compiler adds the correct
      value (this is in the semantics of Vesta, so I thought
      it should be left in Hydra as well)
     */
    return fcset(car(environment), name, value);
}
SExp *
hydra_set_env_(SExp *name, SExp *value, SExp *environment){
    /*  sets a value in the current environment, and returns
      an error if that binding has not been previously defined
     */
    if((nullp(environment) == STRUE)){
        return hydra_error(format(list(2, makestring("SET! error: undefined name \"~a\""), name), enyalios93));
    }
    else if((fdicthas(car(environment), name) == STRUE)){
        return fcset(car(environment), name, value);
    }
    else {
        name77 = name;
        value78 = value;
        environment79 = cdr(environment);
        name = name77; 
        value = value78; 
        environment = environment79; 
    }
}
SExp *
reverse_append(SExp *x){
    /* append but in reverse
     */
    if((nullp(x) == STRUE)){
        return x;
    }
    else if((nullp(cdr(x)) == STRUE)){
        return car(x);
    }
    else {
        return fappend(list(3, reverse_append(cddr(x)), cadr(x), car(x)));
    }
}
SExp *
show(SExp *x){
    f_princ(list(1, makestring("show: ")), enyalios93);
    f_princ(list(1, x), enyalios93);
    f_princ(list(1, makestring("\n")), enyalios93);
    return x;
}
SExp *
hydra_error(SExp *msg){
    /* simple, hydra specific errors
     */
    return list(2, makeatom("error"), msg);
}
SExp *
hydra_eval(SExp *line, SExp *env){
    /* simple wrapper around hydra@vm & hydra@compile
     */
    return hydra_vm(hydra_compile(line, env), env, makeinteger(0), SNIL, SNIL);
}
SExp *
hydra_compile_help(SExp *sym, SExp *iter_list, SExp *env){
    /*  a helper function for hydra@compile, which collects
      the old use of append-map into a single function that
      Eprime can compile (still haven't added HOFs to E'...
      embarrassing, I know)
    
     */
    if((nullp(iter_list) == STRUE)){
        return iter_list;
    }
    else {
        return fappend(list(2, fappend(list(2, hydra_compile(car(iter_list), env), list(1, list(1, cdr(hydra_lookup(sym, env)))))), hydra_compile_help(sym, cdr(iter_list), env)));
    }
}
SExp *
hydra_compile(SExp *line, SExp *env){
    if((nullp(line) == STRUE)){
        return SNIL;
    }
    else {
        if((vectorp(line) == STRUE)){
            return list(1, list(2, makeinteger(3), line));
        }
        else if((dictp(line) == STRUE)){
            return list(1, list(2, makeinteger(3), line));
        }
        else if((symbolp(line) == STRUE)){
            return list(1, list(2, makeinteger(31), line));
        }
        else if((pairp(line) == STRUE)){
            SExp *tmp108 = car(line);
            SExp *tmp109 = hydra_lookup(fst, env);
            SExp *tmp110 = cdr(line);
            if((hydra_syntaxp(tmp109) == STRUE)){
                if((eqp(cdr(tmp109), makeatom("primitive-syntax-quote")) == STRUE)){
                    if((nullp(car(tmp110)) == STRUE)){
                        return list(1,list(1,makeinteger(4)));
                    }
                    else {
                        return list(1, list(2, makeinteger(3), car(tmp110)));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-plus")) == STRUE)){
                    return fappend(list(2, list(1,list(2,makeinteger(3),makeinteger(0))), hydra_compile_help(makeatom("%+"), tmp110, env)));
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-minus")) == STRUE)){
                    if((fnumeq(list(2, flength(tmp110), makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(2,makeinteger(3),makeinteger(0))), hydra_compile(car(tmp110), env), list(1, list(1, hydra_lookup(makeatom("%-"), env)))));
                    }
                    else if((fgt(list(2, flength(tmp110), makeinteger(1))) == STRUE)){
                        return fappend(list(2, hydra_compile(car(tmp110), env), hydra_compile_help(makeatom("%-"), cdr(tmp110), env)));
                    }
                    else {
                        return ferror(makestring("minus fail"));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-mult")) == STRUE)){
                    return fappend(list(2, list(1,list(2,makeinteger(3),makeinteger(1))), hydra_compile_help(makeatom("%*"), tmp110, env)));
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-div")) == STRUE)){
                    if((fnumeq(list(2, flength(tmp110), makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(2,makeinteger(3),makeinteger(1))), hydra_compile(car(tmp110), env), list(1, list(1, hydra_lookup(makeatom("%/"), env)))));
                    }
                    else if((fgt(list(2, flength(tmp110), makeinteger(1))) == STRUE)){
                        return fappend(list(2, hydra_compile(car(tmp110), env), hydra_compile_help(makeatom("%/"), cdr(tmp110), env)));
                    }
                    else {
                        return ferror(makestring("division fail"));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-numeq")) == STRUE)){
                    if((fnumeq(list(2, flength(tmp110), makeinteger(1))) == STRUE)){
                        return list(1, list(2, makeinteger(3), STRUE));
                    }
                    else if((fgt(list(2, flength(tmp110), makeinteger(1))) == STRUE)){
                        return fappend(list(2, hydra_compile(car(tmp110), env), hydra_compile_help(makeatom("%="), cdr(tmp110), env)));
                    }
                    else {
                        return ferror(makestring("numeq fail"));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-define")) == STRUE)){
                    SExp *tmp111 = car(tmp110);
                    SExp *tmp112 = cadr(tmp110);
                    if((pairp(tmp111) == STRUE)){
                        return fappend(list(3, hydra_compile(cons(makeatom("fn"), cons(cdar(tmp110), cdr(tmp110))), env), list(1, list(2, makeinteger(3), caar(tmp110))), list(1, list(1, cdr(hydra_lookup(makeatom("%define"), env))))));
                    }
                    else if((symbolp(tmp111) == STRUE)){
                        return fappend(list(3, hydra_compile(tmp112, env), list(1, list(2, makeinteger(3), tmp111)), list(1, list(1, cdr(hydra_lookup(makeatom("%define"), env))))));
                    }
                    else {
                        return ferror(makestring("DEFINE error: define SYMBOL VALUE | DEFINE PAIR S-EXPR*"));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-set")) == STRUE)){
                    SExp *tmp113 = car(tmp110);
                    SExp *tmp114 = cadr(tmp110);
                    if((symbolp(tmp113) == STRUE)){
                        return fappend(list(3, hydra_compile(tmp114, env), list(1, list(2, makeinteger(3), tmp113)), list(1, list(1, cdr(hydra_lookup(makeatom("%set!"), env))))));
                    }
                    else {
                        return ferror(makestring("SET!: set! SYMBOL S-EXPR*"));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-defsyn")) == STRUE)){
                    return STRUE;
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-defmac")) == STRUE)){
                    return STRUE;
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-fn")) == STRUE)){
                    return list(1, list(2, makeinteger(3), compile_lambda(tmp110, env)));
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-lt")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp110), env), hydra_compile_help(makeatom("%<"), cdr(tmp110), env)));
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-gt")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp110), env), hydra_compile_help(makeatom("%>"), cdr(tmp110), env)));
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-lte")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp110), env), hydra_compile_help(makeatom("%<="), cdr(tmp110), env)));
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-gte")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp110), env), hydra_compile_help(makeatom("%>="), cdr(tmp110), env)));
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-list")) == STRUE)){
                    if((nullp(tmp110) == STRUE)){
                        return list(1, list(1, makeinteger(4)));
                    }
                    else {
                        return fappend(list(3, reverse_append(map(fun_115, tmp110)), list(1, list(2, makeinteger(3), flength(tmp110))), list(1, list(1, cdr(hydra_lookup(makeatom("%list"), env))))));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-vector")) == STRUE)){
                    if((nullp(tmp110) == STRUE)){
                        return list(1, list(1, makeinteger(4)));
                    }
                    else {
                        return fappend(list(3, reverse_append(map(fun_117, tmp110)), list(1, list(2, makeinteger(3), flength(tmp110))), list(1, list(1, cdr(hydra_lookup(makeatom("%vector"), env))))));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-makevector")) == STRUE)){
                    SExp *tmp119 = flength(tmp110);
                    if((fnumeq(list(2, tmp119, makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(1,makeinteger(4))), hydra_compile(car(tmp110), env), list(1, list(1, cdr(hydra_lookup(makeatom("%make-vector"), env))))));
                    }
                    else if((fnumeq(list(2, tmp119, makeinteger(2))) == STRUE)){
                        return fappend(list(2, reverse_append(map(fun_120, tmp110)), list(1, list(1, cdr(hydra_lookup(makeatom("%make-vector"), env))))));
                    }
                    else {
                        return hydra_error(makestring("make-vector len : INTEGER (v : SEXPR) => VECTOR"));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-makestring")) == STRUE)){
                    SExp *tmp122 = flength(tmp110);
                    if((fnumeq(list(2, tmp122, makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(2,makeinteger(3),makechar(' '))), hydra_compile(car(tmp110), env), list(1, list(1, cdr(hydra_lookup(makeatom("%make-string"), env))))));
                    }
                    else if((fnumeq(list(2, tmp122, makeinteger(2))) == STRUE)){
                        return fappend(list(2, reverse_append(map(fun_123, tmp110)), list(1, list(1, cdr(hydra_lookup(makeatom("%make-string"), env))))));
                    }
                    else {
                        return hydra_error(makestring("make-string len : INTEGER (c : CHAR) => STRING"));
                    }
                }
                else if((eqp(cdr(tmp109), makeatom("primitive-syntax-if")) == STRUE)){
                    SExp *tmp125 = hydra_compile(car(tmp110), env);
                    SExp *tmp126 = hydra_compile(cadr(tmp110), env);
                    SExp *tmp127 = hydra_compile(caddr(tmp110), env);
                    SExp *tmp128 = fplus(list(2, flength(tmp126), makeinteger(2)));
                    SExp *tmp129 = fplus(list(2, flength(tmp127), makeinteger(1)));
                    return fappend(list(5, tmp125, list(1, list(2, makeinteger(29), tmp128)), tmp126, list(1, list(2, makeinteger(28), tmp129)), tmp127));
                }
                else {
                    return STRUE;
                }
            }
            else if((pairp(tmp108) == STRUE)){
                return fappend(list(3, reverse_append(map(fun_130, tmp110)), hydra_compile(tmp108, env), list(1, list(1, makeinteger(30)))));
            }
            else if((hydra_primitivep(tmp109) == STRUE)){
                return fappend(list(2, reverse_append(map(fun_132, tmp110)), list(1, list(1, cdr(tmp109)))));
            }
            else if((hydra_lambdap(tmp109) == STRUE)){
                return fappend(list(3, reverse_append(map(fun_134, tmp110)), list(1, list(2, makeinteger(3), tmp109)), list(1, list(1, makeinteger(30)))));
            }
            else if((hydra_continuationp(tmp109) == STRUE)){
                return fappend(list(3, reverse_append(map(fun_136, tmp110)), list(1, list(2, makeinteger(3), tmp109)), list(1, list(1, makeinteger(108)))));
            }
            else if((symbolp(tmp108) == STRUE)){
                return fappend(list(3, reverse_append(map(fun_138, tmp110)), list(1, list(2, makeinteger(31), tmp108)), list(1, list(1, makeinteger(30)))));
            }
            else {
                return ferror(makestring("error: the only applicable types are primitive procedures, closures & syntax"));
            }
        }
        else {
            return list(1, list(2, makeinteger(3), line));
        }
    }
}
SExp *
top_level_print(SExp *x){
    /*  print #<foo> at the top level
     */
    if((hydra_lambdap(x) == STRUE)){
        return f_princ(list(1, makestring("#<closure>")), enyalios93);
    }
    else if((hydra_continuationp(x) == STRUE)){
        return f_princ(list(1, makestring("#<continuation>")), enyalios93);
    }
    else if((hydra_primitivep(x) == STRUE)){
        return f_princ(list(1, format(list(2, makestring("#<primitive-procedure ~a>"), cdr(x)), enyalios93)), enyalios93);
    }
    else if((hydra_syntaxp(x) == STRUE)){
        return f_princ(list(1, format(list(2, makestring("#<syntax ~a>"), cdr(x)), enyalios93)), enyalios93);
    }
    else if((hydra_errorp(x) == STRUE)){
        return f_princ(list(1, format(list(2, makestring("ERROR: ~a"), cdr(x)), enyalios93)), enyalios93);
    }
    else {
        return f_princ(list(1, x), enyalios93);
    }
}
SExp *
hydra_load(SExp *src_file, SExp *env){
    /* an implementation of the primitive procedure load
     */
    return SFALSE;
}
SExp *
hydra_repl(){
while(1) {
        f_princ(list(1, makestring("h; ")), enyalios93);
        SExp *tmp140 = f_read(SNILenyalios93);
        if((eqp(ftype(tmp140), makestring("Pair")) == STRUE) && (eqp(car(tmp140), makeatom("unquote")) == STRUE)){
            if((eqp(cadr(tmp140), makeatom("exit")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp140), makeatom("q")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp140), makeatom("quit")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp140), makeatom("bye")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp140), makeatom("dribble")) == STRUE)){
                continue;
            }
            else if((eqp(cadr(tmp140), makeatom("save")) == STRUE)){
                continue;
            }
            else if((eqp(cadr(tmp140), makeatom("save-and-die")) == STRUE)){
                continue;
            }
            else {
                f_princ(list(1, format(list(2, makestring("Unknown command: ~a~%"), cadr(tmp140)), enyalios93)), enyalios93);
                continue;
            }
        }
        else {
            if((not(pairp(tmp140)) == STRUE)){
                if((eqp(tmp140, SVOID) == STRUE)){
                    continue;
                }
                else {
                    top_level_print(hydra_lookup(tmp140, _tlenv_));
                    f_princ(list(1, makestring("\n")), enyalios93);
                    continue;
                }
            }
            else {
                SExp *tmp141 = hydra_eval(tmp140, _tlenv_);
                if((eqp(tmp141, SVOID) == STRUE)){
                    continue;
                }
                else {
                    top_level_print(tmp141);
                    f_princ(list(1, makestring("\n")), enyalios93);
                    continue;
                }
            }
        }
    }
}
SExp *
hydra_main(){
    f_princ(list(1, makestring("\n\t()\n\t  ()\n\t()  ()\nDigamma/Hydra: 2012.0/r0\n")), enyalios93);
    return hydra_repl();
}

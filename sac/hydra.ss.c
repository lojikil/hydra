#include "murt.h"

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
SExp *compile_lambda_helper(SExp *, SExp *);
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
SExp *loop_set_env_(SExp *, SExp *);
SExp *map(SExp *, SExp *);
SExp *foreach(SExp *, SExp *);
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
SExp *hydra_map(SExp *, SExp *);
SExp *hydra_main();
SExp *hydra_repl();
SExp *reverse_append(SExp *);
SExp *top_level_print(SExp *);
Symbol *enyalios99;

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
loop_set_env_(SExp *env, SExp *pairs){
        while(1) {
        if((nullp(pairs) == STRUE)){
            return SVOID;
        }
        else {
            fcset(env, caar(pairs), cadar(pairs));
            env54 = env;
            pairs55 = cdr(pairs);
            env = env54; 
            pairs = pairs55; 
        }
    }
}
SExp *
build_environment(SExp *environment, SExp *stack, SExp *params){
    /* Adds a new window to the environment, removes |params| items from the stack
     and binds those values in the new window. It returns a list of environment and
     the new stack.
     */
    SExp *tmp100 = flength(stack);
    SExp *tmp101 = flength(params);
    SExp *tmp102 = makedict();
    if((flt(list(2, tmp100, tmp101)) == STRUE)){
        return ferror(makestring("non-optional parameters are not statisfied by stack items in build-environment"));
    }
    else {
        if((fnumeq(list(2, tmp101, makeinteger(0))) == STRUE)){
            return list(2, cons(tmp102, environment), cdr(stack));
        }
        else {
            loop_set_env_(tmp102, zip(params, fcslice(stack, makeinteger(0), tmp101)));
            return list(2, cons(tmp102, environment), fcslice(stack, tmp101, tmp100));
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
                code62 = caar(dump);
                env63 = cadar(dump);
                ip64 = fplus(list(2, caddar(dump), makeinteger(1)));
                stack65 = cons(car(stack), cadddar(dump));
                dump66 = cdr(dump);
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
        }
        else {
            SExp *tmp103 = fnth(list(2, code, ip));
            SExp *tmp104 = hydra_instruction(c);
            if((eqp(tmp104, makeinteger(0)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(car(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(1)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(cdr(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(2)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(cons(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(3)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(hydra_operand(tmp103), stack);
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(4)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(SNIL, stack);
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(5)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fsubt(list(2, cadr(stack), car(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(6)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fplus(list(2, car(stack), cadr(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(7)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fmult(list(2, car(stack), cadr(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(8)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fdivi(list(2, cadr(stack), car(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(9)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(flt(list(2, cadr(stack), car(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(10)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fgt(list(2, cadr(stack), car(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(11)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(flte(list(2, cadr(stack), car(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(12)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fgte(list(2, cadr(stack), car(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(13)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(flength(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(14)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexactp(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(15)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(finexactp(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(16)) == STRUE)){
                f_princ(list(1, car(stack)), enyalios99);
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(SVOID, cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(18)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(frealp(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(19)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fintegerp(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(20)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcomplexp(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(21)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(frationalp(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(22)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fgcd(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(23)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(flcm(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(24)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fnum(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(25)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fden(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(26)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fnumeq(list(2, car(stack), cadr(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(27)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(eqp(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(28)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, hydra_operand(tmp103)));
                stack65 = stack;
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(29)) == STRUE)){
                if((car(stack) == STRUE)){
                    code62 = code;
                    env63 = env;
                    ip64 = fplus(list(2, ip, makeinteger(1)));
                    stack65 = cdr(stack);
                    dump66 = dump;
                    code = code62; 
                    env = env63; 
                    ip = ip64; 
                    stack = stack65; 
                    dump = dump66; 
                }
                else {
                    code62 = code;
                    env63 = env;
                    ip64 = fplus(list(2, ip, hydra_operand(tmp103)));
                    stack65 = cdr(stack);
                    dump66 = dump;
                    code = code62; 
                    env = env63; 
                    ip = ip64; 
                    stack = stack65; 
                    dump = dump66; 
                }
            }
            else if((eqp(tmp104, makeinteger(30)) == STRUE)){
                if((hydra_lambdap(car(stack)) == STRUE)){
                    SExp *tmp105 = build_environment(fnth(list(2, cadar(stack), makeinteger(0))), cdr(stack), fnth(list(2, cadar(stack), makeinteger(2))));
                    code62 = fnth(list(2, cadar(stack), makeinteger(1)));
                    env63 = car(tmp105);
                    ip64 = makeinteger(0);
                    stack65 = SNIL;
                    dump66 = cons(list(4, code, env, ip, cadr(tmp105)), dump);
                    code = code62; 
                    env = env63; 
                    ip = ip64; 
                    stack = stack65; 
                    dump = dump66; 
                }
                else if((hydra_primitivep(car(stack)) == STRUE)){
                    f_princ(list(1, makestring("in hydra@primitive\n\t")), enyalios99);
                    f_princ(list(1, car(stack)), enyalios99);
                    f_princ(list(1, makestring("\n")), enyalios99);
                    return STRUE;
                }
                else {
                    f_princ(list(1, makestring("in <else> of CALL\n")), enyalios99);
                    return SFALSE;
                }
            }
            else if((eqp(tmp104, makeinteger(31)) == STRUE)){
                SExp *tmp106 = hydra_lookup(hydra_operand(tmp103), env);
                if((eqp(tmp106, SFALSE) == STRUE)){
                    return SFALSE;
                }
                else {
                    code62 = code;
                    env63 = env;
                    ip64 = fplus(list(2, ip, makeinteger(1)));
                    stack65 = cons(tmp106, stack);
                    dump66 = dump;
                    code = code62; 
                    env = env63; 
                    ip = ip64; 
                    stack = stack65; 
                    dump = dump66; 
                }
            }
            else if((eqp(tmp104, makeinteger(32)) == STRUE)){
                if((not(nullp(stack)) == STRUE) && (eqp(caar(stack), makeatom("compiled-lambda")) == STRUE)){
                    code62 = fnth(list(2, cdar(stack), makeinteger(0)));
                    env63 = fnth(list(2, cdar(stack), makeinteger(1)));
                    ip64 = makeinteger(0);
                    stack65 = SNIL;
                    dump66 = dump;
                    code = code62; 
                    env = env63; 
                    ip = ip64; 
                    stack = stack65; 
                    dump = dump66; 
                }
                else {
                    return SFALSE;
                }
            }
            else if((eqp(tmp104, makeinteger(33)) == STRUE)){
                hydra_add_env_(car(stack), cadr(stack), env);
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(SVOID, stack);
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(34)) == STRUE)){
                hydra_set_env_(car(stack), cadr(stack), env);
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(SVOID, stack);
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(35)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fceil(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(36)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(ffloor(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(37)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(ftruncate(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(38)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fround(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(39)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fin2ex(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(40)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fquotient(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(41)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fmodulo(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(42)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fbitand(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(43)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fbitor(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(44)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fbitxor(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(45)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fbitnot(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(46)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcslice(cdr(stack), makeinteger(0), car(stack)), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(47)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcoerce(fcslice(cdr(stack), makeinteger(0), car(stack)), makeatom("vector")), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(48)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fmkvector(list(2, car(stack), cadr(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(49)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fmakestring(list(2, car(stack), cadr(stack))), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(50)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fstring(fcslice(cdr(stack), makeinteger(0), car(stack))), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(51)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fappend(fcslice(cdr(stack), makeinteger(0), car(stack))), fcslice(cdr(stack), car(stack), fsubt(list(2, flength(stack), makeinteger(1)))));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(52)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(ffirst(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(53)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(frest(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(54)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fccons(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(55)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fnth(list(3, caddr(stack), cadr(stack), car(stack))), cdddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(56)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fkeys(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(57)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fpartial_key(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(58)) == STRUE)){
                fcset(caddr(stack), cadr(stack), car(stack));
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(SVOID, cdddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(59)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fempty(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(60)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(gensym(list(1, car(stack))), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(61)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fimag_part(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(62)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(freal_part(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(63)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fmake_rect(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(64)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fmake_pole(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(65)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fmag(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(66)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(farg(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(67)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fconjugate(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(68)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fconjugate_bang(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(69)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fpol2rect(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(70)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(frect2pol(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(71)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fsin(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(72)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcos(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(73)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(ftan(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(74)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fasin(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(75)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(facos(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(76)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fatan(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(77)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fatan2(cadr(stack), car(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(78)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fsinh(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(79)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcosh(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(80)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(ftanh(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(81)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(82)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fln(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(83)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fnabs(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(84)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fsqrt(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(85)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(86)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexpm1(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(87)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(flog2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(88)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(flog10(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(89)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fbitshl(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(90)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fbitshr(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(91)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(92)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(assq(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(93)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(memq(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(94)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(95)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fdict(SNIL), stack);
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(96)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fdicthas(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(97)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcoerce(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(98)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcupdate(car(stack), cadr(stack), caddr(stack)), cdddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(99)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fcslice(car(stack), cadr(stack), caddr(stack)), cdddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(100)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(101)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(102)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(103)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(104)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(fexp2(car(stack)), cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(105)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(frationalize(car(stack), cadr(stack)), cddr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(106)) == STRUE)){
                SExp *tmp107 = hydra_vm(cons(list(2, makeinteger(3), car(stack)), list(1, list(1, makeinteger(30)))), env, makeinteger(0), cons(list(6, makeatom("continuation"), copy_code(code, ip, makeinteger(0)), ip, env, stack, dump), SNIL), SNIL);
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = cons(tmp107, cdr(stack));
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(107)) == STRUE)){
                code62 = code;
                env63 = env;
                ip64 = fplus(list(2, ip, makeinteger(1)));
                stack65 = stack;
                dump66 = dump;
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
            }
            else if((eqp(tmp104, makeinteger(108)) == STRUE)){
                SExp *tmp108 = car(stack);
                SExp *tmp109 = cadr(stack);
                code62 = fnth(list(2, tmp108, makeinteger(1)));
                env63 = fnth(list(2, tmp108, makeinteger(3)));
                ip64 = makeinteger(0);
                stack65 = cons(tmp109, fnth(list(2, tmp108, makeinteger(4))));
                dump66 = fnth(list(2, tmp108, makeinteger(5)));
                code = code62; 
                env = env63; 
                ip = ip64; 
                stack = stack65; 
                dump = dump66; 
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
        return hydra_error(format(list(2, makestring("unbound variable: ~a"), item), enyalios99));
    }
    else if((fdicthas(car(env), item) == STRUE)){
        return fnth(list(2, car(env), item));
    }
    else {
        item67 = item;
        env68 = cdr(env);
        item = item67; 
        env = env68; 
    }
}
SExp *
compile_lambda_helper(SExp *lst, SExp *env){
    if((nullp(lst) == STRUE)){
        return SNIL;
    }
    else {
        return fappend(list(2, hydra_compile(car(lst), env), compile_lambda_helper(cdr(lst), env)));
    }
}
SExp *
compile_lambda(SExp *rst, SExp *env){
    return list(2, makeatom("compiled-lambda"), vector(3, list_copy(env), compile_lambda_helper(cdr(rst), env), car(rst)));
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
        return hydra_error(format(list(2, makestring("SET! error: undefined name \"~a\""), name), enyalios99));
    }
    else if((fdicthas(car(environment), name) == STRUE)){
        return fcset(car(environment), name, value);
    }
    else {
        name81 = name;
        value82 = value;
        environment83 = cdr(environment);
        name = name81; 
        value = value82; 
        environment = environment83; 
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
    f_princ(list(1, makestring("show: ")), enyalios99);
    f_princ(list(1, x), enyalios99);
    f_princ(list(1, makestring("\n")), enyalios99);
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
        return fappend(list(3, hydra_compile(car(iter_list), env), list(1, list(1, cdr(hydra_lookup(sym, env)))), hydra_compile_help(sym, cdr(iter_list), env)));
    }
}
SExp *
hydra_map(SExp *iter_list, SExp *env){
    if((nullp(iter_list) == STRUE)){
        return iter_list;
    }
    else {
        return cons(hydra_compile(car(iter_list), env), hydra_map(cdr(iter_list), env));
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
            SExp *tmp110 = car(line);
            SExp *tmp111 = hydra_lookup(fst, env);
            SExp *tmp112 = cdr(line);
            if((hydra_syntaxp(tmp111) == STRUE)){
                if((eqp(cdr(tmp111), makeatom("primitive-syntax-quote")) == STRUE)){
                    if((nullp(car(tmp112)) == STRUE)){
                        return list(1,list(1,makeinteger(4)));
                    }
                    else {
                        return list(1, list(2, makeinteger(3), car(tmp112)));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-plus")) == STRUE)){
                    return fappend(list(2, list(1,list(2,makeinteger(3),makeinteger(0))), hydra_compile_help(makeatom("%+"), tmp112, env)));
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-minus")) == STRUE)){
                    if((fnumeq(list(2, flength(tmp112), makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(2,makeinteger(3),makeinteger(0))), hydra_compile(car(tmp112), env), list(1, list(1, hydra_lookup(makeatom("%-"), env)))));
                    }
                    else if((fgt(list(2, flength(tmp112), makeinteger(1))) == STRUE)){
                        return fappend(list(2, hydra_compile(car(tmp112), env), hydra_compile_help(makeatom("%-"), cdr(tmp112), env)));
                    }
                    else {
                        return ferror(makestring("minus fail"));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-mult")) == STRUE)){
                    return fappend(list(2, list(1,list(2,makeinteger(3),makeinteger(1))), hydra_compile_help(makeatom("%*"), tmp112, env)));
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-div")) == STRUE)){
                    if((fnumeq(list(2, flength(tmp112), makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(2,makeinteger(3),makeinteger(1))), hydra_compile(car(tmp112), env), list(1, list(1, hydra_lookup(makeatom("%/"), env)))));
                    }
                    else if((fgt(list(2, flength(tmp112), makeinteger(1))) == STRUE)){
                        return fappend(list(2, hydra_compile(car(tmp112), env), hydra_compile_help(makeatom("%/"), cdr(tmp112), env)));
                    }
                    else {
                        return ferror(makestring("division fail"));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-numeq")) == STRUE)){
                    if((fnumeq(list(2, flength(tmp112), makeinteger(1))) == STRUE)){
                        return list(1, list(2, makeinteger(3), STRUE));
                    }
                    else if((fgt(list(2, flength(tmp112), makeinteger(1))) == STRUE)){
                        return fappend(list(2, hydra_compile(car(tmp112), env), hydra_compile_help(makeatom("%="), cdr(tmp112), env)));
                    }
                    else {
                        return ferror(makestring("numeq fail"));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-define")) == STRUE)){
                    SExp *tmp113 = car(tmp112);
                    SExp *tmp114 = cadr(tmp112);
                    if((pairp(tmp113) == STRUE)){
                        return fappend(list(3, hydra_compile(cons(makeatom("fn"), cons(cdar(tmp112), cdr(tmp112))), env), list(1, list(2, makeinteger(3), caar(tmp112))), list(1, list(1, cdr(hydra_lookup(makeatom("%define"), env))))));
                    }
                    else if((symbolp(tmp113) == STRUE)){
                        return fappend(list(3, hydra_compile(tmp114, env), list(1, list(2, makeinteger(3), tmp113)), list(1, list(1, cdr(hydra_lookup(makeatom("%define"), env))))));
                    }
                    else {
                        return ferror(makestring("DEFINE error: define SYMBOL VALUE | DEFINE PAIR S-EXPR*"));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-set")) == STRUE)){
                    SExp *tmp115 = car(tmp112);
                    SExp *tmp116 = cadr(tmp112);
                    if((symbolp(tmp115) == STRUE)){
                        return fappend(list(3, hydra_compile(tmp116, env), list(1, list(2, makeinteger(3), tmp115)), list(1, list(1, cdr(hydra_lookup(makeatom("%set!"), env))))));
                    }
                    else {
                        return ferror(makestring("SET!: set! SYMBOL S-EXPR*"));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-defsyn")) == STRUE)){
                    return STRUE;
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-defmac")) == STRUE)){
                    return STRUE;
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-fn")) == STRUE)){
                    return list(1, list(2, makeinteger(3), compile_lambda(tmp112, env)));
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-lt")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp112), env), hydra_compile_help(makeatom("%<"), cdr(tmp112), env)));
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-gt")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp112), env), hydra_compile_help(makeatom("%>"), cdr(tmp112), env)));
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-lte")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp112), env), hydra_compile_help(makeatom("%<="), cdr(tmp112), env)));
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-gte")) == STRUE)){
                    return fappend(list(2, hydra_compile(car(tmp112), env), hydra_compile_help(makeatom("%>="), cdr(tmp112), env)));
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-list")) == STRUE)){
                    if((nullp(tmp112) == STRUE)){
                        return list(1, list(1, makeinteger(4)));
                    }
                    else {
                        return fappend(list(3, reverse_append(hydra_map(tmp112, env)), list(1, list(2, makeinteger(3), flength(tmp112))), list(1, list(1, cdr(hydra_lookup(makeatom("%list"), env))))));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-vector")) == STRUE)){
                    if((nullp(tmp112) == STRUE)){
                        return list(1, list(1, makeinteger(4)));
                    }
                    else {
                        return fappend(list(3, reverse_append(hydra_map(tmp112, env)), list(1, list(2, makeinteger(3), flength(tmp112))), list(1, list(1, cdr(hydra_lookup(makeatom("%vector"), env))))));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-makevector")) == STRUE)){
                    SExp *tmp117 = flength(tmp112);
                    if((fnumeq(list(2, tmp117, makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(1,makeinteger(4))), hydra_compile(car(tmp112), env), list(1, list(1, cdr(hydra_lookup(makeatom("%make-vector"), env))))));
                    }
                    else if((fnumeq(list(2, tmp117, makeinteger(2))) == STRUE)){
                        return fappend(list(2, reverse_append(hydra_map(tmp112, env)), list(1, list(1, cdr(hydra_lookup(makeatom("%make-vector"), env))))));
                    }
                    else {
                        return hydra_error(makestring("make-vector len : INTEGER (v : SEXPR) => VECTOR"));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-makestring")) == STRUE)){
                    SExp *tmp118 = flength(tmp112);
                    if((fnumeq(list(2, tmp118, makeinteger(1))) == STRUE)){
                        return fappend(list(3, list(1,list(2,makeinteger(3),makechar(' '))), hydra_compile(car(tmp112), env), list(1, list(1, cdr(hydra_lookup(makeatom("%make-string"), env))))));
                    }
                    else if((fnumeq(list(2, tmp118, makeinteger(2))) == STRUE)){
                        return fappend(list(2, reverse_append(hydra_map(tmp112, env)), list(1, list(1, cdr(hydra_lookup(makeatom("%make-string"), env))))));
                    }
                    else {
                        return hydra_error(makestring("make-string len : INTEGER (c : CHAR) => STRING"));
                    }
                }
                else if((eqp(cdr(tmp111), makeatom("primitive-syntax-if")) == STRUE)){
                    SExp *tmp119 = hydra_compile(car(tmp112), env);
                    SExp *tmp120 = hydra_compile(cadr(tmp112), env);
                    SExp *tmp121 = hydra_compile(caddr(tmp112), env);
                    SExp *tmp122 = fplus(list(2, flength(tmp120), makeinteger(2)));
                    SExp *tmp123 = fplus(list(2, flength(tmp121), makeinteger(1)));
                    return fappend(list(5, tmp119, list(1, list(2, makeinteger(29), tmp122)), tmp120, list(1, list(2, makeinteger(28), tmp123)), tmp121));
                }
                else {
                    return STRUE;
                }
            }
            else if((pairp(tmp110) == STRUE)){
                return fappend(list(3, reverse_append(hydra_map(tmp112, env)), hydra_compile(tmp110, env), list(1, list(1, makeinteger(30)))));
            }
            else if((hydra_primitivep(tmp111) == STRUE)){
                return fappend(list(2, reverse_append(hydra_map(tmp112, env)), list(1, list(1, cdr(tmp111)))));
            }
            else if((hydra_lambdap(tmp111) == STRUE)){
                return fappend(list(3, reverse_append(hydra_map(tmp112, env)), list(1, list(2, makeinteger(3), tmp111)), list(1, list(1, makeinteger(30)))));
            }
            else if((hydra_continuationp(tmp111) == STRUE)){
                return fappend(list(3, reverse_append(hydra_map(tmp112, env)), list(1, list(2, makeinteger(3), tmp111)), list(1, list(1, makeinteger(108)))));
            }
            else if((symbolp(tmp110) == STRUE)){
                return fappend(list(3, reverse_append(hydra_map(tmp112, env)), list(1, list(2, makeinteger(31), tmp110)), list(1, list(1, makeinteger(30)))));
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
        return f_princ(list(1, makestring("#<closure>")), enyalios99);
    }
    else if((hydra_continuationp(x) == STRUE)){
        return f_princ(list(1, makestring("#<continuation>")), enyalios99);
    }
    else if((hydra_primitivep(x) == STRUE)){
        return f_princ(list(1, format(list(2, makestring("#<primitive-procedure ~a>"), cdr(x)), enyalios99)), enyalios99);
    }
    else if((hydra_syntaxp(x) == STRUE)){
        return f_princ(list(1, format(list(2, makestring("#<syntax ~a>"), cdr(x)), enyalios99)), enyalios99);
    }
    else if((hydra_errorp(x) == STRUE)){
        return f_princ(list(1, format(list(2, makestring("ERROR: ~a"), cdr(x)), enyalios99)), enyalios99);
    }
    else {
        return f_princ(list(1, x), enyalios99);
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
        f_princ(list(1, makestring("h; ")), enyalios99);
        SExp *tmp124 = f_read(SNIL, enyalios99);
        if((eqp(ftype(tmp124), makestring("Pair")) == STRUE) && (eqp(car(tmp124), makeatom("unquote")) == STRUE)){
            if((eqp(cadr(tmp124), makeatom("exit")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp124), makeatom("q")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp124), makeatom("quit")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp124), makeatom("bye")) == STRUE)){
                return SVOID;
            }
            else if((eqp(cadr(tmp124), makeatom("dribble")) == STRUE)){
                continue;
            }
            else if((eqp(cadr(tmp124), makeatom("save")) == STRUE)){
                continue;
            }
            else if((eqp(cadr(tmp124), makeatom("save-and-die")) == STRUE)){
                continue;
            }
            else {
                f_princ(list(1, format(list(2, makestring("Unknown command: ~a~%"), cadr(tmp124)), enyalios99)), enyalios99);
                continue;
            }
        }
        else {
            if((not(pairp(tmp124)) == STRUE)){
                if((eqp(tmp124, SVOID) == STRUE)){
                    continue;
                }
                else {
                    top_level_print(hydra_lookup(tmp124, _tlenv_));
                    f_princ(list(1, makestring("\n")), enyalios99);
                    continue;
                }
            }
            else {
                SExp *tmp125 = hydra_eval(tmp124, _tlenv_);
                if((eqp(tmp125, SVOID) == STRUE)){
                    continue;
                }
                else {
                    top_level_print(tmp125);
                    f_princ(list(1, makestring("\n")), enyalios99);
                    continue;
                }
            }
        }
    }
}
SExp *
hydra_main(){
    f_princ(list(1, makestring("\n\t()\n\t  ()\n\t()  ()\nDigamma/Hydra: 2012.0/r0\n")), enyalios99);
    return hydra_repl();
}

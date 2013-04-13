/* collection of POSIX integration routines;
 * basically, just keep the core clean of POSIXy
 * things as much as possible
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
extern int quit_note;
extern const char **typenames;
extern const char **numtypes;
#ifndef CYGWIN
extern const char **environ;
#endif
SExp *
interrogate(SExp *s, Symbol *env)
{
	char buf[256] = {0};
	SExp *tmp = env->snil;
	int iter = 0, l = 0;
	if(pairlength(s) == 1)
	{
		tmp = car(s);
		if(tmp->type != ATOM)
			return makeerror(2,0,"interrogate requires a symbol or nil args (for an interactive prompt)");
		tmp = symlookup(tmp->object.str,env);
		if(tmp == nil)
			return makeerror(2,0,"undfefined atom passed to interrogate");
		printf("Object: %s\n",mcar(s)->object.str);
		printf("Type: %d\n",tmp->type);
		if(tmp->type == NUMBER)
			printf("Numeric type: %d\n",NTYPE(tmp));
		printf("Value: ");
		princ(tmp);
		printf("\n");
		return env->svoid;
	}
	fgets(buf,256,stdin); /* hackish way to catch the \n comming off of llread...\n" */
	printf("Welcome to INTERROGATE, the F debugger (of sorts)...\n");
	/* should really add stack features to this & integrate it into the core 
	 * interpreter, rather than as a light-weight as it currently is.
	 * Would need to be able to walk the call stack, but that's not a big deal...
	 */
	while(1)
	{
		printf("INTERROGATE> ");
		fgets(buf,256,stdin);
		if(feof(stdin))
		{
			printf("Recieved eof in interrogate; clearing error & returning to main interaction...\n");
			clearerr(stdin);
			break;
		}
		if(!strncasecmp(buf,"help",4))
		{
			printf("INTERROGATE help\n");
			printf("type: type of argument\nval: value of argument\nquit : quits INTERROGATE\n");
		}
		else if(!strncasecmp(buf,"quit",4))
			break;
		else if(!strncasecmp(buf,"type",4))
		{
			l = strlen(&buf[5]);
			for(iter = 5;iter < (l + 5);iter++)
				if(buf[iter] == '\n')
					buf[iter] = '\0';
			tmp = symlookup(&buf[5],env);
			if(tmp != nil)
				printf("%s->type == %d\n",&buf[5],tmp->type);
			else
				printf("symlookup returned nil (!!)\n");
		}
        else if(!strncasecmp(buf,"splay",5))
        {

        }
		else if(!strncasecmp(buf,"val",3))
		{
			l = strlen(&buf[4]);
			for(iter = 4;iter < (l + 4);iter++)
				if(buf[iter] == '\n')
					buf[iter] = '\0';
			tmp = symlookup(&buf[4],env);
			if(tmp != nil)
			{
				princ(tmp);
				printf("\n");
			}
			else
				printf("symlookup returned nil (!!)\n");
		}
	}
	printf("returning from INTERROGATE...\n");
	return env->svoid;
}
SExp *
f_system(SExp *src, Symbol *env)
{
	int len = pairlength(src), ret = 0;
	SExp *f = env->snil;
	switch(len)
	{
		case 0:
			ret = system(nil);
			break;
		case 1:
			f = car(src);
			if(f->type != STRING)
				return makeerror(2,0,"system s : STRING => INTEGER");
			ret = system(f->object.str);
			break;
		default:
			return makeerror(2,0,"system s : STRING => INTEGER");
	}
	f = makenumber(INTEGER);
	f->object.n->nobject.z = ret;
	return f;
}
SExp *
f_pwd(SExp *src, Symbol *env)
{
	char *buf = nil;
	SExp *ret = env->snil;
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = STRING;
	ret->object.str = (char *)hmalloc(sizeof(char) * MAXPATHLEN);
	ret->object.str = getcwd(ret->object.str,MAXPATHLEN);
	ret->length = strlen(ret->object.str);
	return ret;
}
SExp *
f_ls(SExp *src, Symbol *env)
{
	SExp *ret = env->snil;
	DIR *dp = nil;
	struct dirent *e = nil;
	if(pairlength(src) == 1)
	{
		if(mcar(src)->type != STRING)
			return makeerror(2,0,"ls path : STRING => PAIR");
		dp = opendir(mcar(src)->object.str);
	}
	else
		dp = opendir(".");
	if(!dp)
		return makeerror(2,0,"opendir returned nil");
	while((e = readdir(dp)) != nil)
		ret = cons(makestring(e->d_name),ret);
	closedir(dp);
	return ret;
}
SExp *
f_cd(SExp *src, Symbol *env)
{
	int rc = 0, len = pairlength(src);
	SExp *tmp = env->snil;
	if(len != 1)
		return makeerror(2,0,"cd path : STRING => boolean");
	tmp = car(src);
	if(tmp->type != STRING)
		return makeerror(2,0,"cd path : STRING => boolean");
	rc = chdir(tmp->object.str);
	return !rc ? env->strue : env->sfalse;
}
SExp *
f_sysopen(SExp *src, Symbol *env)
{
	int rc = 0, opts = 0, mode = 0666;
	SExp *ret = env->snil, *tmp = env->snil, *holder = env->snil;
	ret = car(src);
	if(ret->type != STRING)
		return makeerror(2,0,"sys/open path : STRING keys : KEYOBJ => integer");
	holder = cdr(src);
	while(holder != env->snil)
	{
		tmp = car(holder);
		if(tmp->type != KEY)
			return makeerror(2,0,"sys/open's args must be KEYOBJs");
		if(!strncasecmp(tmp->object.str,"append",6))
			opts |= O_APPEND;
		else if(!strncasecmp(tmp->object.str,"create",6))
			opts |= O_CREAT;
		else if(!strncasecmp(tmp->object.str,"read-only",9))
			opts |= O_RDONLY;
		else if(!strncasecmp(tmp->object.str,"write-only",10))
			opts |= O_WRONLY;
		else if(!strncasecmp(tmp->object.str,"read-write",10))
			opts |= O_RDWR;
		else if(!strncasecmp(tmp->object.str,"non-blocking",12))
			opts |= O_NONBLOCK;
		else if(!strncasecmp(tmp->object.str,"noctty",6))
			opts |= O_NOCTTY;
		else if(!strncasecmp(tmp->object.str,"mode",4))
		{
			tmp = car(cdr(holder));
			if(tmp->type != NUMBER || (tmp->type == NUMBER && NTYPE(tmp) != INTEGER))
				return makeerror(2,0,"sys/open's :mode parameter *must* have an integer argument");
			mode = tmp->object.n->nobject.z;
			holder = cdr(holder);
		}
#ifdef MACBSD
		else if(!strncasecmp(tmp->object.str,"nosymlink",9))
			opts |= O_NOFOLLOW;
		else if(!strncasecmp(tmp->object.str,"symlink",7))
			opts |= O_SYMLINK;
#endif
		else
			return makeerror(2,0,"unknown keyobj passed to sysopen");
		holder = cdr(holder);
	}
	rc = open(ret->object.str,opts,mode);
	if(rc < 0)
		return makeerror(2,0,"open returned an error");
	ret = makenumber(INTEGER);
	ret->object.n->nobject.z = rc;
	return ret;
}
SExp *
f_sysread(SExp *src, Symbol *env)
{
	int rc = 0;
	SExp *tmp0 = env->snil, *tmp1 = env->snil;
	if(pairlength(src) != 2)
		return makeerror(2,0,"sys/read fd : INTEGER data : STRING => STRING");
	//printf("Made it to sysread...\n");
	tmp0 = car(src);
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) != INTEGER))
		return makeerror(2,0,"sys/read fd : INTEGER data : STRING => STRING");
	tmp1 = car(cdr(src));
	if(tmp1->type != STRING)
		return makeerror(2,0,"sys/read fd : INTEGER data : STRING => STRING");
	rc = read(tmp0->object.n->nobject.z,tmp1->object.str,tmp1->length);
	//printf("Past read...\n");
	if(rc >= 0)
	{
		//printf("rc == %d\n",rc);
		if(rc > 0)
			tmp1->object.str[rc] = nul;
		return tmp1;
	}
	return makeerror(2,0,"read did not return 0...");
}
SExp *
f_syswrite(SExp *src, Symbol *env)
{
	int rc = 0;
	SExp *tmp0 = env->snil, *tmp1 = env->snil;
	//printf("%d\n",pairlength(src));
	if(pairlength(src) != 2)
		return makeerror(2,0,"sys/write fd : INTEGER data : STRING => boolean 0");
	tmp0 = car(src);
	if(tmp0->type != NUMBER || (tmp0->type == NUMBER && NTYPE(tmp0) != INTEGER))
		return makeerror(2,0,"sys/write fd : INTEGER data : STRING => boolean 1");
	tmp1 = car(cdr(src));
	if(tmp1->type != STRING)
		return makeerror(2,0,"sys/write fd : INTEGER data : STRING => boolean 2");
	//printf("syswrite: attempting to write %d bytes\n",tmp1->length);
	rc = write(tmp0->object.n->nobject.z,tmp1->object.str,tmp1->length);
	//printf("Past write...\n");
	return rc ? env->strue : env->sfalse;
}
SExp *
f_sysclose(SExp *src, Symbol *e)
{
	int rc = 0;
	SExp *tmp = e->snil;
	if(pairlength(src) != 1)
		return makeerror(2,0,"sys/close fd : INTEGER => boolean");
	tmp = car(src);
	if(tmp->type != NUMBER || (tmp->type == NUMBER && NTYPE(tmp) != INTEGER))
		return makeerror(2,0,"sys/close fd : INTEGER => boolean");
	rc = close(tmp->object.n->nobject.z);
	return !rc ? e->strue : e->sfalse;
}
SExp *
f_syspipe(SExp *src, Symbol *e)
{
	int rc = 0, p[2] = {0};
	SExp *tmp = e->snil, *h = e->snil;
	rc = pipe(p);
	if(rc == -1)
		return makeerror(2,0,"sys/pipe returned -1!");
	tmp = makenumber(INTEGER);
	h = makenumber(INTEGER);
	tmp->object.n->nobject.z = p[0];
	h->object.n->nobject.z = p[1];
	return cons(tmp,cons(h,e->snil));
}
SExp *
f_popen(SExp *src, Symbol *e)
{
	/* this is an internal implementation of popen, that
	 * doesn't use FILEs, but regular pipe fds.
	 * this means that sysread/syswrite can be used
	 * there should also be an option to return a pair
	 * of ports...
	 * Similar to system(3), this uses sh to run any 
	 * user programs with arguments... probably should allow
	 * full specification of argv, but for now this is sufficient.
	 */
	int pid = 0, p[2] = {0}, rc = 0, rv[3] = {0};
	char *kv[3] = {"pid","read","write"}, *al[4] = {"sh","-c",0,0};
	struct stat b;
	SExp *tmp0 = e->snil, *tmp1 = e->snil, *tmp2 = e->snil;
	if(pairlength(src) != 1)
		return makeerror(2,0,"popen path : STRING => a-list");
	tmp0 = car(src);
	if(tmp0->type != STRING)
		return makeerror(2,0,"popen's sole argument must be a string");
	rc = stat(tmp0->object.str,&b);
	if(rc != 0)
		return makeerror(2,0,"stat did not return 0 on requested process");
	rc = pipe(p);
	if(rc != 0)
		return makeerror(2,0,"pipe did not return 0 in popen");
	pid = fork();
	if(pid == -1)
		return makeerror(2,0,"fork returned -1");
	if(pid == 0) /* child */
	{
		/* setup argument list, execve process after dup'ing 
		 * file descriptors to be the pipe's fd's...
		 */
		al[2] = tmp0->object.str;
		//printf("al[2] == %s\n",al[2]);
		dup2(p[0],0);
		dup2(p[1],1);
		dup2(p[1],2);

		rc = execve("/bin/sh",al,(char * const *)environ);
		if(rc == -1) /* should never return... */
			_exit(-11);
	}
	rv[0] = pid;
	rv[1] = p[0];
	rv[2] = p[1];
	for(tmp0 = e->snil, rc = 0; rc < 3;rc++)
	{
		tmp1 = makestring(kv[rc]);
		tmp1->type = KEY;
		tmp2 = makenumber(INTEGER);
		tmp2->object.n->nobject.z = rv[rc];
		tmp0 = cons(cons(tmp1,tmp2),tmp0);
	}
	return tmp0;
}
SExp *
f_pclose(SExp *src, Symbol *e)
{
	SExp *dat = e->snil, *tmp = e->snil;
	int stat = 0;
	if(pairlength(src) != 1)
		return makeerror(2,0,"pclose may only close one open pid/pipe pair at a time...");
	dat = car(src);
	if(dat->type != PAIR)
		return makeerror(2,0,"pclose tup : ALIST => boolean");
	while(dat != e->snil)
	{
		tmp = car(dat);
		if(tmp->type != PAIR)
			return makeerror(2,0,"pclose given argument that is not an ALIST");
		if(mcar(tmp)->type != KEY && mcar(tmp)->type != STRING && mcar(tmp)->type != ATOM)
			return makeerror(2,0,"pclose given malformed ALIST");
		if(!strncasecmp(mcar(tmp)->object.str,"pid",3))
		{
			kill(mcdr(tmp)->object.n->nobject.z, SIGKILL);
			waitpid(mcdr(tmp)->object.n->nobject.z,&stat,0);
		}
		else if(!strncasecmp(mcar(tmp)->object.str,"read",4))
			close(mcdr(tmp)->object.n->nobject.z);
		else if(!strncasecmp(mcar(tmp)->object.str,"write",5))
			close(mcdr(tmp)->object.n->nobject.z);
		dat = cdr(dat);
	}
	return e->strue;
}
SExp *
f_fork(SExp *src, Symbol *e)
{
	int rc = 0;
	SExp *ret = makenumber(INTEGER);
	rc = fork();
	if(rc == -1)
		return makeerror(2,0,"fork returned -1!");
	ret->object.n->nobject.z = rc;
	return ret;
}
SExp *
f_vfork(SExp *src, Symbol *e)
{
	int rc = 0;
	SExp *ret = makenumber(INTEGER);
	rc = vfork();
	if(rc == -1)
		return makeerror(2,0,"vfork returned -1!");
	ret->object.n->nobject.z = rc;
	return ret;
}
SExp *
f_execve(SExp *src, Symbol *e)
{
	SExp *path = e->snil, *env = e->snil, *args = e->snil;
	int aiter = 0, eiter = 0, rc = 0;
	char **el = nil, **al = nil;
	return e->snil;
}
SExp *
f_waitpid(SExp *src, Symbol *e)
{
	SExp *tmp0 = e->snil, *tmp1 = e->snil;
	int rc = 0, status = 0, pid = 0;
	tmp0 = car(src);
	if(tmp0->type != NUMBER || NTYPE(tmp0) != INTEGER)
		return makeerror(2,0,"waitpid requires an integer process id");
	pid = tmp0->object.n->nobject.z;
	rc = waitpid(pid,&status,WNOHANG);
	if(rc == 0) /* process has not exited */
		return cons(e->ssucc,e->snil);
	if(rc == -1) 
		return makeerror(2,0,"waitpid returned -1");
	if(pid == rc)
	{
		tmp0 = makenumber(INTEGER);
		if(WIFEXITED(status))
		{
			tmp0->object.n->nobject.z = WEXITSTATUS(status);
			return cons(e->strue,cons(tmp0,e->snil));
		}
		else if(WIFSIGNALED(status))
		{
			tmp0->object.n->nobject.z = WTERMSIG(status);
			return cons(e->sfalse,cons(tmp0,e->snil));
		}
		else if(WIFSTOPPED(status))
		{
			tmp0->object.n->nobject.z = WSTOPSIG(status);
			return cons(e->sunsucc,cons(tmp0,e->snil));
		}
	}
	return e->snil;
}
SExp *
f_quit(SExp *src, Symbol *e)
{
	quit_note = 1;
	return e->svoid;
}
SExp *
f_kill(SExp *src, Symbol *e)
{
	int rc = 0;
	SExp *pid = e->snil, *sig = e->snil;
	if(pairlength(src) != 2)
		return makeerror(2,0,"sys/kill pid : INTEGER sig : INTEGER => boolean");
	pid = car(src);
	if(pid->type != NUMBER || (pid->type == NUMBER && NTYPE(pid) != INTEGER))
		return makeerror(2,0,"pid *must* be an integer");
	sig = car(cdr(src));
	if(sig->type != NUMBER || (sig->type == NUMBER && NTYPE(sig) != INTEGER))
		return makeerror(2,0,"sig *must* be an integer");
	if(sig->object.n->nobject.z < 0 || sig->object.n->nobject.z > 31)
		return makeerror(2,0,"invalid signal");
	rc = kill(pid->object.n->nobject.z,sig->object.n->nobject.z);
	if(!rc)
		return e->strue;
	return e->sfalse;
}
SExp *
f_remote_openp(SExp *src, Symbol *e)
{
	struct sockaddr_in saddr;
	struct protoent *p = nil;
	int res = 0, fd = 0;
	SExp *t0 = e->snil, *t1 = e->snil;
	if(pairlength(src) != 2)
		return makeerror(2,0,"remote-port-open? address : STRING port : INTEGER => boolean");
	t0 = car(src);
	if(t0->type != STRING)
		return makeerror(2,0,"first argument to remote-port-open? *must* be a string");
	t1 = car(cdr(src));
	if(t1->type != NUMBER || (t1->type == NUMBER && NTYPE(t1) != INTEGER))
		return makeerror(2,0,"second argument to remote-port-open? *must* be an integer");
	p = getprotobyname("tcp");
	if ((fd =  socket(PF_INET, SOCK_STREAM, p->p_proto)) == -1)
		return makeerror(2,0,"socket(2) returned -1");
	memset(&saddr, 0, sizeof(saddr));
	saddr.sin_family = AF_INET;
	saddr.sin_port = htons(t1->object.n->nobject.z);
	res = inet_pton(AF_INET, t0->object.str, &saddr.sin_addr);
	if(res != 1)
	{
		close(fd);
		return makeerror(2,0,"inet_pton returned something other than 1");
	}
	if(connect(fd,(void *)&saddr,sizeof(struct sockaddr_in)) == -1)
	{
		close(fd);
		return e->sfalse;
	}
	shutdown(fd,SHUT_RDWR);
	close(fd);
	return e->strue;
}
SExp *
f_rete(SExp *src, Symbol *e)
{
	int snoopy = -1, size = 16384;
	char *buf = nil;
	snoopy = socket(AF_INET,SOCK_RAW,IPPROTO_TCP);

	return e->sfalse;	
}
SExp *
f_gethostbyname(SExp * src, Symbol *e)
{
	/* needs to by AF_INET6-proofed */
	SExp *ymp = e->snil, *ret = e->snil;
	struct hostent *h = nil;
	char *buf = nil;
	const char *rr = nil;
	int iter = 0;
	if(pairlength(src) != 1)
		return makeerror(2,0,"gethostbyname name : STRING => LIST");
	ymp = car(src);
	if(ymp->type != STRING)
		return makeerror(2,0,"name *must* be a string");
	h = gethostbyname(ymp->object.str);
	if(h == nil)
		return makeerror(2,0,(char *)hstrerror(h_errno));
	ret = makevector(5,nil);
	ret->object.vec[0] = makestring(h->h_name);
	ymp = e->snil;
	while(h->h_aliases[iter])
	{
		ymp = cons(makestring(h->h_aliases[iter]),ymp);
		iter++;
	}
	ret->object.vec[1] = ymp;
	ymp = makenumber(INTEGER);
	ymp->object.n->nobject.z = h->h_addrtype;
	ret->object.vec[2] = ymp;
	ymp = makenumber(INTEGER);
	ymp->object.n->nobject.z = h->h_length;
	ret->object.vec[3] = ymp;
	ymp = e->snil;
	iter = 0;
	buf = (char *)malloc(sizeof(char) * 17);
	while(h->h_addr_list[iter])
	{
		rr = inet_ntop(h->h_addrtype,h->h_addr_list[iter],buf,17);
		ymp = cons(makestring(buf),ymp);
		iter++;
	}
	ret->object.vec[4] = ymp;
	free(buf);
	return ret;
}
/* collapse these two? pass in a :host or :addr maybe? 
 * then, the proper "gethostbyname" or "gethostbyaddress"
 * becomes a simple wrapper around llgethostby
 */
SExp *
f_gethostbyaddr(SExp * src, Symbol *e)
{
	/* needs to by AF_INET6-proofed */
	SExp *ymp = e->snil, *ret = e->snil;
	struct hostent *h = nil;
	char *buf = nil;
	const char *rr = nil;
	int iter = 0;
	if(pairlength(src) != 1)
		return makeerror(2,0,"gethostbyname name : STRING => LIST");
	ymp = car(src);
	if(ymp->type != STRING)
		return makeerror(2,0,"name *must* be a string");
	//printf("String length == %d\n",ymp->length);
	buf = (char *)malloc(sizeof(char) * 17);
	iter = inet_pton(AF_INET,ymp->object.str,(void *)buf);
	if(iter != 1)
		return makeerror(2,0,(char *)hstrerror(h_errno));
	//printf("buf.length == %d\n",strlen(buf));
	h = gethostbyaddr(buf, strlen(buf), AF_INET);
	if(h == nil)
		return makeerror(2,0,(char *)hstrerror(h_errno));
	ret = makevector(5,nil);
	ret->object.vec[0] = makestring(h->h_name);
	ymp = e->snil;
	while(h->h_aliases[iter])
	{
		ymp = cons(makestring(h->h_aliases[iter]),ymp);
		iter++;
	}
	ret->object.vec[1] = ymp;
	ymp = makenumber(INTEGER);
	ymp->object.n->nobject.z = h->h_addrtype;
	ret->object.vec[2] = ymp;
	ymp = makenumber(INTEGER);
	ymp->object.n->nobject.z = h->h_length;
	ret->object.vec[3] = ymp;
	ymp = e->snil;
	iter = 0;
	while(h->h_addr_list[iter])
	{
		rr = inet_ntop(h->h_addrtype,h->h_addr_list[iter],buf,17);
		ymp = cons(makestring(buf),ymp);
		iter++;
	}
	ret->object.vec[4] = ymp;
	free(buf);
	return ret;
}
SExp *
f_stat(SExp *s, Symbol *e)
{
	struct stat st;
	SExp *ret = e->snil, *tmp = e->snil, *opt = e->snil;
	int iter = 0,rc = 0, rettype = 0;
    rc = pairlength(s);
	if(rc < 1 || rc > 2)
		return makeerror(2,0,"sys/stat fp : STRING [option : BOOLEAN] => VECTOR|DICT");
    tmp = car(s);
    if(rc == 2)
    {
          opt = car(cdr(s));
          if(opt->type == BOOL || opt->type == GOAL)
          {
                  if(opt->object.c)
                          rettype = 1;
          }
    }
	if(tmp->type != STRING)
		return makeerror(2,0,"sys/stat's sole argument must be a string");
	rc = stat(tmp->object.str,&st);
	if(rc == -1)
		return e->sfalse;
    if(!rettype)
    {
	    ret = makevector(15,nil);
	    ret->object.vec[0] = makeinteger(st.st_dev);
	    ret->object.vec[1] = makeinteger(st.st_ino);
	    ret->object.vec[2] = makeinteger(st.st_mode);
	    ret->object.vec[3] = makeinteger(st.st_nlink);
	    ret->object.vec[4] = makeinteger(st.st_uid);
	    ret->object.vec[5] = makeinteger(st.st_gid);
	    ret->object.vec[6] = makeinteger(st.st_rdev);
	    ret->object.vec[7] = e->snil;
	    ret->object.vec[8] = e->snil;
	    ret->object.vec[9] = e->snil;
	    ret->object.vec[10] = makeinteger(st.st_size);
	    ret->object.vec[11] = makeinteger(st.st_blocks);
	    ret->object.vec[12] = makeinteger(st.st_blksize);
	/* when everything is split into individual files, there 
	 * should be a low-level OS interaction file for each
	 * system, including Mac OS X...
	 */
#ifdef MACSRC
	    ret->object.vec[13] = makeinteger(st.st_flags);
	    ret->object.vec[14] = makeinteger(st.st_gen);
#else
	    ret->object.vec[13] = makeinteger(0);
	    ret->object.vec[14] = makeinteger(0);
#endif
    }
    else
    {
        ret = makedict();
        trie_put("st_dev",makeinteger(st.st_dev),ret->object.dict);
        trie_put("st_ino",makeinteger(st.st_ino),ret->object.dict);
        trie_put("st_mode",makeinteger(st.st_mode),ret->object.dict);
        trie_put("st_nlink",makeinteger(st.st_nlink),ret->object.dict);
        trie_put("st_uid",makeinteger(st.st_uid),ret->object.dict);
        trie_put("st_gid",makeinteger(st.st_gid),ret->object.dict);
        trie_put("st_rdev",makeinteger(st.st_rdev),ret->object.dict);
        trie_put("st_size",makeinteger(st.st_size),ret->object.dict);
        trie_put("st_blocks",makeinteger(st.st_blocks),ret->object.dict);
        trie_put("st_blksize",makeinteger(st.st_blksize),ret->object.dict);
    }
	return ret;
}
SExp *
f_open(SExp *s, Symbol *e)
{
	SExp *ret = e->snil, *path = e->snil, *mode = e->snil;
	FILE *fd = nil;
	int iter = 0;
	char *modes[] = {"r","r+","w","w+","a","a+"};
	if(pairlength(s) != 2)
		return makeerror(2,0,"open path : STRING direction : KEYOBJ => Port");
	path = car(s);
	if(path->type != STRING)
		return makeerror(2,0,"path must be a string");
	mode = car(cdr(s));
	if(mode->type != KEY)
		return makeerror(2,0,"direction must refer to a keyobject");
	if(!strncasecmp(mode->object.str,"read",4))
		iter = 0;
	else if(!strncasecmp(mode->object.str,"read+",5))
		iter = 1;
	else if(!strncasecmp(mode->object.str,"write",5))
		iter = 2;
	else if(!strncasecmp(mode->object.str,"write+",6))
		iter = 3;
	else if(!strncasecmp(mode->object.str,"append",6))
		iter = 4;
	else if(!strncasecmp(mode->object.str,"append+",7))
		iter = 5;
	else
		return makeerror(2,0,"unknown keyobj passed to open!");
	if((fd = fopen(path->object.str,modes[iter])) == nil)
		return makeerror(2,0,"fopen returned nil");
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = PORT;
	PORT(ret) = (Port *)hmalloc(sizeof(Port));
	PORT(ret)->state = 1;
	PTYPE(ret) = PFILE;
	FILEPORT(ret) = fd;
	FILEADDRESS(ret) = hstrdup(path->object.str);
	strcpy(FILEMODE(ret),modes[iter]);
	PROTONUMBER(ret) = -1;
	NETBIND(ret) = -1;
	return ret;	
}
SExp *
f_close(SExp *s, Symbol *e)
{
	SExp *tmp = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"close fd : PORT => boolean");
	tmp = car(s);
	if(tmp->type != PORT)
		return e->sfalse;
	/* prevent multiple calls to close on a PORT */
	if(PORT(tmp)->state)
	{
		fclose(FILEPORT(tmp));
		PORT(tmp)->state = 0;
		return e->strue;
	}
	return e->sfalse;
}
SExp *
f_read(SExp *s, Symbol *e)
{
	/* Port should be optional, but I've not
	 * yet introduced current-input-port/current-output-port
	 * and the associated functions, so this is not fully
	 * useful yet...
	 */
	SExp *tmp = e->snil, *ret = e->snil;
	int itmp = pairlength(s);
	if( itmp > 1)
		return makeerror(2,0,"read [fd: PORT] => sexpression");
	if(itmp == 1)
	{
		tmp = car(s);
		if(tmp->type != PORT)
			return makeerror(2,0,"read's optional argument must be a port");
		ret = llread(FILEPORT(tmp)); /* should check if it's a PFILE first... */
	}
	else
		ret = llread(stdin); /* should lookup current-input-port */
	return ret;
}
SExp *
f_write(SExp *s, Symbol *e)
{
	/* this should simply be a call to llprinc with the 
	 * correct options:
	 * llprinc(obj,file_descriptor,1);
	 * where:
	 *  o obj is the object to be printed
	 *  o file_descripter is the FILE-backed port to print to
	 *  o 1 is the mode llprinc should use (i.e. WRITE_MODE)
	 * The princ-modes (DISPLAY_MODE, WRITE_MODE) should probably
	 * be enum'd or #define'd somewhere for clarity
	 */
	int i = pairlength(s);
	SExp *t = e->snil, *f = e->snil;
	/*printf("s == ");
	llprinc(s,stdout,1);
	printf("\n");*/
	if(i < 1 || i > 2)
		return makeerror(2,0,"write expects at least one argument, and no more than two: write o : SEXPRESSION [p : PORT] => SEXRESSION");
	f = car(s);
	if(i == 1)
		llprinc(f,stdout,1); /* should look up what *current-output-port* is set to... */
	else
	{
		t = car(cdr(s));
		if(t->type != PORT)
			return makeerror(2,0,"write's p argument must be of type PORT");
		llprinc(f,FILEPORT(t),1);
	}
	return f;
}
SExp *
f_read_char(SExp *s, Symbol *e)
{
	SExp *tmp = e->snil, *ret = e->snil;
	FILE *fd = nil;
	int itmp = pairlength(s), len = 0, sd = 0, ptype = 0;
	if(itmp > 1)
		return makeerror(2,0,"read-char [fd : PORT] => sexpression");
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = CHAR;
	if(itmp == 1)
	{
		tmp = car(s);
		if(tmp->type != PORT)
			return makeerror(2,0,"read-char's optional argument must be a port");
		if(PTYPE(tmp) == PNET)
		{
			sd = tmp->object.p->pobject.fd;
			ptype = 1;
		}
		else
			fd = FILEPORT(tmp); /* should check if it's a PFILE first... */
	}
	else
		fd = stdin; /* should lookup current-input-port */
	if(!ptype)
	{
		ret->object.c = fgetc(fd);
		if(feof(fd))
			return e->seof;
	}
	else
	{
		len = read(sd,&ret->object.c,1);
		if(len == 0)
			return e->seof;
	}	
	return ret;
}
SExp *
f_read_string(SExp *s, Symbol *e)
{
	SExp *tmp = e->snil, *ret = e->snil;
	char buf[8192] = {0};
	int itmp = pairlength(s), iter = 0, in = 0, fd = 0, ptype = 0, len = 0;
	FILE *fdin = nil;
	if(itmp > 1)
		return makeerror(2,0,"read-string [fd : PORT] => sexpression");
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = STRING;
	if(itmp == 1)
	{
		tmp = car(s);
		if(tmp->type != PORT)
			return makeerror(2,0,"read-string's optional argument must be a port");
		if(PTYPE(tmp) == PNET)
		{
			fd = tmp->object.p->pobject.fd;
			ptype = 1;
		}
		else
			fdin = FILEPORT(tmp); /* should check if it's a PFILE first... */
	}
	else
		fdin = stdin; /* should lookup current-input-port */
	//printf("Made it to f_read_string: ptype == %d\n",ptype);
	while(1)
	{
		//printf("looping in f_read_string\n");
		if(!ptype)
		{
			in = fgetc(fdin);
			if(feof(fdin) || in == EOF)
			{
				if(iter == 0)
					return e->seof;
				break;
			}
		}	
		else
		{
			//printf("%d: before read\n",__LINE__);
			len = read(fd,&in,1);
			//printf("%d: after read\n",__LINE__);
			if(len == 0)
			{
				if(iter == 0)
					return e->seof;
				break;
			}
			else if(len < 0)
			{
				if(errno == EBADF)
					printf("BADF\n");
				else if(errno == EAGAIN)
					printf("AGAIN\n");
				else if(errno == EINTR)
					printf("EINTr\n");
				else if(errno == EINVAL)
					printf("EINVAL\n");
				else if(errno == EIO)
					printf("EIO\n");
				else if(errno == EFAULT)
					printf("EFAULT\n");
				else 
					printf("SOME OTHER errno\n");
				return makeerror(2,0,"read-string: error from PNET on read");
			}
		}
		if(in == '\r')
		{
			if(!ptype)
			{
				in = fgetc(fdin);
				if(in == '\n')
					break;
				else if(feof(fdin))
				{
					if(iter == 0)
						return e->seof;
					break;
				}
				else
					ungetc(in,fdin);
			}
			else
			{
				len = read(fd,&in,1);
				if(len == 0) // eof
				{
					if(iter == 0)
						return e->seof;
					break;
				}
				else if(in == '\n')
					break;
			}
		}
		if(in == '\n')
			break;
		buf[iter] = in;
		iter++;
		if(iter > 8190)
			break;
	}
	buf[iter] = '\0';
	ret->length = iter;
	ret->object.str = hstrdup(buf);
	return ret;
}
SExp *
f_write_char(SExp *s, Symbol *e)
{
	SExp *tmp = e->snil, *port = e->snil, *ret = e->snil;
	int itmp = pairlength(s), rc = 0, ptype = 0, len = 0, sd = 0;
	FILE *fd = nil;
	if(itmp < 1 || itmp > 2)
		return makeerror(2,0,"write-char c : CHAR [fd : PORT] => sexpression");
	tmp = car(s);
	if(tmp->type != CHAR)
		return makeerror(2,0,"write-char's argument c *must* be a character");
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = CHAR;
	if(itmp == 2)
	{
		port = car(cdr(s));
		if(port->type != PORT)
			return makeerror(2,0,"write-char's optional argument must be a port");
		if(PTYPE(port))
			fd = FILEPORT(port); /* should check if it's a PFILE first... */
		else if(PTYPE(port) == PNET)
		{
			ptype = 1;
			sd = PORT(port)->pobject.fd;
		}
	}
	else
		fd = stdout; /* should lookup current-input-port */
	if(!ptype)
	{
		rc = fputc(tmp->object.c,fd);
		if(rc != EOF)
			return tmp;
	}	
	else
	{
		len = write(sd,&tmp->object.c,1);
		if(len < 0)
			return makeerror(2,0,"write-char: write returned an error");
		return tmp;
	}
	return e->sfalse;
}
SExp *
f_dial(SExp *s, Symbol *e)
{
	/* This should really return a pair, one for reading, one for 
	 * writing...
	 * (define-multiple (reader writer) (dial :tcp host port))
	 */
	struct sockaddr_in saddr;
	struct protoent *p = nil;
	int res = 0, fd = 0, inet_type = 0;
	SExp *ret = e->snil, *type = e->snil, *host = e->snil, *port = e->snil, *ret1 = e->snil;
	if(pairlength(s) != 3)
		return makeerror(2,0,"dial type : KEYOBJECT host : STRING p : INTEGER => PORT");
	type = car(s);
	if(type->type != KEY)
		return makeerror(2,0,"type *must* be bound to a key object \n");
	if(!strncasecmp(type->object.str,"tcp",3))
		p = getprotobyname("tcp");
	else if(!strncasecmp(type->object.str,"udp",3))
		p = getprotobyname("udp");
	else if(!strncasecmp(type->object.str,"tcp6",4))
	{
		inet_type = 1;
		p = getprotobyname("tcp");
	}
	else if(!strncasecmp(type->object.str,"udp6",4))
	{
		inet_type = 1;
		p = getprotobyname("udp");
	}
	else if(!strncasecmp(type->object.str,"sctp",4))
		p = getprotobyname("sctp");
	else if(!strncasecmp(type->object.str,"sctp6",5))
	{
		inet_type = 1;
		p = getprotobyname("sctp");
	}
	host = car(cdr(s));
	if(host->type != STRING)
		return makeerror(2,0,"host *must* be a string object");
	port = car(cdr(cdr(s)));
	if(port->type != NUMBER || (port->type == NUMBER && NTYPE(port) != INTEGER))
		return makeerror(2,0,"p *must* be bound to an INTEGER");
	if(inet_type)
		fd =  socket(PF_INET6, SOCK_STREAM, p->p_proto);
	else
		fd =  socket(PF_INET, SOCK_STREAM, p->p_proto);
	if (fd == -1)
	{
		printf("socket(2) returned -1");
		return nil;
	}
	memset(&saddr, 0, sizeof(saddr));
	if(inet_type)
		saddr.sin_family = AF_INET6;
	else
		saddr.sin_family = AF_INET;
	saddr.sin_port = htons(port->object.n->nobject.z);
	if(inet_type)
		res = inet_pton(AF_INET6, host->object.str, &saddr.sin_addr);
	else
		res = inet_pton(AF_INET, host->object.str, &saddr.sin_addr);
	if(res != 1)
	{
		close(fd);
		makeerror(2,0,"inet_pton returned something other than 1\n");
	}
    	if(connect(fd,(void *)&saddr,sizeof(struct sockaddr_in)) == -1)
	{
		close(fd);
		return makeerror(2,0,"connect returned -1\n");
	}
	//return fdopen(fd,"r+");
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = PORT;
	PORT(ret) = (Port *)hmalloc(sizeof(Port));
	//FILEPORT(ret) = fdopen(fd,"w+"); // this isn't working for accept & friends
	PORT(ret)->pobject.fd = fd;
	FILEADDRESS(ret) = hstrdup(host->object.str);
	strcpy(FILEMODE(ret),"w+");
	PROTONUMBER(ret) = p->p_proto;
	NETBIND(ret) = port->object.n->nobject.z;
	PTYPE(ret) = PNET;
	return ret;
}
SExp *
f_announce(SExp *s, Symbol *e)
{
	/* announce a server port; 
	 * similar format to Plan9's formmating:
	 * proto!host!port
	 * :proto "host" port
	 */
	struct sockaddr_in sa;
	int sock = 0;
	SExp *ret = nil, *stype = nil, *host = nil, *port = nil;
	struct protoent *p = nil;
    char *buf = nil;
    socklen_t sze = sizeof(struct sockaddr_in);

	if(s == nil || pairlength(s) != 3)
		return makeerror(2,0,"announce type : KEYOBJ host : STRING port : INTEGER => PORT");
	/* type: tcp, tcp6, udp, udp6, sctp, sctp6 */	
	stype = car(s);
	host = car(cdr(s));
	port = car(cdr(cdr(s)));
	if(stype->type != KEY)
		return makeerror(2,0,"announce's type argument *must* be of type KEYOBJ");
	if(host->type != STRING)
		return makeerror(2,0,"announce's host argument *must* be a string");
	if(port->type != NUMBER || (port->type == NUMBER && NTYPE(port) != INTEGER))
		return makeerror(2,0,"announce's port argument *must* be an INTEGER");
	if(!strncasecmp(stype->object.str, "tcp",3))
	{
		// bleh
		memset(&sa, 0, sizeof(struct sockaddr_in));
		sa.sin_family = AF_INET;
		sa.sin_port = htons(AINT(port)); /* should detect this from host arg :D */
		sa.sin_addr.s_addr = INADDR_ANY; /* convert the *host* argument */
		p = getprotobyname("tcp");
		if(p == nil)
			return makeerror(2,0,"announce failed: getprotobyname returned nil");
		if((sock = socket(PF_INET, SOCK_STREAM, p->p_proto)) == -1)
			return makeerror(2,0,"announce failed: socket returned -1");
		if(bind(sock,(const struct sockaddr *)&sa, sizeof(struct sockaddr_in)) == -1)
		{
			close(sock);
			return makeerror(2,0,"announce failed: bind returned -1");
		}
		if(listen(sock,10) == -1)
		{
			close(sock);
			return makeerror(2,0,"announce failed: listen returned -1");
		}
		ret = (SExp *)hmalloc(sizeof(SExp));
		ret->type = PORT;
        buf = (char *)hmalloc(sizeof(INET_ADDRSTRLEN));
        inet_ntop(AF_INET,&sa.sin_addr, buf, INET_ADDRSTRLEN);
		PORT(ret) = (Port *)hmalloc(sizeof(Port));
		//FILEPORT(ret) = fdopen(sock,"w+");
        FILEADDRESS(ret) = buf;
		PORT(ret)->pobject.fd = sock;
		PROTONUMBER(ret) = p->p_proto;
		NETBIND(ret) = AINT(port);
		PTYPE(ret) = PNET;
		SOCKINFO(ret) = (void *) &sa;
		return ret;	
	}
	else
		return makeerror(2,0,"announce: unknown socket type");
	return e->snil;
}
SExp *
f_accept(SExp *s, Symbol *e)
{
	SExp *tmp = e->snil, *ret = nil;
    struct sockaddr_in peer;
    socklen_t peer_size = sizeof(struct sockaddr_in);
	int fd = 0, rc = 0;
    char *buf;
	if(pairlength(s) != 1)
		return makeerror(2,0,"accept s : PORT => PORT");
	tmp = car(s);
	if(tmp->type != PORT || PTYPE(tmp) != PNET)
		return makeerror(2,0,"accept's sole argument *must* be a network port");	
	//fd = fileno(FILEPORT(tmp));
	fd = PORT(tmp)->pobject.fd;
	rc = accept(fd,(struct sockaddr *)&peer,&peer_size);
	if(rc < 0)
		return makeerror(2,0,"accept returned -1; exiting");
    buf = (char *)hmalloc(INET_ADDRSTRLEN);
    inet_ntop(AF_INET,&peer.sin_addr,buf,INET_ADDRSTRLEN);
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = PORT;
	PORT(ret) = (Port *)hmalloc(sizeof(Port));
    if(buf)
        FILEADDRESS(ret) = buf;
	//FILEPORT(ret) = fdopen(rc,"w+");
	PORT(ret)->pobject.fd = rc;
	PROTONUMBER(ret) = -1;
	NETBIND(ret) = -1;
	PTYPE(ret) = PNET;
	SOCKINFO(ret) = nil;
	return ret;
}
SExp *
f_listen(SExp *s, Symbol *e)
{
	SExp *tmp = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"listen s : PORT => PORT");
	tmp = car(s);
	return e->snil;
}
SExp *
f_hangup(SExp *s, Symbol *e)
{
	SExp *tmp = e->snil;
	int fd = 0;
	if(pairlength(s) != 1)
		return makeerror(2,0,"hangup p : PORT => BOOLEAN");
	tmp = car(s);
	if(tmp->type != PORT)
		return makeerror(2,0,"p *must* be bound to a PORT");
	//fd = fileno(FILEPORT(tmp));
	fd = PORT(tmp)->pobject.fd;
	if(shutdown(fd,SHUT_RDWR))
	{
		close(fd);
		if(errno == ENOTCONN)
			return e->strue;
		return makeerror(2,0,"shutdown returned something other than 0...");
	}
	close(fd);
	return e->strue;
}
/* mostly redundant; would be nice to factor these out... */
SExp *
f_getuid(SExp *s, Symbol *e)
{
	return makeinteger(getuid());
}
SExp *
f_geteuid(SExp *s, Symbol *e)
{
	return makeinteger(geteuid());
}
SExp *
f_getgid(SExp *s, Symbol *e)
{
	return makeinteger(getgid());
}
SExp *
f_getegid(SExp *s, Symbol *e)
{
	return makeinteger(getegid());
}
SExp *
f_setsid(SExp *s, Symbol *e)
{
	SExp *r = e->snil, *type = e->snil;
	int rc = 0;
	if(pairlength(s) != 2)
		return makeerror(2,0,"setuid class : KEYOBJ uid : INTEGER => BOOLEAN");
	type = car(s);
	if(type->type != KEY)
		return makeerror(2,0,"class *must* be bound to a KEYOBJ");
	r = car(cdr(s));
	if(r->type != NUMBER || (r->type == NUMBER && NTYPE(r) != INTEGER))
		return makeerror(2,0,"uid *must* be bound to an integer...");
	if(!strncasecmp(type->object.str,"uid",3))
		rc = setuid(r->object.n->nobject.z);
	else if(!strncasecmp(type->object.str,"euid",4))
		rc = seteuid(r->object.n->nobject.z);
	else if(!strncasecmp(type->object.str,"gid",3))
		rc = setgid(r->object.n->nobject.z);
	else if(!strncasecmp(type->object.str,"egid",4))
		rc = setegid(r->object.n->nobject.z);
	else
		return makeerror(2,0,"unknown class type for setsid");
	if(rc != 0)
		return makeerror(2,0,strerror(errno));
	return e->strue;
}

SExp *
f_write_buf(SExp *s, Symbol *e)
{
	SExp *buf = e->snil, *port = e->snil;
	int len = 0;
	if(pairlength(s) != 2)
		return makeerror(2,0,"write-buffer buffer : STRING p : PORT => boolean?");
	buf = car(s);
	if(buf->type != STRING)
		return makeerror(2,0,"buffer *must* be bound to a STRING");
	port = car(cdr(s));
	if(port->type != PORT)
		return makeerror(2,0,"p *must* be bound to a port");
	len = buf->length;
	if(PTYPE(port) == PFILE)
	{
		if((len = fwrite(buf->object.str,sizeof(char),buf->length,FILEPORT(port))) != buf->length)
			return makeerror(2,0,"fwrite returned mis-matched sizes!");
		if(fflush(FILEPORT(port)) != 0)
			return makeerror(2,0,"fflush return something other than 0!");
	}
	else if(PTYPE(port) == PNET)
	{
		if((len = write(PORT(port)->pobject.fd,buf->object.str,buf->length)) != buf->length)
			return makeerror(2,0,"write returned mis-matched sizes for PNET!");
	}	
	return e->svoid;
}
SExp *
f_read_buf(SExp *s, Symbol *e)
{
	SExp *buf = e->snil, *port = e->snil;
	int len = 0;
	if(pairlength(s) != 2)
		return makeerror(2,0,"read-buffer buffer : STRING p : PORT => boolean?");
	buf = car(s);
	if(buf->type != STRING)
		return makeerror(2,0,"buffer *must* be bound to a STRING");
	port = car(cdr(s));
	if(port->type != PORT)
		return makeerror(2,0,"p *must* be bound to a port");
	len = buf->length;
	if(PTYPE(port) == PFILE)
	{
		if((len = fread(buf->object.str,sizeof(char),buf->length,FILEPORT(port))) != buf->length)
		{
			//printf("made it past fread...\n");
			if(len > 0)
				return e->strue;
			if(feof(FILEPORT(port)))
				return e->seof;
			return makeerror(2,0,"fread hit an error...");
		}
	}
	else if(PTYPE(port) == PNET)
	{
		if((len = read(PORT(port)->pobject.fd,buf->object.str,buf->length)) != buf->length)
		{
			if(len > 0)
				return e->svoid;
			if(len == 0)
				return e->seof;
			return makeerror(2,0,"read returned -1...");
		}
	}
	return e->svoid;
}
SExp *
f_gettimeofday(SExp *s, Symbol *e)
{
	struct timeval tp;
	struct timezone tz;
	int i = 0;
	SExp *ret = e->snil;
	if((i = gettimeofday(&tp,(void *)&tz)) != 0)
		return makeerror(2,0,"gettimeofday returned something other than 0...");
	ret = makevector(4,nil);
	ret->object.vec[0] = makeinteger(tp.tv_sec);
	ret->object.vec[1] = makeinteger(tp.tv_usec);
	ret->object.vec[2] = makeinteger(tz.tz_minuteswest);
	ret->object.vec[3] = makeinteger(tz.tz_dsttime);
	return ret;
}
SExp *
f_ssockopt(SExp *s, Symbol *e)
{
	struct linger sl;
	struct timeval tv;
	SExp *op = e->snil, *port = e->snil, *key = e->snil, *opt_val = e->snil, *ret = e->snil, *lvl = e->snil;
	int access = 0, fd = 0, internal_value = 0, rc = 0, size = 0, slevel = 0;
	if(pairlength(s) != 5)
		return makeerror(2,0,"sys/getsockopt op : KEYOBJ p : PORT level : KEYOBJ accessor : KEYOBJ value : SEXPRESSION => SEXPRESSION");
	op = car(s);
	if(op->type != KEY)
		return makeerror(2,0,"op *must* be bound to a keyobject");
	port = car(cdr(s));
	if(port->type != PORT)
		return makeerror(2,0,"p *must* be bound to a port object");
	lvl = car(cdr(cdr(s)));
	if(lvl->type != KEY)
		return makeerror(2,0,"level *must* be bound to a keyobject");
	if(!strncasecmp(lvl->object.str,"p",1))
		slevel = PROTONUMBER(port);
	else if(!strncasecmp(lvl->object.str,"s",1))
		slevel = SOL_SOCKET;
	else
		return makeerror(2,0,"level must be one of two key objects: :p[rotocol] (for protocol level) or :s[ocket] (for socket level)");
	key = car(cdr(cdr(cdr(s))));
	if(key->type != KEY)
		return makeerror(2,0,"accessor *must* be bound to a keyobject");
	opt_val = car(cdr(cdr(cdr(cdr(s)))));
	if((opt_val->type != NUMBER || (opt_val->type == NUMBER && NTYPE(opt_val) != INTEGER)) && opt_val->type != VECTOR)
		return makeerror(2,0,"value *must* be bound to either an integer or a vector");
	if(!strncasecmp(key->object.str,"debug",5))
		access = SO_DEBUG;
	else if(!strncasecmp(key->object.str,"reuseaddr",9))
		access = SO_REUSEADDR;
#ifdef BSD
	else if(!strncasecmp(key->object.str,"reuseport",9))
		access = SO_REUSEPORT;
#endif
	else if(!strncasecmp(key->object.str,"keepalive",9))
		access = SO_KEEPALIVE;
	else if(!strncasecmp(key->object.str,"dontroute",9))
		access = SO_DONTROUTE;
	else if(!strncasecmp(key->object.str,"linger",6))
		access = SO_LINGER;
	else if(!strncasecmp(key->object.str,"broadcast",9))
		access = SO_BROADCAST;
	else if(!strncasecmp(key->object.str,"oobinline",9))
		access = SO_OOBINLINE;
	else if(!strncasecmp(key->object.str,"sndbuf",6))
		access = SO_SNDBUF;
	else if(!strncasecmp(key->object.str,"rcvbuf",6))
		access = SO_RCVBUF;
	else if(!strncasecmp(key->object.str,"sndlowat",8))
		access = SO_SNDLOWAT;
	else if(!strncasecmp(key->object.str,"rcvlowat",8))
		access = SO_RCVLOWAT;
	else if(!strncasecmp(key->object.str,"sndtimeo",8))
		access = SO_SNDTIMEO;
	else if(!strncasecmp(key->object.str,"rcvtimeo",8))
		access = SO_SNDTIMEO;
	else if(!strncasecmp(key->object.str,"type",4))
		access = SO_TYPE;
	else if(!strncasecmp(key->object.str,"error",5))
		access = SO_ERROR;
#ifdef MACBSD
	else if(!strncasecmp(key->object.str,"nosigpipe",9))
		access = SO_NOSIGPIPE;
#endif
	else
		return makeerror(2,0,"unknown accessor type...");
	fd = fileno(FILEPORT(port));
	if(!strncasecmp(op->object.str,"get",3))
	{
		/* for most accessors, this is simply to
		 * unbox the integer; for SO_LINGER,
		 * SO_{SND RCV}TIMEO, we must inflate
		 * structs (struct linger & struct timeval respectively)
		 * from Vectors (of integers)
		 */
		switch(access)
		{
			case SO_LINGER:
				rc = getsockopt(fd,slevel,SO_LINGER,(void *)&sl,(unsigned int *)&size);
				if(rc)
					return makeerror(2,0,"getsockopt returned something other than 0...");
				ret = makevector(2,nil);
				ret->object.vec[0] = makeinteger(sl.l_onoff);
				ret->object.vec[1] = makeinteger(sl.l_linger);
				return ret;
			case SO_SNDTIMEO:
			case SO_RCVTIMEO:
				rc = getsockopt(fd,slevel,access,(void *)&tv,(unsigned int *)&size);
				if(rc)
					return makeerror(2,0,"getsockopt returned something other than 0...");
				ret = makevector(2,nil);
				ret->object.vec[0] = makeinteger(tv.tv_sec);
				ret->object.vec[1] = makeinteger(tv.tv_usec);
				return ret;
			default:
				rc = getsockopt(fd,slevel,access,(void *)&internal_value,(unsigned int *)&size);
				if(rc)
					return makeerror(2,0,"getsockopt returned something other than 0...");
				ret = makeinteger(internal_value);
				return ret;
		}
	}
	else if(!strncasecmp(op->object.str,"set",3))
	{
		switch(access)
		{
			default: break;	
		}
	}
	else
		return makeerror(2,0,"unknown operation type...");
	return e->strue;
}
SExp *
f_peekchar(SExp *s, Symbol *e)
{
	SExp *port = e->snil, *ret = e->snil;
	char c = 0;
	int itmp = pairlength(s);
	if(itmp > 1)
		return makeerror(2,0,"peek-char p : PORT => CHAR?");
	if(itmp)
		port = car(s);
	else
	{
		port = (SExp *)hmalloc(sizeof(SExp));
		port->type = PORT;
		port->object.p = (Port *)hmalloc(sizeof(Port));
		port->object.p->type = PFILE;
		FILEPORT(port) = stdin;
	}
	if(port->type != PORT)
		return makeerror(2,0,"p *must* be bound to a PORT argument..");
	c = fgetc(FILEPORT(port));
	if(feof(FILEPORT(port)))
			return e->seof;
	else if(ferror(FILEPORT(port)))
			return makeerror(2,0,"an error occurred on requested port...");
	ungetc(c,FILEPORT(port));
	ret = (SExp *)hmalloc(sizeof(SExp));
	ret->type = CHAR;
	ret->object.c = c;
	return ret;
}
SExp *
f_char_ready(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_chown(SExp *s, Symbol *e)
{
	SExp *file = e->snil, *uid = e->snil, *gid = e->snil, *symp = e->snil;
	int rc = 0;
	/* add a symbolic? flag to tell whether to use chown or lchown...*/
	if(pairlength(s) != 4)
		return makeerror(2,0,"chown path : STRING uid : INTEGER gid : INTEGER symbolic? : BOOLEAN=> BOOLEAN?");
	file = car(s);
	if(file->type != STRING)
		return makeerror(2,0,"chown's path argument *must* be bound to a STRING object");
	uid = car(cdr(s));
	if(uid->type != NUMBER || (uid->type == NUMBER && NTYPE(uid) != INTEGER))
		return makeerror(2,0,"chown's uid argument *must* be bound to an INTEGER object");
	gid = car(cdr(cdr(s)));
	if(gid->type != NUMBER || (gid->type == NUMBER && NTYPE(uid) != INTEGER))
		return makeerror(2,0,"chown's uid argument *must* be bound to an INTEGER object");
	symp = car(cdr(cdr(cdr(s))));
	if(symp->type != BOOL)
		return makeerror(2,0,"chown's uid argument *must* be bound to an INTEGER object");
	if(symp == e->strue)
	{
		if((rc = lchown(file->object.str,uid->object.n->nobject.z,gid->object.n->nobject.z)) != 0)
			return makeerror(2,0,"lchown returned something other than 0...");
	}
	else
	{
		if((rc = chown(file->object.str,uid->object.n->nobject.z,gid->object.n->nobject.z)) != 0)
			return makeerror(2,0,"chown returned something other than 0...");
	}
	return e->strue;
}
SExp *
f_chmod(SExp *s, Symbol *e)
{
	SExp *file = e->snil, *mode = e->snil;
	int rc = 0;
	/* add a symbolic? flag to tell whether to use chown or lchown...*/
	if(pairlength(s) != 2)
		return makeerror(2,0,"chmod path : STRING mode : INTEGER => BOOLEAN?");
	file = car(s);
	if(file->type != STRING)
		return makeerror(2,0,"chmod's path argument *must* be bound to a STRING object");
	mode = car(cdr(s));
	if(mode->type != NUMBER || (mode->type == NUMBER && NTYPE(mode) != INTEGER))
		return makeerror(2,0,"chmod's mode argument *must* be bound to an INTEGER object");
	if(chmod(file->object.str,mode->object.n->nobject.z))
		return makeerror(2,0,"chmod returned something other than 0");
	return e->strue;
}
SExp *
f_chroot(SExp *s, Symbol *e)
{
	SExp *file = e->snil;
	int rc = 0;
	/* add a symbolic? flag to tell whether to use chown or lchown...*/
	if(pairlength(s) != 2)
		return makeerror(2,0,"chroot path : STRING => BOOLEAN?");
	file = car(s);
	if(file->type != STRING)
		return makeerror(2,0,"chroot's path argument *must* be bound to a STRING object");
	if(chroot(file->object.str))
		return makeerror(2,0,"chroot returned something other than 0...");
	return e->strue;
}
SExp *
f_umask(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_port_filename(SExp *s, Symbol *e)
{
	SExp *port = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"port-filename p : PORT => STRING");
	port = car(s);
	if(port->type != PORT)
		return makeerror(2,0,"port *must* be bound to a PORT object");
	return makestring(FILEADDRESS(port));
}
SExp *
f_port_mode(SExp *s, Symbol *e)
{
	SExp *port = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"port-filename p : PORT => STRING");
	port = car(s);
	if(port->type != PORT)
		return makeerror(2,0,"port *must* be bound to a PORT object");
	return makestring(FILEMODE(port));
}
SExp *
f_port_bind(SExp *s, Symbol *e)
{
	SExp *port = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"port-filename p : PORT => STRING");
	port = car(s);
	if(port->type != PORT)
		return makeerror(2,0,"port *must* be bound to a PORT object");
	return makeinteger(NETBIND(port));
}
SExp *
f_port_proto(SExp *s, Symbol *e)
{
	SExp *port = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"port-filename p : PORT => STRING");
	port = car(s);
	if(port->type != PORT)
		return makeerror(2,0,"port *must* be bound to a PORT object");
	return makeinteger(PROTONUMBER(port));
}
SExp *
f_port_state(SExp *s, Symbol *e)
{
	SExp *port = e->snil, *ret = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"port-state p : PORT => BOOLEAN");
	port = car(s);
	if(port->type != PORT)
		return makeerror(2,0,"port-state's p argument *must* be bound to a PORT object");
    if(PORT(port)->state)
        return e->strue;
    return e->sfalse;
}	
SExp *
f_port_type(SExp *s, Symbol *e)
{
	SExp *port = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"port-type p : PORT => INTEGER");
	port = car(s);
	if(port->type != PORT)
		return makeerror(2,0,"port-type's p argument *must* be bound to a PORT object");
	return makeinteger(PTYPE(port));
}	
SExp *
f_end_of_portp(SExp *s, Symbol *e)
{
	SExp *port = e->snil;
	if(pairlength(s) != 1)
		return makeerror(2,0,"end-of-port? p : PORT => BOOLEAN");
    port = car(s);
    if(port->type != PORT)
        return makeerror(2,0,"end-of-port?'s sole argument *must* be bound to a PORT object");
    switch(PTYPE(port))
    {
        case PFILE:
            if(feof(FILEPORT(port)))
                return e->strue;
            else
                return e->sfalse;
        default:
            return makeerror(2,0,"end-of-port? only operates on FILE ports.");
    }
}
SExp *
f_set_input(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_set_output(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_set_error(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_cur_input(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_cur_output(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_cur_error(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_random(SExp *s, Symbol *e)
{
	SExp *ret = e->snil, *tmp = e->snil;
	int itmp = 0;
	itmp = pairlength(s);
	switch(itmp)
	{
		case 0:
			ret = makereal(drand48());
			break;
		case 1:
			tmp = car(s);
			if(tmp->type == NUMBER && NTYPE(tmp) == INTEGER)
			{
				ret = makeinteger(random() % tmp->object.n->nobject.z);
				break;
			}
			/*else if(tmp->type == NUMBER && NTYPE(tmp) == REAL)
			{
				//srand48(tmp->object.n->nobject.real);
				ret = makereal(drand48());
				break;
			}*/
			// makeerror(1,0,"random [seed : (INTEGER | REAL)] => NUMBER?");
			else
				return makeerror(1,0,"argument type clash in random: seed *must* be bound to an INTEGER");
			break;
		default:
			return makeerror(1,0,"random [seed : INTEGER] => NUMBER?");
	}
	return ret;
}
SExp *
f_seed_random(SExp *s, Symbol *e)
{
	SExp *ret = e->snil, *tmp1 = e->snil;
	int itmp = 0;
	itmp = pairlength(s);
	if(itmp != 1)
		return makeerror(2,0,"seed-random seed : NUMBER => BOOLEAN?");
	tmp1 = car(s);
	if(tmp1->type != NUMBER)
		return makeerror(2,0,"seed-random's seed argument *must* be bound to a NUMBER");
	switch(NTYPE(tmp1))
	{
		case INTEGER:
			srandom(tmp1->object.n->nobject.z);
			break;
		case RATIONAL:
			srand48((NUM(tmp1) * 1.0) / (DEN(tmp1) * 1.0f));
			break;
		case REAL:
			srand48(tmp1->object.n->nobject.real);
			break;
		case COMPLEX:
			srand48(IMMAG(tmp1) + CEREAL(tmp1));
			break;
	}
	return e->svoid;	
}
SExp *
f_time(SExp *s, Symbol *e)
{
	return makeinteger(time(nil));
}
SExp *
f_getenv(SExp *s, Symbol *e)
{
	SExp *ret = nil, *path = nil;
	char *data = nil;
	/* oh, right; we're just pasing in a list to these functions...
	 * need to specify *arity* to register_procedure...
	 */
	if(s == nil || s == e->snil)
		return makeerror(1,0,"sys/getenv: error with arguments");
	path = car(s);
	if(path->type != STRING)
		return makeerror(1,0,"sys/getenv: typeclash: ENV is indexed on strings");
	data = getenv(path->object.str);
	if(data == nil)
		return e->sfalse;
	return makestring(data);
}
SExp *
f_setenv(SExp *s, Symbol *e)
{
	return e->svoid;
}
SExp *
f_sysfcntl(SExp *s, Symbol *e)
{
	int rc = 0, fl = 0, set = 0;
	SExp *fd = nil, *cmd = nil, *arg = nil;
	if(s == nil || s == e->snil)
		return makeerror(1,0,"sys/fcntl: argument error");
	fd = car(s);
	if(fd->type != PORT || (fd->type == PORT && PTYPE(fd) == PSTRING))
		return makeerror(1,0,"sys/fcntl: fd argument *must* be a Network or String port");
	cmd = car(cdr(s));
	if(cmd->type != ATOM && cmd->type != KEY && cmd->type != STRING)
		return makeerror(1,0,"sys/fcntl: cmd *must* be a keyobj, an atom or a string");
	/*LINE_DEBUG;
	printf("fd == %d\n",fd->type);
	LINE_DEBUG;
	printf("PORTTYPE(fd) == %d\n",PTYPE(fd));
	LINE_DEBUG;
	*/
	if(!strncmp("dupfd",cmd->object.str,5))
		fl = F_DUPFD;
	else if(!strncmp("setfd",cmd->object.str,5))
	{
		set = 1;
		fl = F_SETFD;
	}
	else if(!strncmp("getfd",cmd->object.str,5))
		fl = F_GETFD;
	else if(!strncmp("getfl",cmd->object.str,5))
		fl = F_GETFL;
	else if(!strncmp("setfl",cmd->object.str,5))
	{
		set = 1;
		fl = F_SETFL;
	}
	else
		return makeerror(1,0,"sys/fcntl: unknown command");
	if(set)
	{
		arg = car(cdr(cdr(s)));
		if(arg == e->snil)
			return makeerror(1,0,"sys/fcntl: SET command without SET argument");
		if(arg->type != NUMBER || (arg->type == NUMBER && NTYPE(arg) != INTEGER))
			return makeerror(1,0,"sys/fcntl: SET command with non-INTEGER argument");
		rc = fcntl(fileno(FILEPORT(fd)),fl,AINT(arg));
	}
	else
		rc = fcntl(fileno(FILEPORT(fd)),fl,0);
	if(rc == -1)
	{	
		printf("FCNTL returned -1; errno == %d\n", errno);
		printf("%s\n",strerror(errno));
	}
	return makeinteger(rc);
}
SExp *
f_sysfcntlconst(SExp *s, Symbol *e)
{
	SExp *cmd = nil;
	if(s == nil || s == e->snil)
		return e->sfalse;
	cmd = car(s);
	if(cmd->type != ATOM && cmd->type != STRING && cmd->type != KEY)
		return makeerror(1,0,"sys/fcntl-const: type clash");
	if(!strncasecmp("nonblock",cmd->object.str,8))
		return makeinteger(O_NONBLOCK);
	else if(!strncasecmp("append",cmd->object.str,6))
		return makeinteger(O_APPEND);
#ifndef CYGWIN
	else if(!strncasecmp("async",cmd->object.str,5))
		return makeinteger(O_ASYNC);
#endif
	else if(!strncasecmp("sync",cmd->object.str,4))
		return makeinteger(O_SYNC);
	else
		return makeerror(1,0,"sys/fcntl-const: unknown constant name");
}
SExp *
f_syssleep(SExp *s, Symbol *e)
{
	int rc = 0;
	SExp *tmp = nil;
	if(pairlength(s) != 1)
		return makeerror(1,0,"sys/sleep time : INTEGER => INTEGER");
	tmp = car(s);
	if(tmp->type != NUMBER || (tmp->type == NUMBER && NTYPE(tmp) != INTEGER))
		return makeerror(1,0,"sys/sleep: type clash; syssleep's time parameter *must* be an integer");
	rc = sleep(AINT(tmp));
	return makeinteger(rc);
}
SExp *
f_sysusleep(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_sysnanosleep(SExp *s, Symbol *e)
{
	return e->snil;
}
SExp *
f_sysselect(SExp *s, Symbol *e)
{
    int idx = 0;
    if(s->type != PAIR && s->type != VECTOR)
        return makeerror(1,0,"sys/select: sys/select's parameter must be a sequence of type (PAIR PORT) | (VECTOR PORT)");

	return e->snil;
}

SExp *
f_sysgetpid(SExp *s, Symbol *e)
{
    pid_t i = 0;
    i = getpid();
    return makeinteger(i);
}

SExp *
f_sysgetppid(SExp *s, Symbol *e)
{
    pid_t i = 0;
    i = getppid();
    return makeinteger(i);
}

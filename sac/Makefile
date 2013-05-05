CC=cc
LIBS=-lgc -lm
TEXE=typhon
CEXE=ceres
RTSFILES=posix.c main.c vesta.c
TFILE=typhon.ss.c
CFILE=ceres.ss.c
CFLAGS=
typhon: $(TFILE) $(RTSFILES)
	$(CC) -o $(TEXE) $(TFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
opt-typhon: $(TFILE) $(RTSFILES)
	$(CC) -O2 -o $(TEXE) $(TFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
ceres: $(CFILE) $(RTSFILES)
	$(CC) -o $(CEXE) $(CFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
opt-ceres: $(CFILE) $(RTSFILES)
	$(CC) -O2 -o $(CEXE) $(CFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
clean:
	@rm -f $(EXE)

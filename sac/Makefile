CC=cc
LIBS=-lgc -lm
HEXE=hydra
CEXE=ceres
RTSFILES=posix.c main.c vesta.c
HFILE=hydra.ss.c
CFILE=ceres.ss.c
CFLAGS=
hydra: $(HFILE) $(RTSFILES)
	$(CC) -o $(HEXE) $(HFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
opt-hydra: $(HFILE) $(RTSFILES)
	$(CC) -O2 -o $(HEXE) $(HFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
ceres: $(CFILE) $(RTSFILES)
	$(CC) -o $(CEXE) $(CFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
opt-ceres: $(CFILE) $(RTSFILES)
	$(CC) -O2 -o $(CEXE) $(CFILE) $(RTSFILES) $(LIBS) $(CFLAGS)
clean:
	@rm -f $(EXE)

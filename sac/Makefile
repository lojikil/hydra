CC=cc
LIBS=-lgc -lm
HEXE=hydra
CEXE=ceres
RTSFILES=posix.c main.c vesta.c
HFILE=hydra.ss.c
CFILE=ceres.ss.c
hydra: $(HFILE) $(RTSFILES)
	$(CC) -o $(HEXE) $(HFILE) $(RTSFILES) $(LIBS)
opt-hydra: $(HFILE) $(RTSFILES)
	$(CC) -O2 -o $(HEXE) $(HFILE) $(RTSFILES) $(LIBS)
ceres: $(CFILE) $(RTSFILES)
	$(CC) -o $(CEXE) $(CFILE) $(RTSFILES) $(LIBS)
opt-ceres: $(CFILE) $(RTSFILES)
	$(CC) -O2 -o $(CEXE) $(CFILE) $(RTSFILES) $(LIBS)
clean:
	@rm -f $(EXE)

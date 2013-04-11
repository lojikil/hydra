CC=cc
LIBS=-lgc -lm
HEXE=hydra
SEXE=sac/hydra
CEXE=sac/ceres
RTSFILES=posix.c main.c vesta.c
HFILE=hydra.ss
SFILE=sac/hydra.ss
CFILE=ceres.ss.c
hydra: $(HFILE)
	@./compile.ss $(HFILE)
sac-hydra: $(SFILE)
	@./compile.ss $(SFILE)
	@(cd ./sac && make hydra)
sac-opt-hydra: $(SFILE)
	@./compile.ss $(SFILE)
	@(cd ./sac && make opt-hydra)
clean:
	@rm -f $(EXE) $(SEXE) $(CEXE)

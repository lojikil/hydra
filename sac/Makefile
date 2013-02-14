CC=cc
LIBS=-lgc -lm
EXE=hydra
FILES=hydra.ss.c posix.c main.c vesta.c
hydra: $(FILES)
	$(CC) -o $(EXE) $(FILES) $(LIBS)
clean:
	@rm -f $(EXE)

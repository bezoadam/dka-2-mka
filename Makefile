NAME=dka-2-mka
CC=ghc
FLAGS=-o

all: clean dka-2-mka

dka-2-mka:
	$(CC) $(FLAGS) $(NAME) Main.hs

clean:
	rm -f dka-2-mka
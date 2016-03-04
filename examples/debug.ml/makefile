
OCB_FLAGS = -use-ocamlfind -I src

OCB = ocamlbuild $(OCB_FLAGS)

all: clean
	$(OCB) main.byte
	./main.byte -dre .*

clean:
	$(OCB) -clean


all: clean
	ocamlbuild -use-ocamlfind main.byte
	./main.byte

clean:
	ocamlbuild -clean
	rm -f *.byte

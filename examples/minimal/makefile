
all: clean
	ocamlbuild -use-ocamlfind main.byte
	./main.byte

clean:
	ocamlbuild -clean

POLYPRINT_PATH = $(shell ocamlfind query ppx_polyprint)

debug:
	ocamlfind ppx_tools/rewriter $(POLYPRINT_PATH)/ppx_polyprint.native \
		-I $(POLYPRINT_PATH) main.ml

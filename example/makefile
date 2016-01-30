
all:
	ocamlbuild -use-ocamlfind print.byte
	./print.byte

POLYPRINT_PATH = $(shell ocamlfind query ppx_polyprint)

debug:
	ocamlfind ppx_tools/rewriter $(POLYPRINT_PATH)/ppx_polyprint.native \
		-I $(POLYPRINT_PATH) print.ml

clean:
	ocamlbuild -clean

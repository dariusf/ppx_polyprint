
OCB_FLAGS = -use-ocamlfind -I src

OCB = /home/chinwn/.opam/4.02.3/bin/ocamlbuild $(OCB_FLAGS)

all: clean
	$(OCB) print.byte
	./print.byte -dre .*
# fact|plus

# ./print.byte -dre fact
clean:
	$(OCB) -clean

###########################

POLYPRINT_PATH = $(shell ocamlfind query ppx_polyprint)

debug:
	ocamlfind ppx_tools/rewriter $(POLYPRINT_PATH)/ppx_polyprint.native \
		-I $(POLYPRINT_PATH) \
		-I _build/src \
		src/print.ml

manual: clean
	ocamlc -ppx ../ppx_polyprint/ppx_polyprint.native -I ../ppx_polyprint/_build/src -dsource print.ml

c: clean
	ocamlc -I ../ppx_polyprint/_build/src polyPrint.cma -ppx ../ppx_polyprint/ppx_polyprint.native print.ml -o print.byte
	./print.byte

# manual:
# 	ocamlc -ppx $(shell ocamlfind query ppx_log)/ppx_log.native -ppx $(shell ocamlfind query ppx_polyprint)/ppx_polyprint.native -I $(shell ocamlfind query ppx_polyprint) polyPrint.cma -dsource print.ml

# auto:
# 	ocamlbuild -use-ocamlfind \
# 		-tag "ppx($(shell ocamlfind query ppx_log)/ppx_log.native)" \
# 		-tag "ppx($(shell ocamlfind query ppx_polyprint)/ppx_polyprint.native)" \
# 		-package ppx_polyprint \
# 		print.byte


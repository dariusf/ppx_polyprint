
PACKAGE = ppx_polyprint
LIB = PolyPrint
TEST = test

INSTALL = META \
	_build/src/$(LIB).cmi \
	_build/src/$(PACKAGE) \
	_build/src/$(LIB).cma \
	_build/src/$(LIB).cmxa \
	_build/src/$(LIB).a

OCB_FLAGS = -use-ocamlfind

OCB = ocamlbuild $(OCB_FLAGS)

all: runtime ppx

.PHONY: runtime ppx test examples doc clean

runtime:
	$(OCB) $(LIB).cma
	$(OCB) $(LIB).cmxa

ppx:
	$(OCB) $(PACKAGE).native
	cp $(PACKAGE).native _build/src/$(PACKAGE)

test:
	rm -rf _build/test/
	$(OCB) test/$(TEST).byte
	./$(TEST).byte --show-errors

examples: up
	for d in examples/*/; do \
		make -C $$d; \
	done

doc:
	$(OCB) -use-ocamlfind doc/api.docdir/index.html \
		-docflags -t -docflag "API reference for $(PACKAGE)" \
		-docflags '-colorize-code -short-functors -charset utf-8' \
		-docflags '-css-style style.css'
	cp doc/style.css api.docdir/

clean:
	$(OCB) -clean
	rm -rf _tests
	rm -f *.byte
	for d in examples/*/; do \
		make -C $$d clean; \
	done

# opam

.PHONY: install remove up down

install: all
	ocamlfind install $(PACKAGE) $(INSTALL)

remove:
	ocamlfind remove $(PACKAGE)

up: clean
	opam pin add $(PACKAGE) . -n -y
	opam install $(PACKAGE) -v -y

down:
	opam remove $(PACKAGE)
	opam pin remove $(PACKAGE)

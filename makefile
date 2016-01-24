
PACKAGE = ppx_polyprint
LIB = polyPrint

TEST = test

INSTALL = META \
	_build/src/$(LIB).cmi \
	_build/src/$(PACKAGE).native \
	_build/src/$(LIB).cma

OCB_FLAGS = -use-ocamlfind -I src -I test

OCB = ocamlbuild $(OCB_FLAGS)

all: build

.PHONY: build ppx runtime test doc clean

build: ppx runtime

ppx:
	$(OCB) $(PACKAGE).native

runtime:
	$(OCB) $(LIB).cma

test: all
	rm -rf _build/test/
	$(OCB) -classic-display test/$(TEST).byte --

doc:
	$(OCB) -use-ocamlfind doc/api.docdir/index.html \
		-docflags -t -docflag "API reference for $(PACKAGE)" \
		-docflags '-colorize-code -short-functors -charset utf-8' \
		-docflags '-css-style style.css'
	cp doc/style.css api.docdir/

clean:
	$(OCB) -clean

# opam

.PHONY: install remove up down

install: all
	ocamlfind install $(PACKAGE) $(INSTALL)

remove:
	ocamlfind remove $(PACKAGE)

up:
	opam pin add $(PACKAGE) . -n
	opam install $(PACKAGE) --verbose

down:
	opam remove $(PACKAGE)
	opam pin remove $(PACKAGE)

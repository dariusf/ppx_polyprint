FROM ocaml/opam:ubuntu-14.04_ocaml-4.02.3
MAINTAINER Darius Foo

RUN opam update
RUN opam install ocamlfind

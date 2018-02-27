FROM ocaml/opam:ubuntu-16.04_ocaml-4.02.3

RUN opam update && opam install ocamlfind

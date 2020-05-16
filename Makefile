
DEBUG=
OCAMLFIND_BINDIR:=$(shell dirname `which ocamlfind`)

all: ocamlfind2 papr_official.exe

ocamlfind2: frontend.ml main.ml
	ocamlfind ocamlc $(DEBUG) $(INC) -package str,unix,findlib -linkall -linkpkg frontend.ml main.ml -o ocamlfind2

papr_official.exe: papr_official.ml
	ocamlfind ocamlc $(DEBUG) $(INC) -package str,unix,findlib,compiler-libs.common -linkall -linkpkg $< -o $@

install: ocamlfind2
	install ocamlfind2 $(OCAMLFIND_BINDIR)/ocamlfind2
	ocamlfind remove ocamlfind2 || true
	ocamlfind install ocamlfind2 META papr_official.exe

uninstall:
	rm -f  $(OCAMLFIND_BINDIR)/ocamlfind2
	ocamlfind remove ocamlfind2 || true

clean:
	rm -f *.cm* ocamlfind2

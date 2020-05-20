
DEBUG=
OCAMLFIND_BINDIR:=$(shell dirname `which ocamlfind`)

all: ocamlfind2 papr_official.exe

ocamlfind2: fsmod.ml frontend.ml main.ml
	ocamlfind ocamlc $(DEBUG) $(INC) -package str,unix,findlib,fmt,sexplib,rresult -linkall -linkpkg fsmod.ml frontend.ml main.ml -o ocamlfind2

fsmod.ml: fsmod.ORIG.ml
	ocamlfind2 preprocess -package camlp5,pa_ppx.deriving_plugins.show.syntax,pa_ppx.deriving_plugins.sexp.syntax,camlp5.pr_o.syntax \
	-syntax camlp5o $< > $@.NEW
	mv $@.NEW $@

papr_official.exe: papr_official.ml
	ocamlfind ocamlc $(DEBUG) $(INC) -package str,unix,findlib,compiler-libs.common \
	-linkall -linkpkg $< -o $@

install: ocamlfind2
	install ocamlfind2 $(OCAMLFIND_BINDIR)/ocamlfind2
	ocamlfind remove ocamlfind2 || true
	ocamlfind install ocamlfind2 META papr_official.exe

uninstall:
	rm -f  $(OCAMLFIND_BINDIR)/ocamlfind2
	ocamlfind remove ocamlfind2 || true

clean:
	rm -f *.cm* ocamlfind2

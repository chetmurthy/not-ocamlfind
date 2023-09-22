
DEBUG=
OCAMLFIND_BINDIR:=$(shell dirname `which ocamlfind`)
PACKAGES = str,unix,fmt,rresult,ocamlgraph,camlp-streams
INC= -I local-packages/ocamlfind/src/findlib findlib.cma

all: not-ocamlfind papr_official.exe

not-ocamlfind: fsmod.ml frontend.ml main.ml
	(cd  local-packages/ocamlfind/ && make)
	ocamlfind ocamlc $(DEBUG) $(INC) -package $(PACKAGES) -linkall -linkpkg fsmod.ml frontend.ml main.ml -o not-ocamlfind

bootstrap: fsmod.ORIG.ml
	not-ocamlfind preprocess -package camlp5,pa_ppx.deriving_plugins.show,camlp5.pr_o \
	-syntax camlp5o $< > fsmod.ml.NEW
	mv fsmod.ml.NEW fsmod.ml

papr_official.exe: papr_official.ml
	ocamlfind ocamlc $(DEBUG) $(INC) -package str,unix,compiler-libs.common \
	-linkall -linkpkg $< -o $@

install: not-ocamlfind
	install not-ocamlfind $(OCAMLFIND_BINDIR)/not-ocamlfind
	ocamlfind remove not-ocamlfind || true
	ocamlfind install not-ocamlfind META papr_official.exe

uninstall:
	rm -f  $(OCAMLFIND_BINDIR)/not-ocamlfind
	ocamlfind remove not-ocamlfind || true

clean:
	rm -f *.cm* not-ocamlfind *.exe
	(cd local-packages/ocamlfind && make clean)

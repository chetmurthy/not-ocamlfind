
DEBUG=
OCAMLFIND_BINDIR:=$(shell dirname `which ocamlfind`)
PACKAGES = str,unix,fmt,sexplib,rresult
#PACKAGES := $(PACKAGES),findlib
INC= -I local-packages/ocamlfind/src/findlib findlib.cma

all: not-ocamlfind papr_official.exe

not-ocamlfind: fsmod.ml frontend.ml main.ml
	(cd  local-packages/ocamlfind/ && make)
	ocamlfind ocamlc $(DEBUG) $(INC) -package $(PACKAGES) -linkall -linkpkg fsmod.ml frontend.ml main.ml -o not-ocamlfind

fsmod.ml: fsmod.ORIG.ml
	not-ocamlfind preprocess -package camlp5,pa_ppx.deriving_plugins.show.syntax,pa_ppx.deriving_plugins.sexp.syntax,camlp5.pr_o.syntax \
	-syntax camlp5o $< > $@.NEW
	mv $@.NEW $@

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

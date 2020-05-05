
DEBUG=
INC=-I ../ocamlfind/src/findlib
OCAMLFIND_BINDIR:=$(shell dirname `which ocamlfind`)

ocamlfind2: frontend.ml main.ml
	ocamlfind ocamlc $(DEBUG) $(INC) -package str,unix,findlib -linkall -linkpkg ocaml_args.cmo frontend.ml main.ml -o ocamlfind2

install: ocamlfind2
	install ocamlfind2 $(OCAMLFIND_BINDIR)/ocamlfind2

uninstall:
	rm -f  $(OCAMLFIND_BINDIR)/ocamlfind2

clean:
	rm -f *.cm* ocamlfind2

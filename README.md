# not-ocamlfind

Front-end to ocamlfind to add a few new commands

# Features

The command `not-ocamlfind` is a pass-thru to `ocamlfind`, but adds two new commands: `preprocess` and `reinstall-if-diff`.

### `reinstall-if-diff`

1. `reinstall-if-diff` does what it says on the label: a regular `ocamlfind install` command like
```
ocamlfind install pa_ppx_base -destdir $(DESTDIR)/lib META $(TARGET) pa_ppx_base.cmx pa_ppx_base.cmi
```
installs a package `pa_ppx_base`.  Rewrite that to 
```
ocamlfind reinstall-if-diff pa_ppx_base -destdir $(DESTDIR)/lib META $(TARGET) pa_ppx_base.cmx pa_ppx_base.cmi
```
and it check (a) that the package is already-installed, (b) that the files-to-be-installed are identical in names and checksums to the files already-installed, (c) if not does
```
ocamlfind remove pa_ppx_base
ocamlfind install pa_ppx_base -destdir $(DESTDIR)/lib META $(TARGET) pa_ppx_base.cmx pa_ppx_base.cmi
```
and (d) if they are identical in names and checksums, does *nothing*.

### `preprocess`

A compile command using PPX rewriters
```
ocamlfind ocamlc -package ounit2,ppx_deriving.show -c test_deriving_show.ml
```
can be coaxed to produce its preprocessed output by adding "-dsource".  But this is messy and ugly, esp. when we might want to use that source for further compilation.  It also isn't very helpful when dealing with a multi-stage PPX rewriter sequence.  `preprocess` produces the source and does not attempt to compile it; as an added benefit, it prints (to stderr) the commands it executed to produce that source.  So you can use this for debugging  multi-stage PPX rewriter sequences:
```
ocamlfind ocamlc -package ounit2,ppx_deriving.show test_deriving_show.ml
```
Note well that all the options destined only for the compiler, linker, etc, are gone.

This can work for camlp5 also, but that little bit of documentation is TBD.

# Installation

To install `not-ocamlfind` you need a slightly-patched version of
ocamlfind (branch `export-more-cmis`).  There are two ways of doing
this:

## Via opam

You can clone the patched repo, pin it with opam, and build
`not-ocamlfind` after.

```
git clone git@github.com:chetmurthy/ocamlfind.git
cd ocamlfind
git checkout export-more-cmis
opam pin ocamlfind `pwd`
```

At this point you can build `not-ocamlfind` with:
```
make
make install
```

or (via opam)
```
opam pin .
```

## Via a local copy of `ocamlfind`

Again, clone ocamlfind, build it, and then point `not-ocamlfind` at 
that tree (I assume you'll know how to configure it for your local
environment).

```
git clone git@github.com:chetmurthy/ocamlfind.git
cd ocamlfind
git checkout export-more-cmis
./configure
make
```

then look at `./Makefile` and follow instructions to uncomment `INC` and comment `PACKAGES:=`
and 
```
make
make install
```

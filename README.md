# not-ocamlfind

Front-end to ocamlfind to add a few new commands

# Installation

To install `not-ocamlfind` you need a slightly-patched version of
ocamlfind.  There are two ways of doing this:

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

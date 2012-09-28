all: test

setup.ml _tags myocamlbuild.ml: _oasis
	oasis setup

configure setup.data: setup.ml _tags
	ocaml setup.ml -configure --enable-debug --enable-tests --override ocamlbuildflags -no-links

build: setup.data myocamlbuild.ml
	ocaml setup.ml -build

test: build
	OCAMLRUNPARAM=b,l=64K ocaml setup.ml -test

.PHONY: all configure build test

DUNE=~/.opam/system/bin/dune

all: build

build:
#--profile release
	$(DUNE) build

install:
	$(DUNE) install

run: build
	./main.native -v tests/test.c

doc:
	cd src/; \
	ocamlbuild -use-menhir -use-ocamlfind -pkgs zarith -docflag -all-params analyser.docdir/index.html; \
	cd ..; \
	mv src/_build/analyser.docdir/* docs/

clean:
	$(DUNE) clean
	rm -rf src/_build/

.PHONY: build clean

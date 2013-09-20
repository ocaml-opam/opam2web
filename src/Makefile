.PHONY: all run install clean .FORCE

COMMIT=$(shell git rev-parse HEAD)
GIT_DESCR=$(shell git describe)
DIRTY=$(shell git diff-index --quiet HEAD; echo $$?)

PREFIX=/usr/local
WWWDIR=www

OCAMLBUILD=ocamlbuild -classic-display
PACKAGES=-pkgs cow,cow.syntax,opam.client,cmdliner
SYNTAX=-tags "syntax(camlp4o)"
FLAGS=-use-ocamlfind -cflags "-bin-annot" -no-links -tags "debug"
INCLUDES=-I apalog
TARGET=opam2web

JS_OF_OCAML=js_of_ocaml
JSFLAGS=-package js_of_ocaml.syntax,js_of_ocaml -syntax camlp4o

all: $(TARGET).native search.js

version.ml: version.ml.in .FORCE
	sed s/%%COMMIT%%/$(COMMIT)/ < version.ml.in \
	| sed s/%%GIT_DESCR%%/$(GIT_DESCR)/ \
	| sed s/%%DIRTY%%/$(DIRTY)/ > version.ml

$(TARGET).native: version.ml
	$(OCAMLBUILD) $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) $@

%.js: %.ml
	ocamlfind ocamlc -linkpkg -o $*.byte $(JSFLAGS) $<
	mv $*.cmo $*.cmi _build
	$(JS_OF_OCAML) $*.byte

run: $(TARGET).native
	_build/$< -c ../content -o $(WWWDIR)

install:
	install _build/opam2web.native $(PREFIX)/bin/$(TARGET)

clean:
	ocamlbuild -clean
	rm -rf *.cmo *.cmi *.cmt search.js search.byte
	rm -rf $(WWWDIR)

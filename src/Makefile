.PHONY: all run install clean

VERSION=1.3.0

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

version.ml: version.ml.in
	sed s/%%VERSION%%/$(VERSION)/ < version.ml.in > version.ml

$(TARGET).native: version.ml
	$(OCAMLBUILD) $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) $@

%.js: %.ml
	ocamlfind ocamlc -linkpkg -o $*.byte $(JSFLAGS) $<
	mv $*.cmo $*.cmi _build
	$(JS_OF_OCAML) $*.byte

run: $(TARGET).native
	_build/$< -c ../content -o $(WWWDIR) --remote default

install:
	install _build/opam2web.native $(PREFIX)/bin/$(TARGET)

clean:
	ocamlbuild -clean
	rm -rf *.cmo *.cmi *.cmt search.js search.byte
	rm -rf $(WWWDIR)

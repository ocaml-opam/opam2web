.PHONY: all run clean

PREFIX=/usr/local
WWWDIR=www

OCAMLBUILD=ocamlbuild -classic-display
PACKAGES=-pkgs cow,cow.syntax,opam.client
SYNTAX=-tags "syntax(camlp4o)"
FLAGS=-use-ocamlfind -cflags "-bin-annot" -no-links -tags "debug"
INCLUDES=-I apalog
TARGET=opam2web

JS_OF_OCAML=js_of_ocaml
JSFLAGS=-package js_of_ocaml.syntax,js_of_ocaml -syntax camlp4o

all: $(TARGET).native search

$(TARGET).native:
	$(OCAMLBUILD) $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) $@

search: search.ml
	ocamlfind ocamlc -linkpkg -o $@.byte $(JSFLAGS) $<
	mv $@.cmo $@.cmi _build
	$(JS_OF_OCAML) $@.byte

run: $(TARGET).native
	_build/$< -c ../content -o $(WWWDIR) -l default

install: $(TARGET).native
	install _build/$< $(PREFIX)/bin/$(TARGET)

clean:
	ocamlbuild -clean
	rm -rf *.cmo *.cmi *.cmt search search.js search.byte
	rm -rf $(WWWDIR)

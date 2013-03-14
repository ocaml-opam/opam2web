.PHONY: all run clean

PREFIX="/usr"
WWWDIR="www"

NATIVE=opam2web
JS_OF_OCAML=js_of_ocaml
OCAMLFLAGS=-package js_of_ocaml.syntax -syntax camlp4o

all: $(NATIVE).native search

$(NATIVE).native:
	./cmd config
	ocamlbuild -classic-display $@

search: search.ml
	ocamlfind ocamlc -linkpkg -o $@.byte $(OCAMLFLAGS) -package js_of_ocaml $<
	mv $@.cmo $@.cmi _build
	$(JS_OF_OCAML) $@.byte

run: $(NATIVE).native
	_build/$< -c ../content -o $(WWWDIR) -l default

install: $(NATIVE).native
	install _build/$< $(PREFIX)/bin/$(NATIVE)

clean:
	ocamlbuild -clean
	rm -rf *.cmo *.cmi *.cmt search search.js search.byte
	rm -rf _config/ $(WWWDIR)

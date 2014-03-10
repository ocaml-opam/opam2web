.PHONY: all run \
	install install-bin install-lib \
	uninstall uninstall-bin uninstall-lib \
	clean .FORCE

COMMIT=$(shell git rev-parse HEAD)
GIT_DESCR=$(shell git describe)
DIRTY=$(shell git diff-index --quiet HEAD; echo $$?)

PREFIX ?= /usr/local
WWWDIR ?= www

BUILD=_build

OCAMLBUILD=ocamlbuild -classic-display
PACKAGES=-pkgs cow,cow.syntax,opam.client,opamfu.cli,cmdliner
SYNTAX=-tags "syntax(camlp4o)"
FLAGS=-use-ocamlfind -cflags "-bin-annot" -no-links -tags "debug"
INCLUDES=-I apalog
TARGET=opam2web

JS_OF_OCAML=js_of_ocaml
JSFLAGS=-package js_of_ocaml.syntax,js_of_ocaml -syntax camlp4o

SOURCE=$(wildcard *.ml) $(wildcard *.mli) version.ml
ARCHIVES=$(TARGET).cma $(TARGET).cmxa $(TARGET).a
PRODUCTS=$(addprefix $(BUILD)/,$(ARCHIVES) $(TARGET).native) search.js

all: $(PRODUCTS)

version.ml: version.ml.in .FORCE
	sed s/%%COMMIT%%/$(COMMIT)/ < version.ml.in \
	| sed s/%%GIT_DESCR%%/$(GIT_DESCR)/ \
	| sed s/%%DIRTY%%/$(DIRTY)/ > version.ml

$(BUILD)/$(TARGET).native: $(SOURCE)
	$(OCAMLBUILD) $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) $(TARGET).native

$(BUILD)/$(TARGET).cma: $(SOURCE)
	$(OCAMLBUILD) $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) $(TARGET).cma

$(BUILD)/$(TARGET).cmxa: $(SOURCE)
	$(OCAMLBUILD) $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) $(TARGET).cmxa

$(BUILD)/$(TARGET).a: $(SOURCE)
	$(OCAMLBUILD) $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) $(TARGET).a

%.js: %.ml
	ocamlfind ocamlc -linkpkg -o $*.byte $(JSFLAGS) $<
	mv $*.cmo $*.cmi $(BUILD)
	$(JS_OF_OCAML) $*.byte

run: $(BUILD)/$(TARGET).native
	$(BUILD)/$< -c ../content -o $(WWWDIR)

install-lib:
	ocamlfind install opam2web META \
		$(BUILD)/*.mli $(BUILD)/*.cmi $(addprefix $(BUILD)/,$(ARCHIVES))

install-bin:
	install $(BUILD)/opam2web.native $(PREFIX)/bin/$(TARGET)

install: install-bin install-lib

uninstall-lib:
	ocamlfind remove opam2web

uninstall-bin:
	rm -f $(PREFIX)/bin/$(TARGET)

uninstall: uninstall-bin uninstall-lib

clean:
	ocamlbuild -clean
	rm -rf *.cmo *.cmi *.cmt search.js search.byte
	rm -rf $(WWWDIR)

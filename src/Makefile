.PHONY: all run clean

WWWDIR="www/"

NATIVE=opam2web

all: $(NATIVE).native

$(NATIVE).native:
	./cmd config
	ocamlbuild -classic-display $@

run: $(NATIVE).native
	rm -rf $(WWWDIR)
	mkdir -p $(WWWDIR)/packages
	_build/$<

clean:
	ocamlbuild -clean
	rm -rf _config/ www/

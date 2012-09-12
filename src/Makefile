.PHONY: all run clean

WWWDIR="www"

NATIVE=opam2web

all: $(NATIVE).native

$(NATIVE).native:
	./cmd config
	ocamlbuild -classic-display $@

run: $(NATIVE).native
	_build/$< -o $(WWWDIR) -l default

clean:
	ocamlbuild -clean
	rm -rf _config/ $(WWWDIR)

.PHONY: build install uninstall clean test

build:
	dune build @install @js_search
	cp _build/default/src/js_search/search.bc.js ext/js/search.js
	cp _build/default/src/opam2web.exe opam2web

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test-prepare:
	rm -rf www
	mkdir -p www/content/doc www/content/blog
	cd www && \
	cp -r ../content . && \
	cp -r ../../opam.wiki/* content/doc/ && \
	cp -r ../../opam-blog/* content/blog/

test: build test-prepare
	cd www && \
	../src/_build/opam2web.native --content content path:. && \
	cp -r -L ../ext . && \
	xdg-open index.html

fulltest: build test-prepare
	cd www && \
	git clone git@github.com:ocaml/opam-repository -b master && \
	../src/_build/opam2web.native --content content path:opam-repository && \
	cp -r -L ../ext . && \
	xdg-open index.html

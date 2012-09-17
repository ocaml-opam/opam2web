.PHONY: all build run clean

WWWDIR="www"
JSDIR=$(WWWDIR)/js
all: build

build:
	cd src && $(MAKE)

run: build
	cd src && $(MAKE) run
	rm -rf $(WWWDIR)
	mv src/$(WWWDIR) . && cp -r ext/ $(WWWDIR)/
	mkdir -p $(JSDIR)
	mv src/search.js $(JSDIR)

clean:
	cd src && $(MAKE) clean
	rm -rf $(WWWDIR)

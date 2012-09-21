.PHONY: all build run clean

WWWDIR="www"
JSDIR=$(WWWDIR)/js
all: build

build:
	cd src && $(MAKE)

run: build
	cd src && $(MAKE) run
	rm -rf $(WWWDIR)
	mv src/$(WWWDIR) . && cd $(WWWDIR) && ln -s ../ext

clean:
	cd src && $(MAKE) clean
	rm -rf $(WWWDIR)

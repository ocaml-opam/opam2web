.PHONY: all build run clean

PREFIX="/usr"
WWWDIR="www"
JSDIR=$(WWWDIR)/js
all: build

build:
	cd src && $(MAKE)

run: build
	cd src && $(MAKE) run
	rm -rf $(WWWDIR)
	mv src/$(WWWDIR) . && cd $(WWWDIR) && ln -s ../ext

install: build
	cd src && $(MAKE) PREFIX=$(PREFIX) install

clean:
	cd src && $(MAKE) clean
	rm -rf $(WWWDIR)

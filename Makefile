.PHONY: all build run clean

WWWDIR="www"

all: build

build:
	cd src && $(MAKE)

run: build
	cd src && $(MAKE) run
	rm -rf $(WWWDIR)
	mv src/$(WWWDIR) . && cp -r ext/ $(WWWDIR)/

clean:
	cd src && $(MAKE) clean
	rm -rf $(WWWDIR)

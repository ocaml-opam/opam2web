.PHONY: all build run clean

WWWDIR="www"

all: build

build:
	cd src && $(MAKE)

run: build
	cd src && $(MAKE) run
	mkdir -p $(WWWDIR)
	mv src/$(WWWDIR)/* $(WWWDIR)/ && cp -r files/* $(WWWDIR)/

clean:
	cd src && $(MAKE) clean
	rm -rf $(WWWDIR)

.PHONY: all build clean

all: build

build:
	cd src && $(MAKE)

install: build
	cd src && $(MAKE) install

clean:
	cd src && $(MAKE) clean

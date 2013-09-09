.PHONY: all build install clean

all: build

build:
	cd src && $(MAKE)

install:
	cd src && $(MAKE) install

clean:
	cd src && $(MAKE) clean

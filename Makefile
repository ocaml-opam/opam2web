.PHONY: all build clean \
	install install-bin install-lib \
	uninstall uninstall-bin uninstall-lib

all: build

build:
	$(MAKE) -C src

install:
	$(MAKE) -C src install

install-bin:
	$(MAKE) -C src install-bin

install-lib:
	$(MAKE) -C src install-lib

uninstall:
	$(MAKE) -C src uninstall

uninstall-bin:
	$(MAKE) -C src uninstall-bin

uninstall-lib:
	$(MAKE) -C src uninstall-lib

clean:
	$(MAKE) -C src clean

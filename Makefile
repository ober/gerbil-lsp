OPENSSL_PREFIX := $(shell brew --prefix openssl@3 2>/dev/null)

ifdef OPENSSL_PREFIX
  export LIBRARY_PATH := $(OPENSSL_PREFIX)/lib:$(LIBRARY_PATH)
endif

build:
	gxpkg build

clean:
	gxpkg clean

install: build
	mkdir -p ~/.gerbil/bin
	cp .gerbil/bin/gerbil-lsp ~/.gerbil/bin/

.PHONY: build clean install

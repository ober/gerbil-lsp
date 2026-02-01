build:
	gxpkg build

clean:
	gxpkg clean

install: build
	cp .gerbil/bin/gerbil-lsp ~/.gerbil/bin/

.PHONY: build clean install

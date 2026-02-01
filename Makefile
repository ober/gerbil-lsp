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
	sudo cp .gerbil/bin/gerbil-lsp /usr/local/bin/

test: build
	@export GERBIL_PATH=$(CURDIR)/.gerbil; \
	for f in test/string-test.ss test/position-test.ss test/types-test.ss \
	          test/document-test.ss test/jsonrpc-test.ss test/transport-test.ss \
	          test/parser-test.ss test/symbols-test.ss test/module-test.ss \
	          test/completion-data-test.ss test/diagnostics-test.ss \
	          test/formatting-test.ss test/highlight-test.ss \
	          test/capabilities-test.ss test/state-test.ss test/sync-test.ss \
	          test/hover-test.ss test/definition-test.ss test/references-test.ss \
	          test/rename-test.ss test/completion-test.ss test/signature-test.ss \
	          test/folding-test.ss test/selection-test.ss test/links-test.ss \
	          test/index-test.ss test/semantic-tokens-test.ss \
	          test/inlay-hints-test.ss; do \
		echo "Running $$f..."; \
		gxi $$f || exit 1; \
	done; \
	echo "All tests passed."

.PHONY: build clean install test

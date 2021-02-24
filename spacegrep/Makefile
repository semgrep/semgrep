# Choices for DUNE_PROFILE are 'default' and 'static'.
ifndef DUNE_PROFILE
  DUNE_PROFILE = default
endif
export DUNE_PROFILE

# Build a single executable. Its behavior depends on the command name:
#
# - spacegrep (default)
# - spacecat
#
.PHONY: build
build:
	rm -rf bin
	dune build --profile $(DUNE_PROFILE)
	mkdir -p bin
	ln -sf ../_build/install/default/bin/spacegrep bin/spacegrep
	ln -sf spacegrep bin/spacecat

.PHONY: install
install:
	dune install

.PHONY: examples
examples:
	./scripts/run-examples

.PHONY: test
test:
	dune exec src/test/test.exe
	$(MAKE) examples > /dev/null

.PHONY: clean
clean:
	git clean -dfX
	rm -rf bin

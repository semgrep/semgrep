#
# Spacegrep's library and executables are built as part of semgrep-core.
# This 'build' target is for building just the spacegrep stuff and having
# 'spacegrep' and 'spacecat' handy in the local 'bin' folder.
#
.PHONY: build
build:
	dune build
	$(MAKE) symlinks

.PHONY: symlinks
symlinks:
	mkdir -p bin
	ln -sf ../../../_build/install/default/bin/spacegrep bin/spacegrep
	ln -sf spacegrep bin/spacecat

.PHONY: test
test:
	dune exec -- src/test/test.exe -e
	$(MAKE) symlinks
	$(MAKE) examples > /dev/null
	$(MAKE) show-perf > /dev/null

.PHONY: examples
examples:
	./scripts/run-examples

.PHONY: show-perf
show-perf:
	./scripts/show-perf

.PHONY: clean
clean:
	git clean -dfX
	rm -rf bin tmp

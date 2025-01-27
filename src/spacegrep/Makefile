#
# Spacegrep's library and executables are built as part of semgrep-core.
# Use './bin/spacegrep' and './bin/spacecat' after running 'make'.
#
.PHONY: build
build:
	cd "$$(git rev-parse --show-toplevel)" && \
	  dune build _build/install/default/bin/spacegrep

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

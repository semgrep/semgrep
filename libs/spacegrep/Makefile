#
# Spacegrep's library and executables are built as part of semgrep-core.
# Use './bin/spacegrep' and './bin/spacecat' after running 'make'.
#
.PHONY: build
build:
	dune build .

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

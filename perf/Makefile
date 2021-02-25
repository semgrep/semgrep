#
# Run semgrep benchmarks.
# See './run-benchmarks --help' for options.
#
.PHONY: run
run:
	./run-benchmarks --docker returntocorp/semgrep:develop

.PHONY: dummy
dummy:
	./run-benchmarks --dummy --docker returntocorp/semgrep:develop --upload

.PHONY: clean
clean:
	git clean -dfX

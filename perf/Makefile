#
# Run semgrep benchmarks.
# See './run-benchmarks --help' for options.
#
.PHONY: run
run:
	docker pull returntocorp/semgrep:develop
	./run-benchmarks --docker returntocorp/semgrep:develop

.PHONY: dummy
dummy:
	docker pull returntocorp/semgrep:develop
	./run-benchmarks --dummy --docker returntocorp/semgrep:develop --upload

.PHONY: clean
clean:
	git clean -dfX

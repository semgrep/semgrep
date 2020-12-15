#
# Build and run the toy matcher, which is used as a model to study and improve
# the matching algorithms in semgrep-core.
#

# Build the test executable.
.PHONY: build
build:
	dune build

# Run unit tests with minimal output. Exclude slow tests.
.PHONY: test
test:
	$(MAKE) build
	../_build/install/default/bin/test-toy-matcher --quick-tests
	@echo "Run 'make bench' to include the slow tests as well."

# Run tests and show the logs, which include time and other runtime stats.
.PHONY: bench
bench:
	$(MAKE) build
	../_build/install/default/bin/test-toy-matcher -v

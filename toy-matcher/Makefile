#
# Build and run the toy matcher, which is used as a model to study and improve
# the matching algorithms in semgrep-core.
#

# Build the test executable.
.PHONY: build
build:
	dune build

TEST_EXE = ../_build/default/toy-matcher/Test.exe

# Run unit tests with minimal output. Exclude slow tests.
.PHONY: test
test:
	$(MAKE) build
	$(TEST_EXE) --quick-tests
	@echo "Run 'make bench' to include the slow tests as well."

# Run tests and show the logs, which include time and other runtime stats.
.PHONY: bench
bench:
	$(MAKE) build
	$(TEST_EXE) -v

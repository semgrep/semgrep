PYTEST ?= pipenv run pytest -v --tb=short --durations=10

.PHONY: default
default:
	$(MAKE) build
	$(MAKE) test

# Generate code. Needed at least when the interface changes.
.PHONY: build
build:
	$(MAKE) check

# Typecheck and run all tests from quickest to slowest.
.PHONY: test
test:
	$(MAKE) kinda-quick-tests
	$(PYTEST) -n auto -m 'not quick and not kinda_slow' tests

.PHONY: check
check:
	pre-commit run -a mypy
	$(MAKE) check-markers

# Check that all the test_* functions are categorized by duration.
.PHONY: check-markers
check-markers:
	./tests/check-markers tests

# Run only the tests marked as quick (@pytest.mark.quick)
# We have also 'kinda_slow' and 'slow'. See 'tox.ini', section 'pytest'.
#
# '-n auto' parallelizes with too much overhead for the quick tests.
#
.PHONY: quick-tests
quick-tests:
	$(MAKE) check
	$(PYTEST) -m quick tests

.PHONY: kinda-quick-tests
kinda-quick-tests:
	$(MAKE) quick-tests
	$(PYTEST) -n auto -m kinda_slow tests

.PHONY:
regenerate-tests:
	pipenv run pytest -n auto \
	  --snapshot-update --allow-snapshot-deletion \
	  tests

.PHONY: setup
setup:
	pipenv install --dev

.PHONY: clean
clean:
	rm -rf build/ semgrep.egg-info/ .eggs/
	rm -rf .pytest_cache/ .benchmarks/
	rm -f cli/src/semgrep/bin/semgrep-core

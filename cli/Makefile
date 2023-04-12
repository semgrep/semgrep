PYTEST ?= pipenv run pytest -v --tb=short --durations=10

# Run everything that makes sense to run.
.PHONY: default
default:
	$(MAKE) build
	$(MAKE) test
	$(MAKE) qa

# Generate code. Needed at least when the interface changes.
.PHONY: build
build:
	$(MAKE) check

# Typecheck and run all basic tests from quickest to slowest.
.PHONY: test
test:
	$(MAKE) kinda-quick-tests
	$(PYTEST) -n auto -m 'not quick and not kinda_slow' tests

# Run the end-to-end tests only. These are tests that invoke the semgrep
# command line, excluding unit tests. They don't include the quality assurance
# (QA) tests because they take longer and aren't as narrowly targeted.
#
# We exclude the few tests that don't invoke the semgrep command directly.
.PHONY: e2e
e2e:
	$(PYTEST) -n auto -v \
	  -m 'not no_semgrep_cli and not todo' tests/e2e

# Run the extra tests on public repos and semgrep-rules
.PHONY: qa
qa:
	$(PYTEST) -n auto -v tests/qa

# Run the end-to-end tests using osemgrep instead of the Python CLI.
# See tests/semgrep_runner.py for the use of the environment variables below
.PHONY: osemgrep-e2e
osemgrep-e2e:
	PYTEST_OSEMGREP=$(PWD)/src/semgrep/bin/osemgrep PYTEST_USE_OSEMGREP=true $(MAKE) e2e

# Run the quality assurance tests using osemgrep instead of the Python CLI.
.PHONY: osemgrep-qa
osemgrep-qa:
	PYTEST_USE_OSEMGREP=true $(MAKE) qa

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
	  --run-only-snapshots --snapshot-update --allow-snapshot-deletion

.PHONY: setup
setup:
	pipenv install --dev

.PHONY: clean
clean:
	rm -rf build/ semgrep.egg-info/ .eggs/
	rm -rf .pytest_cache/ .benchmarks/
	rm -f cli/src/semgrep/bin/semgrep-core

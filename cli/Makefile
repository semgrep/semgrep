###############################################################################
# Main target
###############################################################################

# Run everything that makes sense to run.
.PHONY: default
default: Pipfile.lock
	$(MAKE) build
	$(MAKE) test
	$(MAKE) qa

###############################################################################
# Build (and clean)
###############################################################################

# Generate code. Needed at least when the interface changes.
.PHONY: build
build:
	$(MAKE) check

.PHONY: check
check:
	$(MAKE) check-markers

# Check that all the test_* functions are categorized by duration.
.PHONY: check-markers
check-markers:
	./tests/check-markers tests

.PHONY: clean
clean:
	rm -rf build/ semgrep.egg-info/ .eggs/
	rm -rf .pytest_cache/ .benchmarks/
	rm -f cli/src/semgrep/bin/semgrep-core

Pipfile.lock: setup.py
	pipenv update

###############################################################################
# Tests
###############################################################################

# Note that '-n auto' parallelizes with too much overhead when running just
# the quick tests but for the sake of maintainability, we use the same options
# everywhere.
#
PYTEST ?= \
  pipenv run pytest \
    -n auto -vv --tb=short --durations=10 $(PYTEST_EXTRA_ARGS)

# This is run from .github/workflow/tests.jsonnet (which sets
# PYTEST_EXTRA_ARGS to some value for snapshot update).
# Note that there are no tests/e2e/ argument to pytest below so it will
# run all the tests specified in cli/pyproject.toml which are
# all tests/xxx except tests/qa/ (which is run separately in another job
# see tests.jsonnet).
#
# coupling: this must run the same tests as 'make test-dev'.
#
# !!! Make sure to pass a folder to pytest !!!
# due to the following unexpected behavior:
# $ pipenv run pytest --collect-only        # shows only 1567 tests
# $ pipenv run pytest --collect-only tests  # shows 1728 tests
# $ pipenv run pytest --collect-only .      # also shows 1728 tests
#
.PHONY: test
test:
	# To fail faster, consider running 'make test-dev' instead.
	$(MAKE) check
	$(PYTEST) tests

# Run the same tests as 'make test' but try to fail faster.
#
# Each test must be marked with one of 3 markers
# (@pytest.mark.quick, @pytest.mark.kinda_slow, or @pytest.mark.slow)
# See 'pyproject.toml', section 'markers'. This is checked by 'check-markers'.
# It allows us to run the quick tests first and fail faster which is
# desirable during development.
#
# From fastest to slowest (on Martin's machine):
# - 'make test-dev':
#     - quick: 30 s,
#     - kinda_slow: 10 min
#     - slow: ???
# Old from Pad's machine:
# - 'make e2e' (=~ 2m20s?)
# - 'make e2e-pro' (=~ 2m20s?)
# - 'make kinda-quick-tests' (=~ 3min40s )
# - 'make test' (=~ 11min)
#
# Note that none of the targets below are used in our CI test workflow.
#
# coupling: this must run the same tests as 'make test'.
#
# TODO: find a way to run these tests in a single pytest command and
# merge 'test-dev' with 'test'. For example, use pytest-order for this.
# See https://pytest-dev.github.io/pytest-order/stable/intro.html
#
.PHONY: test-dev
test-dev:
	# TODO: run mypy checks without relying on 'pre-commit'
	pre-commit run -a mypy
	$(MAKE) check
	# Run the quick tests first
	time $(PYTEST) -m quick tests
	# Now run the medium-quick tests
	time $(PYTEST) -m kinda_slow tests
	# Finally, run the slowest tests
	time $(PYTEST) -m 'not quick and not kinda_slow' tests

# Run the end-to-end tests only. These are tests that invoke the semgrep
# command line, excluding unit tests. They don't include the quality assurance
# (QA) tests because they take longer and aren't as narrowly targeted.
#
# We exclude the few tests that don't invoke the semgrep command directly.
.PHONY: e2e
e2e:
	$(PYTEST) tests/e2e

.PHONY: e2e-pro
e2e-pro:
	$(PYTEST) -m 'not no_semgrep_cli and not todo' tests/e2e-pro

# Run the extra tests on public repos and semgrep-rules
.PHONY: qa
qa:
	$(PYTEST) tests/qa

###############################################################################
# Osemgrep tests
###############################################################################

# Run the end-to-end tests using osemgrep instead of pysemgrep.
# See tests/semgrep_runner.py for the use of the environment variables below.
# To run individual tests, export PYTEST_USE_OSEMGREP=true
# and run 'pytest tests/e2e/test_output.py' in a shell for example.
#TODO: right now we just run the e2e-only because it's our main goal for Q4
.PHONY: osemgrep-e2e
osemgrep-e2e:
	PYTEST_USE_OSEMGREP=true $(MAKE) e2e

# Run all the known passing-with-osemgrep tests (not tagged with osemfail)
# coupling: this is run in CI in .github/workflows/tests.yml
osempass:
	PYTEST_USE_OSEMGREP=true $(PYTEST) -n auto \
	  -m 'not osemfail' tests/e2e

# to be used to discover tests that we should remove the osemfail pytest mark
osempass-check-if-new-osempass:
	PYTEST_USE_OSEMGREP=true $(PYTEST) -n auto -v --no-summary \
	  -m 'not no_semgrep_cli and osemfail' tests/e2e \
	 | (grep PASSED || echo 'Found no new passing tests')

# Run the quality assurance tests using osemgrep instead of the Python CLI.
.PHONY: osemgrep-qa
osemgrep-qa:
	PYTEST_USE_OSEMGREP=true $(MAKE) qa

###############################################################################
# Dev targets
###############################################################################

.PHONY:
regenerate-tests:
	pipenv run pytest -n auto \
	  --run-only-snapshots --snapshot-update --allow-snapshot-deletion

.PHONY: setup
setup:
	pipenv install --dev

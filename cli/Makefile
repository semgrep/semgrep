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
	# TODO: also run 'typecheck' after making it work in CI (without git
	# or pre-commit)
	# $(MAKE) typecheck
	$(MAKE) check-markers

# TODO: is there a way to run the same commands as the pre-commit hook
# without code duplication and without depending on pre-commit?
# Various issues with external libraries arise when trying to
# run 'pipenv run mypy' here directly.
.PHONY: typecheck
typecheck:
	pre-commit run mypy -a

# Check that all the test_* functions are categorized by duration.
.PHONY: check-markers
check-markers:
	./tests/check-markers tests

.PHONY: clean
clean:
	rm -rf build/ semgrep.egg-info/ .eggs/
	rm -rf .pytest_cache/ .benchmarks/
# clean also the semgrep-pro artifacts, not just semgrep-core
	rm -f src/semgrep/bin/semgrep-core*
	rm -f src/semgrep/bin/pro-installed-by.txt

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
#
# Note that pytest without an argument will run the tests configured in
# 'pyproject.toml'. This should be equivalent to running on 'tests/default'
# if the principle of least surprise is respected.
#
# coupling: this must run the same tests as 'make test-dev'.
#
.PHONY: ci-test
ci-test:
	# To fail faster, consider running 'make test-dev' instead.
	$(MAKE) check
	$(PYTEST) -m 'not pysemfail' tests/default

# While 'make typecheck' doesn't work in CI, use 'make ci-test'
.PHONY: test
test:
	# If 'make typecheck' fails, consider calling 'make ci-test' instead
	# of 'make test'.
	$(MAKE) typecheck
	$(MAKE) ci-test

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
# - 'make e2e-other' (=~ 2m20s?)
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
	time $(PYTEST) -m quick tests/default
	# Now run the medium-quick tests
	time $(PYTEST) -m kinda_slow tests/default
	# Finally, run the slowest tests
	time $(PYTEST) -m 'not quick and not kinda_slow' tests/default

# Run the end-to-end tests only. These are tests that invoke the semgrep
# command line, excluding unit tests. They don't include the quality assurance
# (QA) tests because they take longer and aren't as narrowly targeted.
#
# We exclude the few tests that don't invoke the semgrep command directly.
.PHONY: e2e
e2e:
	$(PYTEST) -m 'not pysemfail' tests/default/e2e

# Run the end-to-end tests using osemgrep instead of pysemgrep.
# See tests/semgrep_runner.py for the use of the environment variables below.
# To run individual tests, export PYTEST_USE_OSEMGREP=true
# and run 'pytest tests/default/e2e/test_output.py' in a shell for example.
#TODO: right now we just run the e2e-only because it's our main goal for Q4
#
# coupling: must match the 'e2e' target above except for the pysemfail/osemfail
# markers
.PHONY: osemgrep-e2e
osemgrep-e2e:
	PYTEST_USE_OSEMGREP=true $(PYTEST) -m 'not osemfail' tests/default/e2e

.PHONY: e2e-other
e2e-other:
	$(PYTEST) -m 'not no_semgrep_cli and not todo' tests/default/e2e-other

# For tests that require login and the pro binary.
#
.PHONY: e2e-pro-need-login
e2e-pro-need-login:
	$(PYTEST) tests/e2e-pro-need-login

# Run the extra tests on public repos and semgrep-rules
.PHONY: qa
qa:
	$(PYTEST) tests/qa

###############################################################################
# Osemgrep tests
###############################################################################

# Run all the known passing-with-osemgrep tests (not tagged with osemfail)
# coupling: this is run in CI in .github/workflows/tests.yml
osempass:
	PYTEST_USE_OSEMGREP=true $(PYTEST) -n auto \
	  -m 'not osemfail' tests/default/e2e tests/default/e2e-other

# to be used to discover tests that we should remove the osemfail pytest mark
osempass-check-if-new-osempass:
	PYTEST_USE_OSEMGREP=true $(PYTEST) -n auto -v --no-summary \
	  -m 'not no_semgrep_cli and osemfail' tests/default/e2e \
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
	  --run-only-snapshots --snapshot-update --allow-snapshot-deletion \
	  -m 'not pysemfail' tests/default/e2e tests/default/e2e-other
	PYTEST_USE_OSEMGREP=true pipenv run pytest -n auto \
	  --run-only-snapshots --snapshot-update --allow-snapshot-deletion \
	  -m 'pysemfail' tests/default/e2e tests/default/e2e-other

.PHONY: setup
setup:
	pipenv install --dev

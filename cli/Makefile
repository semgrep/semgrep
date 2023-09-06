###############################################################################
# Main target
###############################################################################

# Run everything that makes sense to run.
.PHONY: default
default:
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
	pre-commit run -a mypy
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

###############################################################################
# Tests
###############################################################################

# From fastest to slowest (on pad's machine):
# - 'make quick-tests' (=~ 30s)
# - 'make e2e' (=~ 2m20s)
# - 'make kinda-quick-tests' (=~ 3min40s )
# - 'make test' (=~ 11min)

PYTEST ?= pipenv run pytest -v --tb=short --durations=10

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
	$(PYTEST) -n auto -v -m 'not no_semgrep_cli and not todo' tests/e2e

# Run the extra tests on public repos and semgrep-rules
.PHONY: qa
qa:
	$(PYTEST) -n auto -v tests/qa

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

###############################################################################
# Osemgrep tests
###############################################################################

# Run the end-to-end tests using osemgrep instead of pysemgrep.
# See tests/semgrep_runner.py for the use of the environment variables below.
# To run individual tests, export PYTEST_USE_OSEMGREP=true
# and run 'pytest tests/e2e/test_output.py' in a shell for example.
.PHONY: osemgrep-e2e
osemgrep-e2e:
	PYTEST_USE_OSEMGREP=true $(MAKE) e2e

# Run all the known passing-with-osemgrep tests (tagged with osempass)
# coupling: this is run in CI in .github/workflows/tests.yml
osempass:
	PYTEST_USE_OSEMGREP=true  $(PYTEST) -n auto -m 'osempass and not todo' tests/e2e

# to be used to discover tests that we should add the osempass pytest mark
osempass-check-if-new-osempass:
	PYTEST_USE_OSEMGREP=true  $(PYTEST) -n auto -v --no-summary -m 'not no_semgrep_cli and not todo and not osempass' tests/e2e

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

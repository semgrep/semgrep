PYTEST ?= pipenv run pytest -v --tb=short --durations=10

# Typecheck and run all tests from quickest to slowest.
.PHONY: test
test:
	$(MAKE) kinda-quick-tests
	$(PYTEST) -n auto -m 'not quick and not kinda_slow' tests

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
	  --snapshot-update --ignore=tests/qa/test_public_repos.py \
	  tests

.PHONY:
setup:
	pipenv install --dev

.PHONY: clean
clean:
	rm -rf build/ semgrep.egg-info/ .eggs/
	rm -rf .pytest_cache/ .benchmarks/
	rm -f semgrep/bin/semgrep-core
	rm -rf semgrep/__pycache__ semgrep/*/__pycache__

.PHONY: check
check:
	pre-commit run -a mypy
	#semgrep -pycheck .

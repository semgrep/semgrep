# Typecheck + run all the tests in arbitrary order.
# See also 'all-tests'
.PHONY: test
test:
	$(MAKE) check
	pipenv run pytest -v --tb=short --durations=10 tests

# Run only the tests marked as quick (@pytest.mark.quick)
# We have also 'kinda_slow' and 'slow'. See 'tox.ini', section 'pytest'.
.PHONY: quick-tests
quick-tests:
	$(MAKE) check
	pipenv run pytest -v --tb=short --durations=10 -m quick tests

.PHONY: kinda-quick-tests
kinda-quick-tests:
	$(MAKE) quick-tests
	pipenv run pytest -v --tb=short --durations=10 -m kinda_slow tests

# Run all tests from quickest to slowest.
# Warning: this won't run the tests that are not marked with @pytest.mark.XXX
.PHONY: all-tests
all-tests:
	$(MAKE) kinda-quick-tests
	pipenv run pytest -v --tb=short --durations=10 -m slow tests

.PHONY:
regenerate-tests:
	pipenv run pytest tests/ --snapshot-update --ignore=tests/qa/test_public_repos.py

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

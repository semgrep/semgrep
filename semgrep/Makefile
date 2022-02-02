test:
	pipenv run pytest -v --tb=short tests/
regenerate-tests:
	pipenv run pytest tests/ --snapshot-update --ignore=tests/qa/test_public_repos.py

setup:
	pipenv install --dev

.PHONY: clean
clean:
	rm -rf build/ semgrep.egg-info/ .eggs/
	rm -rf .pytest_cache/ .benchmarks/
	rm -f semgrep/bin/semgrep-core
	rm -rf semgrep/__pycache__ semgrep/*/__pycache__

.PHONY: check
# this needs to run in pipenv shell to access mypy
check:
	#mypy --config-file ../mypy.ini semgrep
	semgrep-core -pycheck .

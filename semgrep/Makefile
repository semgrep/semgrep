test:
	pipenv run pytest -v --tb=short tests/
regenerate-tests:
	pipenv run pytest tests/ --snapshot-update

.PHONY: qa-test
qa-test:
	pipenv run pytest -v --tb=short tests/ --qa

.PHONY: ci-test
# make sure to run the tests as bash.
ci-test: SHELL:=/bin/bash
ci-test:
ifeq ($(GITHUB_EVENT_NAME),pull_request)
	# check if the branch name is of the form release-X.X.X
	if [[ "$(GITHUB_HEAD_REF)" =~ release-[0-9]+\.[0-9]+\.[0-9]+ ]]; \
	then \
		pipenv run pytest -n auto -v --tb=short tests/ --qa; \
	else \
		pipenv run pytest -n auto -v --tb=short tests/; \
	fi
else ifeq ($(GITHUB_EVENT_NAME),push)
	pipenv run pytest -n auto -v --tb=short tests/ --qa
else
	pipenv run pytest -n auto -v --tb=short tests/
endif

all:
	./build.sh ./build/
clean:
	rm -rf ./build/
test:
	pipenv run pytest -v --tb=short tests/
regenerate-tests:
	pipenv run pytest tests/ --snapshot-update
install:
	cp -rv ./build/semgrep.dist/ /usr/local/bin/semgrep-files/
	ln -sf /usr/local/bin/semgrep-files/semgrep /usr/local/bin/semgrep

.PHONY: qa-test
qa-test:
	pipenv run pytest -v --tb=short tests/ --qa

.PHONY: ci-test
ci-test: SHELL:=/bin/bash
ci-test:
ifeq ($(GITHUB_EVENT_NAME),pull_request)
	# check if the branch name is of the form release-X.X.X
	if [[ "$(GITHUB_HEAD_REF)" =~ test-release-[0-9]+\.[0-9]+\.[0-9]+ ]]; \
	then \
		echo "qa"; pipenv run pytest -n auto -v --tb=short tests/ --qa;\
	else \
		echo "short"; pipenv run pytest -n auto -v --tb=short tests/;\
	fi
else ifeq ($(GITHUB_EVENT_NAME),push)
	pipenv run pytest -n auto -v --tb=short tests/ --qa
else
	if [[ "$(GITHUB_HEAD_REF)" =~ test-release-[0-9]+\.[0-9]+\.[0-9]+ ]]; \
	then \
		echo "qa"; pipenv run pytest -n auto -v --tb=short tests/ --qa;\
	else \
		echo "short"; pipenv run pytest -n auto -v --tb=short tests/;\
	fi
	pipenv run pytest -n auto -v --tb=short tests/
endif

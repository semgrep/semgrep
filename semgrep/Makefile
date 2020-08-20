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
ci-test:
ifeq ($(GITHUB_EVENT_NAME),pull)
ifeq ($(BRANCH_NAME), test-release-%.%.%)
	pipenv run pytest -n auto -v --tb=short tests/ --qa
endif
else ifeq ($(GITHUB_EVENT_NAME),push)
	pipenv run pytest -n auto -v --tb=short tests/ --qa
else
	pipenv run pytest -n auto -v --tb=short tests/
endif

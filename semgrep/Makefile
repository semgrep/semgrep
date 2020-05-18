all:
	./build.sh ./build/
clean:
	rm -rf ./build/
test:
	pipenv run pytest tests/
regenerate-tests:
	pipenv run pytest tests/ --snapshot-update
install:
	cp -rv ./build/semgrep.dist/ /usr/local/bin/semgrep-files/
	ln -sf /usr/local/bin/semgrep-files/semgrep /usr/local/bin/semgrep

.PHONY: qa-test
qa-test:
	pipenv run pytest tests/ --qa

.PHONY: benchmark
benchmark:
	pipenv run pytest tests/ --benchmark-only $(ARGS)

.PHONY: benchmark-snapshot
benchmark-snapshot:
	pipenv run pytest tests/ --benchmark-only --benchmark-autosave $(ARGS)

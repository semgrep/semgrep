all:
	./build.sh ./build/
clean:
	rm -rf ./build/
test:
	PYTHONPATH=. pytest tests/
install:
	cp -rv ./build/semgrep.dist/ /usr/local/bin/semgrep-files/
	ln -sf /usr/local/bin/semgrep-files/semgrep /usr/local/bin/semgrep

.PHONY: qa-test
qa-test:
	pytest -s tests/acceptance/qa.py

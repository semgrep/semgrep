all:
	./build.sh ./build/
clean:
	rm -rf ./build/
test:
	PYTHONPATH=. ./tests/run-lint-tests.sh
install:
	cp -rv ./build/sgrep.dist/ /usr/local/bin/sgrep-lint-files/
	ln -sf /usr/local/bin/sgrep-lint-files/sgrep-lint /usr/local/bin/sgrep-lint

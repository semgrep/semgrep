all:
	./sgrep_lint/build.sh ./build/
clean:
	rm -rf ./sgrep_lint/build/
test:
	./sgrep_lint/tests/run-lint-tests.sh
install:
	cp -rv ./sgrep_lint/build/sgrep.dist/ /usr/local/bin/sgrep-lint-files/
	ln -sf /usr/local/bin/sgrep-lint-files/sgrep-lint /usr/local/bin/sgrep-lint

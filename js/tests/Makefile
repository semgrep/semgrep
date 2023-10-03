default: test

include ../engine/Makefile.include

clean:
	rm -rf dist

languages:
	SEMGREP_JS_TEST_BUILD=1 $(MAKE) -C .. languages

test: dist/semgrep-engine.js languages
	node index.test.js

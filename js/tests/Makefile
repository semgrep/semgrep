default: test

include ../engine/Makefile.include

clean:
	rm -rf dist

languages:
	$(MAKE) -C .. languages

test: dist/semgrep-engine.js languages
	node --stack-size=100000 index.test.js

test-debugger:
	node --inspect-brk --stack-size=100000 index.test.js

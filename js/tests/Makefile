default: test

include ../engine/Makefile.include

clean:
	rm -rf dist

languages:
	$(MAKE) -C .. languages

test: dist/semgrep-engine.js languages
	git rev-parse --show-toplevel || echo "uh oh, we're not in a git project"
	node --stack-size=100000 index.test.js

test-debugger:
	node --inspect-brk --stack-size=100000 index.test.js

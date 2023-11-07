.PHONY: default
default: build

include ../engine/Makefile.include

.PHONY: build
build: out/src/server.js

.PHONY: test
test: build
	npm test

.PHONY: server
server: build
	npm run server

clean:
	rm -rf dist

package-lock.json:
	npm install

node_modules/.package-lock.json: package-lock.json
	npm ci

.PHONY: dist/Main.bc.js
dist/Main.bc.js: ../../_build/default/js/language_server/Main.bc.js
	mkdir -p dist
	cp -rf $< $@

out/src/server.js: node_modules/.package-lock.json dist/semgrep-engine.js dist/Main.bc.js
	mkdir -p dist
	npm run build

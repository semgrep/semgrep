

ESBUILD_DEFAULTS = \
	--bundle \
	--minify \
	--platform=node \
	--log-override:duplicate-case=silent

.PHONY: default
default: build

include Makefile.include

.PHONY: build
build: dist/index.cjs dist/index.mjs dist/index.d.ts

.PHONY: test
test: build
	npm test

.PHONY: package
package: build
	npm pack

.PHONY: clean
clean:
	rm -rf dist

.PHONY: distclean
distclean:
	rm -rf dist node_modules

package-lock.json:
	npm install

node_modules/.package-lock.json: package-lock.json
	npm ci

dist/index.cjs: src/index.js node_modules/.package-lock.json dist/semgrep-engine.js ../../_build/default/js/engine/Main.bc.js
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --format=cjs --outfile=$@

dist/index.mjs: src/index.js node_modules/.package-lock.json dist/semgrep-engine.js ../../_build/default/js/engine/Main.bc.js
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --format=esm --outfile=$@

dist/index.d.ts: src/index.d.ts
	mkdir -p dist
	cp $^ $@

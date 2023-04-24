ESBUILD_DEFAULTS = \
	--bundle \
	--minify \
	--platform=node \
	--log-override:duplicate-case=silent

.PHONY: default
default: build

.PHONY: build
build: dist/index.js dist/index.d.ts dist/libyaml.wasm

.PHONY: test
test: build
	npm test

.PHONY: package
package: build
	npm pack

.PHONY: clean
clean:
	rm -rf dist

../libyaml/dist/index.js ../libyaml/dist/libyaml.wasm:
	cd ../libyaml; $(MAKE) build

package-lock.json:
	npm install

node_modules/.package-lock.json: package-lock.json
	npm ci

dist/index.js: src/index.ts node_modules/.package-lock.json ../libyaml/dist/index.js
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --format=cjs --outfile=$@

dist/libyaml.wasm: ../libyaml/dist/libyaml.wasm
	mkdir -p dist
	cp $^ $@

dist/index.d.ts: node_modules/.package-lock.json src/index.ts
	mkdir -p dist
	npx -p typescript tsc

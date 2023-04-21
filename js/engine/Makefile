ESBUILD_DEFAULTS = \
	--bundle \
	--minify \
	--platform=node \
	--log-override:duplicate-case=silent

.PHONY: default
default: build

.PHONY: build
build: dist/index.js dist/index.d.ts

.PHONY: test
test: build
	npx jest

.PHONY: package
package: build
	npm pack

.PHONY: clean
clean:
	rm -rf dist

../libyaml/dist/libyaml.js:
	cd ../libyaml; $(MAKE) build

node_modules/.package-lock.json:
	npm ci

dist/index.js: src/index.ts ../libyaml/dist/libyaml.js node_modules/.package-lock.json
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --outfile=$@

dist/index.d.ts: node_modules/.package-lock.json src/index.ts
	mkdir -p dist
	npx -p typescript tsc

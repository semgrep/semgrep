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

.PHONY: build-libyaml
build-libyaml:
	cd ../libyaml; $(MAKE) build

node_modules/.package-lock.json:
	npm ci

dist/index.js: src/index.ts node_modules/.package-lock.json build-libyaml
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --outfile=$@

dist/index.d.ts: node_modules/.package-lock.json dist/index.js build-libyaml
	mkdir -p dist
	npx -p typescript tsc

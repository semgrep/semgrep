ESBUILD_DEFAULTS = \
	--bundle \
	--minify \
	--platform=node \
	--log-override:duplicate-case=silent

.PHONY: default
default: build

.PHONY: test
test:
	npm run test

.PHONY: build
build:
	npm ci
	npm run build

.PHONY: package
package:
	npm ci
	npm pack

.PHONY: clean
clean:
	rm -rf dist

../libyaml/dist/libyaml.js:
	make -C ../libyaml dist/libyaml.js

dist/index.js: src/index.ts ../libyaml/dist/libyaml.js
	mkdir -p dist
	esbuild $< $(ESBUILD_DEFAULTS) --outfile=$@

dist/index.d.ts: src/index.ts
	mkdir -p dist
	tsc

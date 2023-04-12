ESBUILD_DEFAULTS = \
	--bundle \
	--minify \
	--platform=neutral \
	--log-override:duplicate-case=silent \
	--external:path \
	--external:fs \
	--external:constants \
	--external:tty \
	--external:child_process

.PHONY: default
default:
	npm ci
	npm run build

.PHONY: test
test:
	npm run test

.PHONY: build
build: dist/index.cjs dist/index.mjs

.PHONY: clean
clean:
	rm -rf dist

../libyaml/dist/libyaml.js:
	make -C ../libyaml dist/libyaml.js

dist/index.cjs: entrypoint.js ../libyaml/dist/libyaml.js
	mkdir -p dist
	esbuild $< $(ESBUILD_DEFAULTS) --format=cjs --outfile=$@

dist/index.mjs: entrypoint.js ../libyaml/dist/libyaml.js
	mkdir -p dist
	esbuild $< $(ESBUILD_DEFAULTS) --format=esm --outfile=$@

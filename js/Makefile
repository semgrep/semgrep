pcre_version = 8.45
pcre_folder = downloads/pcre-$(pcre_version)

tree_sitter_langs = $(shell grep -lr 'public_name tree-sitter-lang.' ../languages | egrep -o 'semgrep-[^/]+')

.PHONY: default
default: build

.PHONY: clean
clean:
	rm -rf downloads build

$(pcre_folder):
	mkdir -p downloads
	curl -L "https://sourceforge.net/projects/pcre/files/pcre/$(pcre_version)/pcre-$(pcre_version).tar.gz/download" | tar xz - --directory downloads

build/libpcre.js: $(pcre_folder)
	mkdir -p build
	$(runner) scripts/build-libpcre.sh $(pcre_folder) build/libpcre.js

build/semgrep-%.js:
	mkdir -p build
	$(runner) scripts/build-semgrep-language.sh ../languages $* build/semgrep-$*.js

.PHONY: build-languages
build-languages: $(patsubst %,build/%.js,$(tree_sitter_langs))

.PHONY: build
build: build/libpcre.js build-languages

.PHONY: test
test: build/libpcre.js build/semgrep-bash.js
	cd tests; npm test


SUPPORTED_LANGS := bash c cpp csharp dockerfile elixir go html java json jsonnet kotlin lisp lua ocaml php python r ruby rust scala solidity swift terraform typescript

.PHONY: default
default: $(SUPPORTED_LANGS:%=%/dist/index.js)

.PHONY: test
test: $(SUPPORTED_LANGS:%=test-%)

.PHONY: test-%
test-%:
	make -C $* test

.PHONY: clean
clean:
	rm -rf **/dist

%/dist/index.js:
	cd $*; npm ci; npm run build

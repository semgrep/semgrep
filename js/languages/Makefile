
SUPPORTED_LANGS := bash c cairo cpp csharp dart dockerfile elixir go hack html java json jsonnet julia kotlin lisp lua ocaml php promql protobuf python r ruby rust scala solidity swift terraform typescript vue

.PHONY: default
default: build

.PHONY: build
build: $(SUPPORTED_LANGS:%=build-%)

.PHONY: test
#test: SEMGREP_JS_TEST_BUILD=1 # Enable this once https://github.com/llvm/llvm-project/pull/67715 is released (or if you've built clang yourself)
test: $(SUPPORTED_LANGS:%=test-%)

.PHONY: package
package: $(SUPPORTED_LANGS:%=package-%)

.PHONY: build-%
build-%:
	. ../../libs/ocaml-tree-sitter-core/tree-sitter-config.sh
	cd $*; $(MAKE) build

.PHONY: test-%
test-%:
	cd $*; $(MAKE) test

.PHONY: package-%
package-%:
	cd $*; $(MAKE) package

.PHONY: clean
clean:
	rm -rf **/dist

.PHONY: distclean
distclean:
	rm -rf **/dist **/node_modules

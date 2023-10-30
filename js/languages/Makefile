
SUPPORTED_LANGS := bash c cairo cpp csharp dart dockerfile elixir go hack html java json jsonnet kotlin lisp lua ocaml php promql protobuf python r ruby rust scala solidity swift terraform typescript vue

.PHONY: default
default: build

.PHONY: build
build: $(SUPPORTED_LANGS:%=build-%)

.PHONY: test
test: $(SUPPORTED_LANGS:%=test-%)

.PHONY: package
package: $(SUPPORTED_LANGS:%=package-%)

.PHONY: build-%
build-%:
	cd $*; $(MAKE) build

.PHONY: test-%
test-%:
	cd $*; SEMGREP_JS_TEST_BUILD=1 $(MAKE) test

.PHONY: package-%
package-%:
	cd $*; $(MAKE) package

.PHONY: clean
clean:
	rm -rf **/dist

.PHONY: distclean
distclean:
	rm -rf **/dist **/node_modules

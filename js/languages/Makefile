
SUPPORTED_LANGS := bash c cpp csharp dart dockerfile elixir go html java json jsonnet julia kotlin lisp lua ocaml php promql protobuf python r ruby rust scala solidity swift terraform typescript

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

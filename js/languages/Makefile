#coupling: if you add a language below, you'll need also to update:
# - ../tests/index.test.js
# - ../language_server/dune
# - ../language_server/Makefile
#Missing from the list (in OSS/languages/ but not below):
# - javascript: because?
# - yaml: because?
# - regexp: because?
# C is missing from this list now, because we use the cpp parser for it. We only
# need to build the actual parser once, and then reference it from C
SUPPORTED_LANGS := bash cairo circom cpp csharp dart dockerfile go hack html java json jsonnet julia kotlin lisp lua move_on_aptos ocaml php promql protobuf python ql r ruby rust scala solidity swift terraform typescript vue

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

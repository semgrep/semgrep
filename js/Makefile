.PHONY: default
default: pro-setup libyaml engine languages

.PHONY: build
build:
	cd libpcre; $(MAKE) build
	cd libyaml; $(MAKE) build
	cd engine; $(MAKE) build
	cd languages; $(MAKE) build

.PHONY: test
test:
	cd libpcre; $(MAKE) test
	cd libyaml; $(MAKE) test
	cd engine; $(MAKE) test
	cd languages; $(MAKE) test

.PHONY: e2e
e2e: engine languages
	cd e2e; $(MAKE) test

.PHONY: package
package:
	cd engine; $(MAKE) package
	cd languages; $(MAKE) package

.PHONY: libpcre
libpcre:
	cd libpcre; $(MAKE)

.PHONY: libyaml
libyaml:
	cd libyaml; $(MAKE)

.PHONY: languages
languages:
	cd languages; $(MAKE)

.PHONY: engine
engine:
	cd engine; $(MAKE)

.PHONY: pro-setup
pro-setup:
ifdef SEMGREP_PRO
	ln -s -f ../_build ../_build
	ln -s -f ../semgrep/js/languages ../_build/default/js/languages
endif


.PHONY: clean
clean:
	cd libpcre; $(MAKE) clean
	cd libyaml; $(MAKE) clean
	cd engine; $(MAKE) clean
	cd languages; $(MAKE) clean
ifdef SEMGREP_PRO
	rm -f ../_build/default/js/languages
	rm -f ../_build
endif

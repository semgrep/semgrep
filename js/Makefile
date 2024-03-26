.PHONY: default
default: libyaml engine languages

.PHONY: build
build:
	cd libpcre; $(MAKE) build
	cd libpcre2; $(MAKE) build
	cd libyaml; $(MAKE) build
	cd engine; $(MAKE) build
	cd languages; $(MAKE) build

.PHONY: test
test:
	cd libpcre; $(MAKE) test
	cd libpcre2; $(MAKE) test
	cd libyaml; $(MAKE) test
	cd engine; $(MAKE) test
	cd languages; $(MAKE) test
	cd tests; $(MAKE)

.PHONY: package
package:
	cd engine; $(MAKE) package
	cd languages; $(MAKE) package

.PHONY: libpcre
libpcre:
	cd libpcre; $(MAKE)

.PHONY: libpcre2
libpcre2:
	cd libpcre2; $(MAKE)

.PHONY: libyaml
libyaml:
	cd libyaml; $(MAKE)

.PHONY: languages
languages:
	cd languages; $(MAKE)

.PHONY: engine
engine:
	cd engine; $(MAKE)

.PHONY: clean
clean:
	cd libpcre; $(MAKE) clean
	cd libpcre2; $(MAKE) clean
	cd libyaml; $(MAKE) clean
	cd engine; $(MAKE) clean
	cd languages; $(MAKE) clean
	cd tests; $(MAKE) clean

.PHONY: distclean
distclean:
	cd libpcre; $(MAKE) distclean
	cd libpcre2; $(MAKE) distclean
	cd libyaml; $(MAKE) distclean
	cd engine; $(MAKE) distclean
	cd languages; $(MAKE) distclean

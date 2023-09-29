.PHONY: default
default: libyaml engine languages

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
	cd languages; $(MAKE) -j $(nproc) test # These are easily parallelizable
	#cd tests; $(MAKE)

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

.PHONY: clean
clean:
	cd libpcre; $(MAKE) clean
	cd libyaml; $(MAKE) clean
	cd engine; $(MAKE) clean
	cd languages; $(MAKE) clean
	cd tests; $(MAKE) clean

.PHONY: distclean
distclean:
	cd libpcre; $(MAKE) distclean
	cd libyaml; $(MAKE) distclean
	cd engine; $(MAKE) distclean
	cd languages; $(MAKE) distclean

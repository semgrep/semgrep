.PHONY: default
default: libyaml engine languages

.PHONY: test
test:
	cd libyaml; $(MAKE) test
	cd engine; $(MAKE) test
	cd languages; $(MAKE) test

.PHONY: e2e
e2e: engine languages
	cd e2e; $(MAKE) test

.PHONY: package
package:
	cd engine; $(MAKE) -C engine package
	cd languages; $(MAKE) -C languages package

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
	cd libyaml; $(MAKE) clean
	cd engine; $(MAKE) clean
	cd languages; $(MAKE) clean

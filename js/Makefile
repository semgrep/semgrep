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

# This was the simplest way to support pro. By symlinking the _build directory
# of the pro repo into the semgrep repo, all of the language/engine recipes stay
# the same as the dir structures are the same. The only difference is that you
# set SEMGREP_PRO=1 in your environment before running make. For example,
# the engine looks for _build/default/js/engine/Main.bc, so if we build pro in
# js/engine/Main.ml, then this recipe still works, but it's now the pro engine.
#
# Alternatively this could be a makefile in the pro repo, but all that would
# do is shell out to this one, minus this step, so I figured I'd just keep this
# here.
.PHONY: pro-setup
pro-setup:
ifdef SEMGREP_PRO
	ln -s -f ../_build ../_build
    # We need to link languages here for the same reason we link _build
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

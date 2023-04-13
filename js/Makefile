.PHONY: default
default: libyaml engine languages

.PHONY: test
test:
	make -C libyaml test
	make -C engine test
	make -C languages test

.PHONY: libyaml
libyaml:
	make -C libyaml

.PHONY: languages
languages:
	make -C languages

.PHONY: engine
engine:
	make -C engine

.PHONY: clean
clean:
	make -C libyaml clean
	make -C engine clean
	make -C languages clean

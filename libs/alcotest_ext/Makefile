#
# The usual Make targets
#

.PHONY: build
build:
	dune build .

.PHONY: test
test: build
	./meta-test

# The test program offers various options. This is one of them.
.PHONY: retest
retest: build
	./meta-test --lazy

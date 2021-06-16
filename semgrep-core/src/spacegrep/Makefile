.PHONY: examples
examples:
	./scripts/run-examples

.PHONY: test
test:
	dune exec src/test/test.exe
	$(MAKE) examples > /dev/null

.PHONY: clean
clean:
	git clean -dfX
	rm -rf bin

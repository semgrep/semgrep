.PHONY: test
test: run-all run-lang
	./run-all

run-all:
	ln -sf ../run-all .
run-lang:
	ln -sf ../run-lang .

.PHONY: clean
clean:
	rm -f run-all run-lang

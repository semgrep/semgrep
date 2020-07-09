.PHONY: default
default:
	@echo "This does nothing."

.PHONY: clean
clean:
	make -C pfff clean
	make -C ocaml-tree-sitter clean
	make -C semgrep-core clean
	make -C semgrep clean

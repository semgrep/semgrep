SEMGREP_CORE ?= semgrep-core

.PHONY: all
all:
	./run-all --semgrep-core "$(SEMGREP_CORE)"

.PHONY: test
test:
	$(MAKE) -C test-parsing
	# Use a fake language named 'test' to avoid polluting production data
	# series for real languages.
	./upload-parsing-rates test-upload/lang/test/stats.json

.PHONY: clean
clean:
	rm -rf lang/*/tmp test/lang/*/tmp
	$(MAKE) -C test-parsing clean

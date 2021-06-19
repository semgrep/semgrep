.PHONY: all
all:
	./run-all

.PHONY: test
test:
	cd test && ./run-all
	# Use a fake language named 'test' to avoid polluting production data
	# series for real languages.
	./upload-parsing-rates test-upload/lang/test/stats.json

.PHONY: clean
clean:
	rm -rf lang/*/tmp test/lang/*/tmp

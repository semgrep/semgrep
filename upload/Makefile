.PHONY: setup
setup:
	# Cannot be a symlink since docker does not support symlinks that are not
	# in current context when building
	cp ../semgrep/semgrep/rule_schema.yaml .

.PHONY: build
build:
	$(MAKE) setup
	docker build -t returntocorp/semgrep-upload .

.PHONY: deploy
deploy:
	$(MAKE) build
	docker push returntocorp/semgrep-upload

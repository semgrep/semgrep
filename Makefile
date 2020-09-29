#
# This makefile is targeted at developers.
# For a one-shot production build, look into Dockerfile.
#

# Routine build. It assumes all dependencies and configuration are already
# in place and correct. It should be fast since it's called often during
# development.
#
.PHONY: build
build:
	$(MAKE) build-core
	cd semgrep && pipenv install --dev

.PHONY: install
install:
	$(MAKE) -C semgrep-core install
	python3.7 -m pip install semgrep

.PHONY: build-core
build-core: build-ocaml-tree-sitter
	$(MAKE) -C semgrep-core

.PHONY: build-ocaml-tree-sitter
build-ocaml-tree-sitter:
	$(MAKE) -C ocaml-tree-sitter
	$(MAKE) -C ocaml-tree-sitter install

# Update and rebuild everything within the project.
#
# At the moment, this is useful when ocaml-tree-sitter get updated,
# since semgrep-core is not rebuilt automatically when they change.
#
.PHONY: rebuild
rebuild:
	git submodule update --init --recursive
	-$(MAKE) clean
	$(MAKE) config
	$(MAKE) build

# This is a best effort to install some external dependencies.
# Should run infrequently.
#
.PHONY: setup
setup:
	git submodule update --init --recursive
	opam update -y
	opam install -y --deps-only ./semgrep-core/pfff
	cd ocaml-tree-sitter && ./scripts/install-tree-sitter-lib
	opam install -y --deps-only ./ocaml-tree-sitter
	opam install -y --deps-only ./semgrep-core

# This needs to run initially or when something changed in the external
# build environment. This typically looks for the location of libraries
# and header files outside of the project.
#
.PHONY: config
config:
	cd ocaml-tree-sitter && ./configure

# Remove from the project tree everything that's not under source control
# and was not created by 'make setup'.
#
.PHONY: clean
clean:
	-$(MAKE) -C ocaml-tree-sitter clean
	-$(MAKE) -C semgrep-core clean
	-$(MAKE) -C semgrep clean

# Same as 'make clean' but may remove additional files, such as external
# libraries installed locally.
#
# Specifically, this removes all files that are git-ignored. New source files
# are preserved, so this command is considered safe.
#
.PHONY: gitclean
gitclean:
	git clean -dfX
	git submodule foreach --recursive git clean -dfX

.PHONY: bump
bump:
	sed -i '' 's/__VERSION__ = ".*"/__VERSION__ = "$(SEMGREP_VERSION)"/g' semgrep/semgrep/__init__.py
	sed -i '' "s/^  rev: 'v.*'$$/  rev: 'v$(SEMGREP_VERSION)'/g" docs/integrations.md
	sed -i '' 's/^    install_requires=\["semgrep==0.23.0"\],$$/    install_requires=["semgrep==$(SEMGREP_VERSION)"],/g' setup.py

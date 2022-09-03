#
# This makefile is targeted at developers.
# For a one-shot production build, look into Dockerfile.
#

# Used to select commands with different usage under GNU/Linux and *BSD/Darwin
# such as 'sed'.
ifeq ($(shell uname -s),Linux)
  LINUX = true
else
  LINUX = false
endif

# :(
ifeq ($(LINUX),true)
  SED = sed -i -e
else
  SED = sed -i ''
endif

# Routine build. It assumes all dependencies and configuration are already
# in place and correct. It should be fast since it's called often during
# development.
#
.PHONY: build
build:
	$(MAKE) build-core
	$(MAKE) -C toy-matcher
	cd cli && pipenv install --dev
	$(MAKE) -C cli build

.PHONY: install
install:
	$(MAKE) -C semgrep-core install
	python3 -m pip install semgrep

.PHONY: build-core
build-core:
	$(MAKE) -C semgrep-core
	$(MAKE) -C semgrep-core install

# Update and rebuild everything within the project.
#
.PHONY: rebuild
rebuild:
	git submodule update --init
	-$(MAKE) clean
	$(MAKE) build

# It is better to run this from a fresh repo or after a 'make clean',
# to not send too much data to the docker daemon.
# For a fresh repo you will need at least to run first 'git submodule update --init'.
.PHONY: build-docker
build-docker:
	docker build -t semgrep .


# This is a best effort to install some external dependencies.
# Should run infrequently.
#
.PHONY: setup
setup:
	git submodule update --init
	# Fetch, build and install the tree-sitter runtime library locally.
	cd semgrep-core/src/ocaml-tree-sitter-core \
	&& ./configure \
	&& ./scripts/install-tree-sitter-lib
	# Install OCaml dependencies (globally).
	opam update -y
	opam install -y --deps-only ./semgrep-core/src/pfff
	opam install -y --deps-only ./semgrep-core/src/ocaml-tree-sitter-core
	opam install -y --deps-only ./semgrep-core

# Install dependencies needed for the Homebrew build.
#
# We don't use just 'make setup' because Homebrew installs its own version
# of tree-sitter, globally.
# The Homebrew package definition ("formula") lives at:
#   https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
#
# Some of this can be tested on Linux, see instructions in
#   dockerfiles/linuxbrew.Dockerfile
#
.PHONY: homebrew-setup
homebrew-setup:
	cd semgrep-core/src/ocaml-tree-sitter-core \
	&& ./configure --prefix "$$(brew --prefix tree-sitter)"
	# We pass --no-depexts so as to disable the check for pkg-config
	# (which is present due to brew dependencies)
	# because this check was failing on some platform.
	# See details at https://github.com/Homebrew/homebrew-core/pull/82693.
	# This workaround may no longer be necessary.
	opam install -y --deps-only --no-depexts ./semgrep-core/src/pfff
	opam install -y --deps-only --no-depexts ./semgrep-core/src/ocaml-tree-sitter-core
	opam install -y --deps-only --no-depexts ./semgrep-core

# Install development dependencies in addition to build dependencies.
#
.PHONY: dev-setup
dev-setup:
	$(MAKE) setup
	opam install -y --deps-only ./semgrep-core/dev

# Remove from the project tree everything that's not under source control
# and was not created by 'make setup'.
#
.PHONY: clean
clean:
	-$(MAKE) -C semgrep-core clean
	-$(MAKE) -C cli clean

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

# Prepare a release branch interactively.
# It's safe to run it multiple times.
.PHONY: release
release:
	./scripts/release/bump

.PHONY: test
test:
	$(MAKE) -C semgrep-core test
	$(MAKE) -C cli test

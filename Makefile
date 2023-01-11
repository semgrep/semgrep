###############################################################################
# Prelude
###############################################################################

# Many targets in this Makefile assume some commands have been run before to
# install the correct build environment supporting the different languages
# used for Semgrep development:
#  - for C: the classic 'gcc', 'ld', but also some C libraries like PCRE
#  - for OCaml: 'opam' and the right OCaml switch (currently 4.14)
#  - for Python: 'python3', 'pip', 'pipenv', 'python-config'
#
# You will also need obviously 'make', but also 'git', and many other
# common dev tools (e.g., 'docker').
#
# Once this basic building/development environment has been setup
# (see the different 'install-deps-XXX-yyy' targets later to assist you),
# you can then use:
#
#     $ make install-deps
#
# to install the dependencies proper to semgrep (e.g., the necessary OPAM
# packages used by semgrep-core).
#
# Then to compile semgrep simply type:
#
#     $ make
#
# See INSTALL.md for more information
# See also https://semgrep.dev/docs/contributing/contributing-code/

###############################################################################
# Portability tricks
###############################################################################

# Most of the targets in this Makefile should work equally under
# Linux (Alpine, Ubuntu, Arch linux), macOS, from a Dockerfile, and
# hopefully also under Windows WSL.
# The main exceptions are the install-deps-XXX-yyy targets below.
# If you really have to use platform-specific commands or flags, try to use
# macros like the one below to make the Makefile portable.

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

###############################################################################
# Build (and clean) targets
###############################################################################

# First (and default) target. Routine build.
# It assumes all dependencies and configuration are already in place and correct.
# It should be fast since it's called often during development.
.PHONY: build
build:
	$(MAKE) build-core
	cd cli && pipenv install --dev
	$(MAKE) -C cli build

.PHONY: build-core
build-core:
	$(MAKE) -C semgrep-core
	# We run this command because the Python code in cli/ assumes the
	# presence of a semgrep-core binary in the PATH somewhere.
	$(MAKE) -C semgrep-core install

# It is better to run this from a fresh repo or after a 'make clean',
# to not send too much data to the Docker daemon.
# For a fresh repo you will need at least to run first 'git submodule update --init'.
.PHONY: build-docker
build-docker:
	docker build -t semgrep .

# Remove from the project tree everything that's not under source control
# and was not created by 'make setup'.
.PHONY: clean
clean:
	-$(MAKE) -C semgrep-core clean
	-$(MAKE) -C cli clean

###############################################################################
# Install targets
###############################################################################

.PHONY: install
install:
	$(MAKE) -C semgrep-core install
	python3 -m pip install semgrep

###############################################################################
# Test target
###############################################################################

.PHONY: test
test:
	$(MAKE) -C semgrep-core test
	$(MAKE) -C cli test

###############################################################################
# External dependencies installation targets
###############################################################################

# **************************************************
# Platform-independent dependencies installation
# **************************************************

# This target is portable; it only assumes you have 'gcc', 'opam' and
# other build-essential tools and a working OCaml (e.g., ocamlc) switch setup.
# Note that this target is now called from our Dockerfile, so do not
# run 'opam update' below to not slow down things.
install-deps-for-semgrep-core:
	# Fetch, build and install the tree-sitter runtime library locally.
	cd semgrep-core/src/ocaml-tree-sitter-core \
	&& ./configure \
	&& ./scripts/install-tree-sitter-lib
	# Install OCaml dependencies (globally).
	opam install -y --deps-only ./semgrep-core/src/pfff
	opam install -y --deps-only ./semgrep-core/src/ocaml-tree-sitter-core
	opam install -y --deps-only ./semgrep-core

# We could also add python dependencies at some point
# and an 'install-deps-for-semgrep-cli' target
install-deps: install-deps-for-semgrep-core

# **************************************************
# Platform-dependent dependencies installation
# **************************************************

# -------------------------------------------------
# Alpine
# -------------------------------------------------

# Here is why we need those external packages below:
# - pcre-dev: for ocaml-pcre now used in semgrep-core
# - python3: used also during building semgrep-core for processing lang.json
# - python3-dev: for the semgrep Python bridge to build Python C extensions
# - gmp-dev: for osemgrep and its use of cohttp
ALPINE_APK_DEPS=pcre-dev python3 python3-dev gmp-dev

#TODO why this one?
PIPENV='pipenv==2022.6.7'

# This target is used in our Dockerfile and a few GHA workflows.
# There are pros and cons of having those commands here instead
# of in the Dockerfile and GHA workflows:
# cons:
#  - this requires the Makefile and so to checkout (COPY in Docker
#    or actions/checkout@v3 in GHA) semgrep first,
#    which prevent some caching Docker/GHA could do. This is alleviated
#    a bit by the fact that anyway we use a special returntocorp/ocaml
#    container with many things pre-installed.
# pro:
#  - it avoids repeating yourself everywhere
install-deps-ALPINE-for-semgrep-core:
	apk add --no-cache $(ALPINE_APK_DEPS)
	pip install --no-cache-dir $(PIPENV)

#TODO: deprecate scripts/install-alpine-xxx in favor of that
install-deps-and-build-ALPINE-semgrep-core:
	$(MAKE) install-deps-ALPINE-for-semgrep-core
	$(MAKE) install-deps
	$(MAKE)
	$(MAKE) install

# -------------------------------------------------
# Ubuntu
# -------------------------------------------------

# -------------------------------------------------
# macOS (brew)
# -------------------------------------------------

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

# -------------------------------------------------
# Arch Linux
# -------------------------------------------------
#TODO: pacman -S ...

###############################################################################
# Developer targets
###############################################################################

# This is a best effort to install some external dependencies.
# As a developer you should not run frequently 'make setup', only when
# important dependencies change.
.PHONY: setup
setup:
	git submodule update --init
	opam update -y
	make install-deps-for-semgrep-core

# Install development dependencies in addition to build dependencies.
.PHONY: dev-setup
dev-setup:
	$(MAKE) setup
	opam install -y --deps-only ./semgrep-core/dev

# Update and rebuild everything within the project.
.PHONY: rebuild
rebuild:
	git submodule update --init
	-$(MAKE) clean
	$(MAKE) build

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

# Prepare a release branch.
# This is mainly called by .github/workflows/start-release.yml
# It's safe to run it multiple times.
.PHONY: release
release:
	./scripts/release/bump

update_semgrep_rules:
	cd semgrep-core/tests/semgrep-rules; git checkout origin/develop

###############################################################################
# Dogfood!
###############################################################################
# There are a few places where we currently dogfood Semgrep:
#
# - in this Makefile with 'make check' below, which tests semgrep in PATH
#   and with 'make check_with_docker' which tests semgrep Docker image,
#   and where we use semgrep.jsonnet in both targets
#
# - in pre-commit in .pre-commit-config.yaml which tests the semgrep
#   Docker image used in a pre-commit 'language: docker_image' context,
#   as well as semgrep official pre-commit hooks in .pre-commit-hooks.yaml
#   in a 'language: python' context (which itself uses setup.py to install semgrep),
#   with semgrep.jsonnet but also with p/python and p/bandit rulesets.
#
# - in circle CI in .circle/config.yml which uses the Docker image
#   and where we use semgrep.jsonnet
#
# - in Github Actions (GHA) in .github/workflows/semgrep.yml where
#   we use semgrep-actions and the App to get the rules
#
# Note that many of those places use semgrep.jsonnet and so would report
# the same findings, but they are useful anyway to test all the different
# places where you can plug semgrep (Makefile, pre-commit, circleCI, GHA, GHA+App).


#coupling: see also .circleci/config.yml and its 'semgrep' job
SEMGREP_ARGS=--config semgrep.jsonnet --error --exclude tests --exclude pfff
# you can add --verbose for debugging

DOCKER_IMAGE=returntocorp/semgrep:develop

# You need to have semgrep in your PATH! do 'cd cli; pipenv shell' before
# if needed.
.PHONY: check
check:
	semgrep $(SEMGREP_ARGS)

# If you get semgrep-core parsing errors while running this command, maybe you
# have an old cached version of the docker image.
# You can invalidate the cache with 'docker rmi returntocorp/semgrep:develop`
check_with_docker:
	docker run --rm -v "${PWD}:/src" $(DOCKER_IMAGE) semgrep $(SEMGREP_ARGS)

# I use the docker here instead of directly semgrep, because semgrep is not always
# in my PATH.
#TODO: this will be less needed once we run semgrep with semgrep.jsonnet in pre-commit
check_for_emacs:
	docker run --rm -v "${PWD}:/src" $(DOCKER_IMAGE) semgrep $(SEMGREP_ARGS) --emacs --quiet

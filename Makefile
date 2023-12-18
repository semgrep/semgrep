###############################################################################
# Prelude
###############################################################################

# Many targets in this Makefile assume some commands have been run before to
# setup the correct build environment supporting the different languages
# used for Semgrep development:
#  - for OCaml: 'opam' and the right OCaml version (currently 4.14)
#  - for C: the classic 'gcc', 'ld', but also some C libraries like PCRE
#  - for Python: 'python3', 'pip', 'pipenv'
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
#     $ make all
#
# See INSTALL.md for more information
# See also https://semgrep.dev/docs/contributing/contributing-code/
#
# Most of the targets in this Makefile should work equally under
# Linux (Alpine, Ubuntu, Arch linux), macOS, from a Dockerfile, and
# hopefully also under Windows WSL.
# The main exceptions are the install-deps-XXX-yyy targets below.
# If you really have to use platform-specific commands or flags, try to use
# macros like the one below to have a portable Makefile.
#
#     # To select commands with different usage under GNU/Linux and *BSD/Darwin
#     ifeq ($(shell uname -s),Linux)
#       LINUX = true
#     else
#       LINUX = false
#     endif
#     ifeq ($(LINUX),true)
#       SED = sed -i -e
#     else
#       SED = sed -i ''
#     endif

###############################################################################
# Build (and clean) targets
###############################################################################

# Set environment variables used by dune files to locate the
# C headers and libraries of the tree-sitter runtime library.
# This file is created by ocaml-tree-sitter-core's configure script.
#
# Because of these required environment variables, we can't call dune directly
# to build semgrep-core, unless you manually execute first
#  `source src/ocaml-tree-sitter-core/tree-sitter-config.sh`
#
# I use '-include' and not 'include' because before 'make setup' this file does
# not exist but we still want 'make setup' to succeed
-include libs/ocaml-tree-sitter-core/tree-sitter-config.mk

# First (and default) target.
.PHONY: default
default: core

# Routine build. It assumes all dependencies and configuration are already in
# place and correct.
.PHONY: all
all:
	# OCaml compilation
	$(MAKE) core
	$(MAKE) copy-core-for-cli
	$(MAKE) build-semgrep-jsoo
	$(MAKE) unused-libs
	# Python setup
	cd cli && pipenv install --dev
	$(MAKE) -C cli build

# Build library code that may not being used or tested in this repo such as
# libs/graph_code which is used by semgrep-proprietary.
# This checks that the code compiles.
.PHONY: unused-libs
unused-libs:
	dune build languages
	dune build libs

#history: was called the 'all' target in semgrep-core/Makefile before
.PHONY: core
core:
	$(MAKE) minimal-build
	# make executables easily accessible for manual testing:
	ln -s semgrep-core bin/osemgrep
	chmod +w _build/install/default/bin/semgrep-core
	strip _build/install/default/bin/semgrep-core

#history: was called the 'all' target in semgrep-core/Makefile before
.PHONY: core-bc
core-bc: minimal-build-bc
	ln -s semgrep-core.bc bin/osemgrep.bc

# Make binaries available to pysemgrep
.PHONY: copy-core-for-cli
copy-core-for-cli:
	rm -f cli/src/semgrep/bin/semgrep-core
	cp bin/semgrep-core cli/src/semgrep/bin/

# Minimal build of the semgrep-core executable. Intended for the docker build.
# Requires the environment variables set by the included file above.
# Builds only the binary needed for development.
# If you need other binaries, look at the build-xxx rules below.
.PHONY: minimal-build
minimal-build:
	dune build _build/install/default/bin/semgrep-core


.PHONY: minimal-build-bc
minimal-build-bc:
	dune build _build/install/default/bin/semgrep-core.bc

# It is better to run this from a fresh repo or after a 'make clean',
# to not send too much data to the Docker daemon.
# For a fresh repo you will need at least to run first 'git submodule update --init'.
.PHONY: build-docker
build-docker:
	docker build -t semgrep .

.PHONY: build-otarzan
build-otarzan:
	dune build _build/install/default/bin/otarzan

.PHONY: build-ojsonnet
build-ojsonnet:
	dune build _build/install/default/bin/ojsonnet

.PHONY: build-pfff
build-pfff:
	dune build _build/install/default/bin/pfff

# This is an example of how to build one of those parse-xxx ocaml-tree-sitter binaries
.PHONY: build-parse-cairo
build-parse-cairo:
	dune build _build/install/default/bin/parse-cairo

.PHONY: build-semgrep-jsoo
build-semgrep-jsoo:
	dune build js --profile=release

# Build Semgrep JS w/debug symbols, no mangling and source maps
.PHONY: build-semgrep-jsoo-debug
build-semgrep-jsoo-debug:
	dune build js --profile=dev

# Remove from the project tree everything that's not under source control
# and was not created by 'make setup'.
.PHONY: clean
clean:
	-$(MAKE) core-clean
	-$(MAKE) -C cli clean

#history: was the 'clean' target in semgrep-core/Makefile before
.PHONY: core-clean
core-clean:
	dune clean
	# We still need to keep the nonempty opam files in git for
	# 'make setup', so we should only remove the empty opam files.
	# This removes the gitignored opam files.
	git clean -fX *.opam

###############################################################################
# Install targets
###############################################################################

# Install semgrep on a developer's machine with pip and opam installed.
# This should *not* install the open-source libraries that we maintain
# as part of the semgrep project.
.PHONY: install
install:
	$(MAKE) copy-core-for-cli
	# Install semgrep and semgrep-core in a place known to pip.
	python3 -m pip install ./cli

.PHONY: uninstall
uninstall:
	-python3 -m pip uninstall --yes semgrep

###############################################################################
# Test target
###############################################################################

# Note that this target is actually not used in CI; it's only for local dev
.PHONY: test
test: core-test

# Experimental - only (re-)run the failed tests
.PHONY: retest
retest:
	$(MAKE) build-core-test
	./test run --lazy

# Note that this target is actually not used in CI; it's only for local dev
.PHONY: test-all
test-all:
	$(MAKE) core-test
	$(MAKE) -C cli test
	$(MAKE) -C cli osempass

#coupling: this is run by .github/workflow/tests.yml
.PHONY: core-test
core-test:
	$(MAKE) build-core-test
	# The following command ensures that we can call 'test.exe --help'
	# from the directory of the checkout
	./test --help 2>&1 >/dev/null
	./scripts/run-core-test

# Please keep this standalone target.
# We want to rebuild the tests without re-running all of them.
# This is for working on one or a few specific test cases.
# It rebuilds the test executable which can then be called with
# './test <filter>' where <filter> selects the tests to run.
.PHONY: build-core-test
build-core-test:
	# Invoke the test program with './test'. Check out './test --help'.
	dune build ./_build/default/src/tests/test.exe

.PHONY: test-bc
test-bc:
	# Bytecode version of the test for debugging
	dune build ./_build/default/src/tests/test.bc


#coupling: this is run by .github/workflow/tests.yml
.PHONY: core-test-e2e
core-test-e2e:
	SEMGREP_CORE=$(PWD)/bin/semgrep-core \
	$(MAKE) -C interfaces/semgrep_interfaces test
	python3 tests/semgrep-core-e2e/test_target_file.py

.PHONY: test-jsoo
test-jsoo: build-semgrep-jsoo-debug
	$(MAKE) -C js test

# Test the compatibility with the main branch of semgrep-proprietary
# in a separate work tree.
.PHONY: pro
pro:
	test -L semgrep-proprietary || ln -s ../semgrep-proprietary .
	@if ! test -e semgrep-proprietary; then \
	  echo "** Please fix the symlink 'semgrep-proprietary'."; \
	  echo "** Make it point to your semgrep-proprietary repo."; \
	  exit 1; \
	fi
	set -eu && \
	worktree_parent=$$(pwd)/.. && \
	commit=$$(git rev-parse --short HEAD) && \
	cd semgrep-proprietary && \
	./scripts/check-compatibility \
	  --worktree "$$worktree_parent"/semgrep-pro-compat \
	  --semgrep-commit "$$commit" \
	  --pro-commit origin/develop

###############################################################################
# External dependencies installation targets
###############################################################################

# **************************************************
# Platform-independent dependencies installation
# **************************************************

# We need to install all the dependencies in a single 'opam install'
# command so as to detect conflicts.
REQUIRED_DEPS = ./ ./libs/ocaml-tree-sitter-core ./dev/required.opam
OPTIONAL_DEPS = $(REQUIRED_DEPS) ./dev/optional.opam

# This target is portable; it only assumes you have 'gcc', 'opam' and
# other build-essential tools and a working OCaml (e.g., ocamlc) switch setup.
# Note that this target is now called from our Dockerfile, so do not
# run 'opam update' below to not slow down things.
.PHONY: install-deps-for-semgrep-core
install-deps-for-semgrep-core: semgrep.opam
	# Fetch, build and install the tree-sitter runtime library locally.
	cd libs/ocaml-tree-sitter-core \
	&& ./configure \
	&& ./scripts/install-tree-sitter-lib
	# Install OCaml dependencies (globally) from *.opam files.
	# This now also installs the dev dependencies. This has the benefit
	# of installing all the packages in one shot and detecting possible
	# version conflicts.
	opam install -y --deps-only $(REQUIRED_DEPS)

# This will fail if semgrep.opam isn't up-to-date (in git),
# and dune isn't installed yet. You can always install dune with
# 'opam install dune' to get started.
semgrep.opam: dune-project
	dune build $@
	# Foolproofing
	chmod a-w semgrep.opam

# The bytecode version of semgrep-core needs dlls for tree-sitter
# stubs installed into ~/.opam/<switch>/lib/stublibs to be able to run.
install-deps-for-semgrep-core-bc: install-deps-for-semgrep-core
	dune build @install # Generate the treesitter stubs for below
	dune install # Needed to install treesitter_<lang> stubs for use by bytecode

# We could also add python dependencies at some point
# and an 'install-deps-for-semgrep-cli' target
install-deps: install-deps-for-semgrep-core

# **************************************************
# Platform-dependent dependencies installation
# **************************************************

# -------------------------------------------------
# Alpine
# -------------------------------------------------

# Here is why we need those external packages to compile semgrep-core:
# - pcre-dev: for ocaml-pcre now used in semgrep-core
# - gmp-dev: for osemgrep and its use of cohttp
ALPINE_APK_DEPS_CORE=pcre-dev gmp-dev libev-dev

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
	apk add --no-cache $(ALPINE_APK_DEPS_CORE)


# Here is why we need those external packages below for pysemgrep:
# - python3: obviously needed for pysemgrep and our e2e tests
# - python-dev: for compiling jsonnet for pysemgrep
ALPINE_APK_DEPS_PYSEMGREP=python3 python3-dev
# We pin to a specific version just to prevent things from breaking randomly.
# We could update to a more recent version.
# coupling: if you modify the version, please modify also .github/workflows/*
PIPENV='pipenv==2022.6.7'

# For '--ignore-installed distlib' below see
# https://stackoverflow.com/questions/63515454/why-does-pip3-install-pipenv-give-error-error-cannot-uninstall-distlib
install-deps-ALPINE-for-pysemgrep:
	apk add --no-cache $(ALPINE_APK_DEPS_PYSEMGREP)
	pip install --no-cache-dir --ignore-installed distlib $(PIPENV)

# -------------------------------------------------
# Ubuntu
# -------------------------------------------------
UBUNTU_DEPS=pkg-config libgmp-dev libpcre3-dev libev-dev

install-deps-UBUNTU-for-semgrep-core:
	apt-get install -y $(UBUNTU_DEPS)

# -------------------------------------------------
# macOS (brew)
# -------------------------------------------------

# Here is why we need those external packages below:
# - pcre: for ocaml-pcre now used in semgrep-core
# - gmp: for osemgrep (now merged with semgrep-core) and its use of cohttp
# - pkg-config?
# - coreutils?
# - gettext?
BREW_DEPS=pcre gmp pkg-config coreutils gettext libev

# see also scripts/osx-setup-for-release.sh that adjust those
# external packages to force static-linking
install-deps-MACOS-for-semgrep-core:
	brew install $(BREW_DEPS)

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
	cd libs/ocaml-tree-sitter-core \
	&& ./configure --prefix "$$(brew --prefix tree-sitter)"
	# We pass --no-depexts so as to disable the check for pkg-config
	# (which is present due to brew dependencies)
	# because this check was failing on some platform.
	# See details at https://github.com/Homebrew/homebrew-core/pull/82693.
	# This workaround may no longer be necessary.
	# LIBRARY_PATH is set here so we build lwt w/libev
	LIBRARY_PATH="$$(brew --prefix)/lib" opam install -y --deps-only --no-depexts $(REQUIRED_DEPS)

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
setup: semgrep.opam
	./scripts/check-bash-version
	opam update -y
	$(MAKE) install-deps-for-semgrep-core

# Install optional development dependencies in addition to build dependencies.
.PHONY: dev-setup
dev-setup:
	$(MAKE) setup
	opam install -y --deps-only $(OPTIONAL_DEPS)

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

.PHONY: update_semgrep_rules
update_semgrep_rules:
	cd tests/semgrep-rules; git checkout origin/develop

# Run utop with all the semgrep-core libraries loaded.
.PHONY: utop
utop:
	dune utop

# This is for tools/hello_script.ml so it can leverage the semgrep libs
# (e.g., commons) by installing them in ~/.opam/.../
.PHONY: install-semgrep-libs
install-semgrep-libs: semgrep.opam
	dune build
	dune install

.PHONY: dump
dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

# Run perf benchmarks
# Running this will reset your `semgrep` command to point to your local version
# For more information, see "Reproducing the CI benchmarks" in perf/README.md
.PHONY: perf-bench
perf-bench:
	scripts/run-benchmarks.sh


# Run matching performance tests
.PHONY: perf-matching
perf-matching:
	@echo "--- default settings ---"
	cd ./perf/perf-matching && ./run-perf-suite
	@echo "--- no caching ---"
	cd ./perf/perf-matching && ./run-perf-suite --no-cache
	@echo "--- maximum caching ---"
	cd ./perf/perf-matching && ./run-perf-suite --max-cache

# Run matching performance tests and post them to the semgrep dashboard
# at https://dashboard.semgrep.dev/
#
# This is meant for CI, which hopefully runs on similar machines each time.
#
.PHONY: report-perf-matching
report-perf-matching:
	cd ./perf/perf-matching && ./run-perf-suite --upload

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
SEMGREP_ARGS=--experimental --config semgrep.jsonnet --error --exclude tests
# you can add --verbose for debugging

#Dogfooding osemgrep!
.PHONY: check
check:
	./bin/osemgrep $(SEMGREP_ARGS)

check_for_emacs:
	./bin/osemgrep $(SEMGREP_ARGS) --emacs --quiet

DOCKER_IMAGE=returntocorp/semgrep:develop

# If you get parsing errors while running this command, maybe you have an old
# cached version of the docker image. You can invalidate the cache with
#   'docker rmi returntocorp/semgrep:develop`
check_with_docker:
	docker run --rm -v "${PWD}:/src" $(DOCKER_IMAGE) semgrep $(SEMGREP_ARGS)

###############################################################################
# Martin's targets
###############################################################################
# Build executables and place them where semgrep expects them.
# These are normally copied by '/cli/setup.py' but it doesn't happen if we
# run only 'dune build'.
#
# Usage:
#  $ make dev
#  $ PIPENV_PIPFILE=~/semgrep/cli/Pipfile pipenv run semgrep ...
.PHONY: dev
dev:
	$(MAKE) core
	$(MAKE) copy-core-for-cli

###############################################################################
# Pad's targets
###############################################################################

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b develop -r returntocorp/pa

push:
	git push origin `git rev-parse --abbrev-ref HEAD`

merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout develop && git pull && git branch -D $$A


# see https://github.com/aryx/codegraph for information on codegraph_build
index:
	codegraph_build -lang cmt -derived_data .

# see https://github.com/aryx/codecheck for information on codecheck
check2:
	codecheck -lang ml -with_graph_code graph_code.marshall -filter 3 .

# see https://github.com/aryx/codemap for information on codemap
visual:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null .
visual2:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null src

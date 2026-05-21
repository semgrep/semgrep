###############################################################################
# Prelude
###############################################################################

# Many targets in this Makefile assume some commands have been run before to
# setup the correct build environment supporting the different languages
# used for Semgrep development.
#  - for OCaml: 'ocamlc' and 'ocamlopt' (currently 4.14.0), 'dune', 'opam'
#  - for C: 'gcc', 'ld', 'pkgconfig', but also some C libs like PCRE, gmp
#  - for Python: 'python3', 'pip', 'pipenv'
#
# You will also need obviously 'make', but also 'git', and many other
# common dev tools (e.g., 'docker', 'bash').
#
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

# Most of the targets in this Makefile should work equally under
# Linux (Alpine, Ubuntu, Arch), macOS (x86 and arm64), Windows (WSL, Cygwin),
# and from a Dockerfile.

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

# This is to deal with paths that change depending on whether we're in the
# semgrep-proprietary monorepo or detached as a standalone semgrep project.
# The script 'scripts/make-symlinks' also deals with such issues.
PROJECT_ROOT = $(shell git rev-parse --show-toplevel || pwd)
ifeq ($(shell pwd),$(PROJECT_ROOT))
  # The root is here.
  BUILD = _build
  BUILD_DEFAULT = _build/default
else
  # Assume we're in the semgrep-proprietary repo where OSS/ = semgrep.
  BUILD = ../_build
  BUILD_DEFAULT = ../_build/default/OSS
endif

-include cygwin-env.mk

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
# Python setup
	cd cli && pipenv install --dev

# Make binaries available to pysemgrep
.PHONY: copy-core-for-cli
copy-core-for-cli:
	rm -f cli/src/semgrep/bin/semgrep-core$(EXE)
	cp bin/semgrep-core$(EXE) cli/src/semgrep/bin/

# Minimal build of the semgrep-core executable. Intended for the docker build.
# If you need other binaries, look at the build-xxx rules below.
# We do not use .../bin/{semgrep-core,osemgrep,semgrep} below to
# factorize because make under Alpine uses busybox/ash for /bin/sh which
# does not support this bash feature.
.PHONY: core
core:
	dune build $(BUILD)/install/default/bin/semgrep-core$(EXE)
	dune build $(BUILD)/install/default/bin/osemgrep$(EXE)
	chmod +w $(BUILD)/install/default/bin/semgrep-core$(EXE)

#coupling: The 'semgrep-oss' is the name of the step in the Dockerfile, the
# 'semgrep' the name of the docker image produced (will be semgrep:latest)
.PHONY: build-docker
build-docker:
	docker build -t semgrep --target semgrep-oss .

.PHONY: build-ojsonnet
build-ojsonnet:
	dune build $(BUILD)/install/default/bin/ojsonnet

# Remove from the project tree everything that's not under source control
# and was not created by 'make setup'.
.PHONY: clean
clean:
	dune clean
# We still need to keep the nonempty opam files in git for
# 'make setup', so we should only remove the empty opam files.
# This removes the gitignored opam files.
	git clean -fX *.opam
	-$(MAKE) -C cli clean

###############################################################################
# Install targets
###############################################################################

# Install semgrep on a developer's machine with pip and opam installed.
# This should *not* install the open-source libraries that we maintain
# as part of the semgrep project.
.PHONY: install
install:
	$(MAKE) copy-core-for-cli
# Install semgrep and semgrep-core in editable mode, so that they are available in the PATH and can be called
	uv tool install -e ./cli

.PHONY: uninstall
uninstall:
	uv tool uninstall semgrep

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
	./scripts/make-symlinks
	$(MAKE) build-core-test
# The following command ensures that we can call 'test.exe --help'
# from the directory of the checkout
# TODO: this generates weird cmdliner errors in Windows
	./test --help 2>&1 >/dev/null
	./scripts/run-core-test

# Please keep this standalone target.
# We want to rebuild the tests without re-running all of them.
# This is for working on one or a few specific test cases.
# It rebuilds the test executable which can then be called with
# './test <filter>' where <filter> selects the tests to run.
.PHONY: build-core-test
build-core-test:
	dune build $(BUILD_DEFAULT)/src/tests/test.exe

###############################################################################
# Validation targets
###############################################################################

# Regenerate all .opam files from dune-project and validate dependencies
# This ensures .opam files stay in sync with dune-project and don't have conflicts
.PHONY: check-opam-conflicts
check-opam-conflicts: dune-project
	@dune build *.opam 2>/dev/null || true
	@OUT=$$(find . -maxdepth 1 -name "*.opam" -type f | xargs opam install --deps-only --dry-run 2>&1); \
	if echo "$$OUT" | grep -q "No solution found"; then \
		echo "ERROR: OPAM package conflicts detected:" >&2; \
		echo "$$OUT" >&2; \
		exit 1; \
	fi

###############################################################################
# External dependencies installation targets
###############################################################################

# **************************************************
# Platform-independent dependencies installation
# **************************************************

# We need to install all the dependencies in a single 'opam install'
# command so as to detect conflicts.
# WEIRD: if you use ./libs/ocaml-tree-sitter-core/ instead of the full
# path, then recent versions of opam crash with a 'git ls-files fatal error'
# about some 'libs/ocaml-tree-sitter-core/../../.git/...' not being a git
# repo.
#
# EXTRA_OPAM_DEPS allows us to add more opam files when building semgrep
# as part of a larger project (e.g. semgrep-proprietary). Using a single
# 'opam install' command to install all the dependencies allows us to detect
# version constraints incompatibilities.
#
# weird: when ./semgrep.opam is left as ./, opam install errors with
# "Undefined boolean filter value: os != win32"
REQUIRED_DEPS = \
  ./semgrep.opam \
  ./dev/required.opam \
  $(EXTRA_OPAM_DEPS)

OPTIONAL_DEPS = $(REQUIRED_DEPS) ./dev/optional.opam

# This target is portable; it only assumes you have 'gcc', 'opam' and
# other build-essential tools and a working OCaml (e.g., ocamlc) switch setup.
# Note that we call opam update below because semgrep.opam may mention
# new packages that are not covered yet by our ocaml-layer docker image.
.PHONY: install-deps-for-semgrep-core
install-deps-for-semgrep-core:
# Fetch, build and install the tree-sitter runtime library locally.
	cd libs/ocaml-tree-sitter-core \
	&& ./configure \
	&& ./scripts/install-tree-sitter-lib
# semgrep-core* are dynamically linked, but semgrep-forge is still statically
# linked. On Alpine, that requires a minimal libcurl.a plus the static apk
# variants of libssl/libcrypto/libz. No-op on non-Alpine systems.
	./scripts/build-static-libcurl.sh
	$(MAKE) install-opam-deps

pin-ocaml-fork:
	# the fork without TSan is pinned via our `semgrep.opam.template` file + `semgrep.opam`
	echo "skipping pinning ocaml fork via make"

pin-ocaml-fork-tsan:
	# NBT: our fork of the compiler
	opam pin add ocaml-variants.5.3.0+options "git+https://github.com/semgrep/ocaml.git#5.3.0-semgrep-tsan" --update-invariant -y
# Install OCaml dependencies (globally) from *.opam files.
# This now also installs the dev dependencies. This has the benefit
# of installing all the packages in one shot and detecting possible
# version conflicts.
# OPAMSOLVERTIMEOUT default is 60 but seems not enough
#
# Per the note above install-deps-ALPINE-for-semgrep-core, we may want
# to keep it and add `--no-cache`
#
# Note that we do the upgrade --fixup here to ensure that the dependencies are
# up to date and have all necessary dependencies installed. One would think that
# since opam complains about missing system dependencies when running `opam
# install` it would try and reinstall the system dependencies. By running `opam
# upgrade --fixup` opam will install these missing system deps.
#
# This helps:
# - If someone accidentally uninstalls a package or cancels the installation and
#   breaks the build
# - When we have cache hits in GHA on things like conf-pcre, by default we won't
#   install the pcre system package, this ensures those are reinstalled
install-opam-deps: pin-ocaml-fork$(OPTIONS)
	opam update -y
	# use the opam cache by default, as third party package hosts are unreliable
	opam option --global 'archive-mirrors="https://opam.ocaml.org/cache"'
	# we want to install our forked OCaml compiler, however this contradicts
	# the default 5.3.0 invariant of `ocaml-base-compiler = 5.3.0`.
	# --update-invariant does just that
	OPAMSOLVERTIMEOUT=1500 LWT_DISCOVER_ARGUMENTS="--use-libev true" LIBRARY_PATH="$(HOMEBREW_PREFIX)/lib:$(LIBRARY_PATH)" opam install --locked --update-invariant --confirm-level=unsafe-yes -y --depext-only $(REQUIRED_DEPS)
	OPAMSOLVERTIMEOUT=1500 LWT_DISCOVER_ARGUMENTS="--use-libev true" LIBRARY_PATH="$(HOMEBREW_PREFIX)/lib:$(LIBRARY_PATH)" opam install --locked --update-invariant --confirm-level=unsafe-yes -y --deps-only $(REQUIRED_DEPS)
	# Validate that after installing deps the pinned compiler hasn't changed
	./scripts/validate-compiler-sha.sh

# This installs pyro caml profiler, which allows us to do some nice continous
# profiling (--profile passed to pysemgrep). This is separate from
# install-opam-deps, as it is not compatible with tsan nor windows. This allows
# us to only install it when needed, in the normal docker image build.
# Additionally the profiler relies on Rust as a dependency, so this avoids
# forcing developers to install Rust unless they need it.
# COUPLING: semgrep.opam.template where we pin the library
.PHONY: install-pyro-caml
install-pyro-caml:
	opam pin add --confirm-level=unsafe-yes pyro-caml.dev $$(cat semgrep.opam | grep -E -o "git\+.*pyro-caml.git#[a-zA-Z0-9]*" | head -1)
	opam install pyro-caml -y  --confirm-level=unsafe-yes

# This will fail if semgrep.opam isn't up-to-date (in git),
# and dune isn't installed yet. You can always install dune with
# 'opam install dune' to get started.
semgrep.opam: dune-project semgrep.opam.template
	dune build $@
# Foolproofing
	chmod a-w semgrep.opam

# We could also add python dependencies at some point
# and an 'install-deps-for-semgrep-cli' target
install-deps:
	./scripts/pick-lockfile.sh --strict semgrep.opam
	$(MAKE) install-deps-for-semgrep-core

# ******************************************
# Release target
# ******************************************

# Prepare a release branch.
# This is mainly called by .github/workflow/start-release.yml
# it is safe to call it multiple times.
.PHONY: release
release:
	./scripts/release/bump

# **************************************************
# Platform-dependent dependencies installation
# **************************************************

# -------------------------------------------------
# Nix
# -------------------------------------------------
# See flake.nix top level comments for more information

# always accept the semgrep cache substituer
NIX=nix --accept-flake-config

# Enter development environment with all dependencies installed
#
shell:
	$(NIX) develop ".?submodules=1#default"

# exclude all non-nix environment variables, good for debugging
shell-pure:
	$(NIX) develop -i ".?submodules=1#pure"

shell-pure-test:
	$(NIX) develop -i ".?submodules=1#pure" -c make core

# Build targets
# For all the .?submodules=1 we need because nix is weird:
# https://github.com/NixOS/nix/issues/4423#issuecomment-791352686
nix-semgrep:
	$(NIX) build ".?submodules=1#semgrep"

nix-semgrep-core:
	$(NIX) build ".?submodules=1#semgrep-core"

# Build + run tests (doesn't run python tests yet)
nix-check:
	$(NIX) flake check ".?submodules=1#"

# verbose and sandboxing are disabled to enable networking for tests
nix-check-verbose:
	$(NIX) flake check -L ".?submodules=1#"

###############################################################################
# Developer targets
###############################################################################

configure-osx:
	# Prep the OCaml package manager (OPAM)'s environment
	opam init
	brew install uv bash opam
	make configure

configure:
	opam switch create semgrep --empty
	eval $(opam env)

# This is a best effort to install some external dependencies.
# As a developer you should not run frequently 'make setup', only when
# important dependencies change.
.PHONY: setup
setup:
	./scripts/make-symlinks
	./scripts/check-bash-version
	./scripts/pick-lockfile.sh semgrep.opam
	LIBRARY_PATH="$(HOMEBREW_PREFIX)/lib:$(LIBRARY_PATH)" $(MAKE) install-deps-for-semgrep-core

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

# This starts the profiling backend Pyroscope, for use with the pyro caml
# profiler. This is needed to collect + visualize profiling data.
pyroscope:
	docker run --rm -it -p 4040:4040 grafana/pyroscope:latest

# Run utop with all the semgrep-core libraries loaded.
.PHONY: utop
utop:
	dune utop

DOCKER_IMAGE=semgrep/semgrep-nightly:develop

# If you get parsing errors while running this command, maybe you have an old
# cached version of the docker image. You can invalidate the cache with
#   'docker rmi semgrep/semgrep-nightly:develop`
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

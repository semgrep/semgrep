###############################################################################
# Prelude
###############################################################################

# Many targets in this Makefile assume some commands have been run before to
# install the correct build environment supporting the different languages
# used for Semgrep development:
#  - for OCaml: 'opam' and the right OCaml switch (currently 4.14)
#  - for C: the classic 'gcc', 'ld', but also some C libraries like PCRE
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

# First (and default) target. Routine build.
# It assumes all dependencies and configuration are already in place and correct.
# It should be fast since it's called often during development.
.PHONY: build
build:
	$(MAKE) core
	$(MAKE) copy-core-for-cli
	cd cli && pipenv install --dev
	$(MAKE) -C cli build

# was the 'all' target in in semgrep-core/Makefile before
.PHONY: core
core:
	rm -f bin
	$(MAKE) minimal-build
	# make executables easily accessible for manual testing:
	test -e bin || ln -s _build/install/default/bin .
	ln -s semgrep-core bin/osemgrep

# Make binaries available to pysemgrep
#
# TODO: find out and explain why we can't use symlinks
# which is faster, clearer, and less wasteful than full copies.
# It may have something to do with how we do Python or Homebrew packaging.
.PHONY: copy-core-for-cli
copy-core-for-cli:
	# Executables
	rm -f cli/src/semgrep/bin/semgrep-core
	cp _build/install/default/bin/semgrep-core cli/src/semgrep/bin/
	rm -f cli/src/semgrep/bin/osemgrep
	ln -s semgrep-core cli/src/semgrep/bin/osemgrep

# Minimal build of the semgrep-core executable. Intended for the docker build.
# Requires the environment variables set by the included file above.
# Builds only the two main binaries needed for development.
# If you need other binaries, look at the rules below.
.PHONY: minimal-build
minimal-build:
	dune build _build/install/default/bin/semgrep-core

# It is better to run this from a fresh repo or after a 'make clean',
# to not send too much data to the Docker daemon.
# For a fresh repo you will need at least to run first 'git submodule update --init'.
.PHONY: build-docker
build-docker:
	docker build -t semgrep .

# Build just this executable
.PHONY: build-otarzan
build-otarzan:
	rm -f bin
	dune build _build/install/default/bin/otarzan
	test -e bin || ln -s _build/install/default/bin .

# Build just this executable
.PHONY: build-pfff
build-pfff:
	rm -f bin
	dune build _build/install/default/bin/pfff
	test -e bin || ln -s _build/install/default/bin .

# Build just this executable
.PHONY: build-parse-cairo
build-parse-cairo:
	rm -f bin
	dune build _build/install/default/bin/parse-cairo
	test -e bin || ln -s _build/install/default/bin .

# Build just this executable
.PHONY: build-spacegrep
build-spacegrep:
	rm -f bin
	dune build _build/install/default/bin/spacegrep
	test -e bin || ln -s _build/install/default/bin .

# Build just this executable
.PHONY: build-oncall
build-oncall:
	rm -f bin
	dune build _build/install/default/bin/oncall
	test -e bin || ln -s _build/install/default/bin .

# Build the js_of_ocaml portion of the semgrep javascript packages
.PHONY: build-semgrep-jsoo
build-semgrep-jsoo:
	dune build js --profile=release

# Remove from the project tree everything that's not under source control
# and was not created by 'make setup'.
.PHONY: clean
clean:
	-$(MAKE) core-clean
	-$(MAKE) -C cli clean

# was the 'clean' target in in semgrep-core/Makefile before
.PHONY: core-clean
core-clean:
	dune clean
	rm -f bin
	# We still need to keep the nonempty opam files in git for
	# 'make setup', so we should only remove the empty opam files.
	# This removes the gitignored opam files.
	git clean -fX *.opam

###############################################################################
# Install targets
###############################################################################

# Install semgrep on a developer's machine with pip and opam installed.
.PHONY: install
install:
	# Install semgrep-core into opam's bin which is in our PATH.
	# This is not needed or used by the pip install.
	$(MAKE) core-install
	# Install semgrep and semgrep-core in a place known to pip.
	python3 -m pip install ./cli

.PHONY: uninstall
uninstall:
	-$(MAKE) core-uninstall
	-python3 -m pip uninstall --yes semgrep

# Install the semgrep-core executable, as well as any other executable or
# library built from OCaml or C and needed for a complete semgrep install
# for a user of semgrep who builds and installs semgrep from source
# for local use.
#
# This should *not* install the open-source libraries that we maintain
# as part of the semgrep project.
.PHONY: core-install
core-install: copy-core-for-cli
	# The executable created by dune doesn't have the write permission,
	# causing an error when running a straight cp if a file is already
	# there.
	# Known alternative: use 'install -m 0644 ...' instead of cp
	$(MAKE) core-uninstall
	cp bin/semgrep-core "$$(opam var bin)"/

# Try to uninstall what was installed by 'make core-install'.
# This is a best effort.
.PHONY: core-uninstall
core-uninstall:
	rm -f "$$(opam var bin)"/semgrep-core

###############################################################################
# Test target
###############################################################################

# Note that this target is actually not used in CI; it's only for local dev
.PHONY: test
test:
	$(MAKE) core-test
	$(MAKE) -C cli test
	$(MAKE) -C cli osempass

# I put 'all' as a dependency because sometimes you modify a test file
# and dune runtest -f does not see this new file, probably because
# the cached file under _build/.../tests/ is still the old one.
#coupling: this is run by .github/workflow/tests.yml
.PHONY: core-test
core-test: core build-spacegrep
	# The test executable has a few options that can be useful
	# in some contexts.
	$(MAKE) build-core-test
	dune build ./_build/default/src/tests/test.exe
	# The following command ensures that we can call 'test.exe --help'
	# from the directory of the checkout
	./_build/default/src/tests/test.exe --show-errors --help 2>&1 >/dev/null
	$(MAKE) -C libs/spacegrep test
	dune runtest -f --no-buffer

# This is useful when working on one or a few specific test cases.
# It rebuilds the test executable which can then be called with
# './test <filter>' where <filter> selects the tests to run.
.PHONY: build-core-test
build-core-test:
	dune build ./_build/default/src/tests/test.exe

#coupling: this is run by .github/workflow/tests.yml
.PHONY: core-e2etest
core-e2etest:
	SEMGREP_CORE=$(PWD)/bin/semgrep-core \
	$(MAKE) -C interfaces/semgrep_interfaces test
	python3 tests/e2e/test_target_file.py

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
	cd libs/ocaml-tree-sitter-core \
	&& ./configure \
	&& ./scripts/install-tree-sitter-lib
	# Install OCaml dependencies (globally).
	opam install -y --deps-only ./libs/ocaml-tree-sitter-core
	opam install -y --deps-only ./

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

# We pin to a specific version just to prevent things from breaking randomly.
# We could update to a more recent version.
# coupling: if you modify the version, please modify also .github/workflows/*
PIPENV='pipenv==2022.6.7'
#TODO: virtualenv 20.22.0 is causing the build to fail with some weird errors:
# 'AttributeError: module 'virtualenv.create.via_global_ref.builtin.cpython.mac_os' has no attribute 'CPython2macOsArmFramework'
# so I pinned an older version
VIRTENV='virtualenv==20.21.0'

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
# For '--ignore-installed distlib' below see
# https://stackoverflow.com/questions/63515454/why-does-pip3-install-pipenv-give-error-error-cannot-uninstall-distlib
install-deps-ALPINE-for-semgrep-core:
	apk add --no-cache $(ALPINE_APK_DEPS)
	pip install --no-cache-dir --ignore-installed distlib $(PIPENV) $(VIRTENV)

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
	cd libs/ocaml-tree-sitter-core \
	&& ./configure --prefix "$$(brew --prefix tree-sitter)"
	# We pass --no-depexts so as to disable the check for pkg-config
	# (which is present due to brew dependencies)
	# because this check was failing on some platform.
	# See details at https://github.com/Homebrew/homebrew-core/pull/82693.
	# This workaround may no longer be necessary.
	opam install -y --deps-only --no-depexts ./libs/ocaml-tree-sitter-core
	opam install -y --deps-only --no-depexts ./

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
	opam install -y --deps-only ./dev

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

.PHONY: dump
dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

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
SEMGREP_ARGS=--config semgrep.jsonnet --error --exclude tests
# you can add --verbose for debugging

#Dogfooding osemgrep!
.PHONY: check
check:
	./bin/osemgrep $(SEMGREP_ARGS)

check_for_emacs:
	./bin/osemgrep $(SEMGREP_ARGS) --emacs --quiet

DOCKER_IMAGE=returntocorp/semgrep:develop

# If you get semgrep-core parsing errors while running this command, maybe you
# have an old cached version of the docker image.
# You can invalidate the cache with 'docker rmi returntocorp/semgrep:develop`
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
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .
visual2:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null src

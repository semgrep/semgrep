#
# Build a rich environment with semgrep pre-built in it and all its dependency,
# similar to a developer's environment.
#
# The goal is to make it easy to implement benchmarks and other jobs that
# are first tested locally by the developer then will run in CI.
#
FROM returntocorp/ocaml:ubuntu
LABEL maintainer="support@r2c.dev"

# Install missing python tools
USER root
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
         python3-pip \
         python-is-python3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
USER user

RUN python -m pip install pipenv
# For pipenv:
ENV PATH=/home/user/.local/bin:$PATH

COPY --chown=user . semgrep

WORKDIR /home/user/semgrep

# Protect against dirty environment during development.
# (ideally, we should translate .gitignore to .dockerignore)
RUN git clean -dfX
RUN git submodule foreach --recursive git clean -dfX

RUN git submodule update --init --recursive --depth 1

RUN opam exec -- ./scripts/install-ocaml-tree-sitter
RUN opam exec -- opam install --deps-only -y spacegrep/
RUN opam exec -- opam install --deps-only -y semgrep-core/src/pfff/
RUN opam exec -- opam install --deps-only -y semgrep-core/
RUN opam exec -- make

# Install into ~/.opam folder, which is not in PATH :(
RUN opam exec -- make install

# Sanity checks
RUN test -x ./spacegrep/bin/spacegrep
RUN ./semgrep-core/bin/semgrep-core -version

# Install executables where they can be found without tweaking PATH
# TODO: have the ocaml base image set PATH and other variables properly
USER ROOT
RUN mkdir -p /usr/local/bin
RUN ln -s "$(pwd)"/spacegrep/bin/spacegrep /usr/local/bin
RUN ln -s spacegrep /usr/local/bin/spacecat
RUN ln -s "$(pwd)"/semgrep-core/bin/semgrep-core /usr/local/bin
USER user

# Install semgrep frontend
RUN SEMGREP_SKIP_BIN=true python -m pip install semgrep

# Sanity check
RUN semgrep --version

# TODO: do we need the following? please explain what it's for
ENV SEMGREP_IN_DOCKER=1
ENV SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version
ENV SEMGREP_USER_AGENT_APPEND="(Docker)"
ENV PYTHONIOENCODING=utf8
ENV PYTHONUNBUFFERED=1

ENTRYPOINT ["bash"]

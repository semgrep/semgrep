#
# A container with semgrep in it, made to work from within the container.
# - changes the entrypoint to bash
# - installs some basic packages
#
# This is used for running semgrep benchmarks and other CI jobs without having
# to rebuild semgrep from scratch.
#

# Version of semgrep built each time the main ('develop') branch is updated.
# It's built from the main Dockerfile (semgrep.Dockerfile) in the semgrep repo.
#
# For things like benchmarks, we want a "recent version of semgrep that works".
#
FROM returntocorp/semgrep:develop

# Various utilities. We can always install them during the CI job but it's
# it's nice to do it here while we're root.
#
RUN apk add --no-cache \
  bash \
  curl \
  jq

# Install pipx so that we can install python utilities into isolated virtualenvs
RUN python3 -m pip install --no-cache-dir --user pipx==1.2.0 && python3 -m pipx ensurepath

# Install pre-commit so that we can run pre-commit checks inside the dev container
RUN /root/.local/bin/pipx install pre-commit==3.2.2

# Let the user know how their container was built
COPY dockerfiles/semgrep-dev.Dockerfile /Dockerfile

# cd ~
WORKDIR /home/semgrep

ENTRYPOINT ["/bin/bash"]

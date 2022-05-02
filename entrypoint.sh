#!/usr/bin/env bash
set -e

if [[ "$*" =~ "--config" || "$*" == "--help" || "$1" == "publish" || "$1" == "ci" ]]; then
  # 1) --config is a required semgrep scan argument,
  #    so if it's present, we assume the user meant to call semgrep.
  #
  #    -f is a valid --config alias,
  #    but that could also be part of many shell commands used by e.g. GitLab CI,
  #    and our docs always type out the full --config flag anyway,
  #    so we don't assume user meant to call semgrep in those cases.
  #
  # 2) if a known subcommand is used, we also assume the user meant to call semgrep.
  >&2 echo "======= DEPRECATION WARNING ======="
  >&2 echo "The returntocorp/semgrep Docker image's custom entrypoint will be removed by June 2022."
  >&2 echo "Please update your command to explicitly call semgrep."
  >&2 echo "Change from:  docker run returntocorp/semgrep $*"
  >&2 echo "Change to:    docker run returntocorp/semgrep semgrep $*"
  exec semgrep "$@"
fi

exec "$@"

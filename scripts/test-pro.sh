#! /usr/bin/env bash

set -e

# Relocate to the directory, because otherwise some weirdness happens and
# we run with all the rules, taking ~30 minutes.
cd /root || exit

semgrep install-semgrep-pro --custom-binary /root/semgrep-core-proprietary

semgrep --config "p/default-v2" . --pro

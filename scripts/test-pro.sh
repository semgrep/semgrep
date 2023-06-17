#! /usr/bin/env bash

set -e

cp /root/semgrep-core-proprietary /usr/local/bin/semgrep-core-proprietary

# We need to give it executable permissions or it won't run
chmod +x /usr/local/bin/semgrep-core-proprietary

# Relocate to the directory, because otherwise some weirdness happens and
# we run with all the rules, taking ~30 minutes.
cd /root || exit

semgrep --config "p/default-v2" . --pro

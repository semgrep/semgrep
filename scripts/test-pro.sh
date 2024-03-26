#! /usr/bin/env sh
# This script is called from the test-semgrep-pro.jsonnet workflow.
# We now use 'sh' and not 'bash' above because we removed bash
# from the returntocorp/semgrep docker image to remain as small
# as possible (and to avoid CVEs on programs we actually don't really
# need in the image).

set -e

# Relocate to the directory, because otherwise some weirdness happens and
# we run with all the rules, taking ~30 minutes.
cd /root || exit

semgrep install-semgrep-pro --custom-binary /root/semgrep-core-proprietary

semgrep --config "p/default-v2" . --pro

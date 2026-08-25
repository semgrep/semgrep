#!/usr/bin/env bash
# Regression: jsonnet_to_yaml.sh must rewrite annotated uses for both quote styles.
set -euo pipefail

rewrite() {
  sed -E "s/^( *-? *)uses: '([^']+) # ([^']+)'$/\\1uses: \\2 # \\3/" \
  | sed -E 's/^( *-? *)uses: "([^"]+) # ([^"]+)"$/\1uses: \2 # \3/'
}

got=$(printf '%s\n' \
  "  - uses: 'org/action@sha # v1.2.3'" \
  '  - uses: "org/action@sha # v1.2.3"' \
  | rewrite)
want=$(printf '%s\n' \
  '  - uses: org/action@sha # v1.2.3' \
  '  - uses: org/action@sha # v1.2.3')

if [[ "$got" != "$want" ]]; then
  printf 'FAIL:\n got:\n%s\n want:\n%s\n' "$got" "$want" >&2
  exit 1
fi
echo OK

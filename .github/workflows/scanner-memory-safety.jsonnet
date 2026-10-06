// Runs vendored tree-sitter external scanners under AddressSanitizer
// See libs/ocaml-tree-sitter-semgrep/scripts/test_scanner_memory_safety.py.
//
// The test is skipped when there are no scanner files changed.

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local uses = import 'libs/uses.libsonnet';

// Relative to the OSS workflow tree. Pro prefixes these with OSS/.
local workflow_paths = [
  '.github/workflows/scanner-memory-safety.yml',
  '.github/workflows/scanner-memory-safety.jsonnet',
];

// Files the test compiles or reads, relative to the OSS root.
// coupling: the scanners and headers used by test_scanner_memory_safety.py.
local scanner_paths = [
  'languages/*/tree-sitter/semgrep-*/lib/scanner.*',
  'languages/*/tree-sitter/semgrep-*/lib/tree_sitter/**',
  'libs/ocaml-tree-sitter-semgrep/lang/semgrep-grammars/src/semgrep-ruby/src/scanner.c',
  'libs/ocaml-tree-sitter-semgrep/scripts/test_scanner_memory_safety.py',
];

// oss_root: '' in the OSS repo, 'OSS/' in semgrep-proprietary.
// extra_paths: more paths that should run the test (e.g. Pro's own yml).
local job(oss_root, extra_paths=[]) =
  local pathspecs = [
    "':(top,glob)%s'" % p
    for p in [oss_root + p for p in scanner_paths + workflow_paths] + extra_paths
  ];
  {
    'runs-on': 'ubuntu-latest',
    'timeout-minutes': 15,
    steps: [
      {
        uses: uses.actions.checkout,
        // HEAD^ must exist for the diff below.
        with: { 'fetch-depth': 2 },
      },
      {
        name: 'Check whether scanner files changed',
        id: 'changes',
        env: { EVENT_NAME: '${{ github.event_name }}' },
        run: |||
          set -euo pipefail
          if [ "$EVENT_NAME" != pull_request ]; then
            echo "Not a pull request; running the test"
            echo "scanners=true" >> "$GITHUB_OUTPUT"
            exit 0
          fi
          # On pull_request, checkout's HEAD is GitHub's merge commit, so
          # HEAD^ is the tip of the base branch.
          changed=$(git diff --no-renames --name-only HEAD^ HEAD -- %s)
          if [ -n "$changed" ]; then
            printf 'Scanner files changed:\n%%s\n' "$changed"
            echo "scanners=true" >> "$GITHUB_OUTPUT"
          else
            echo "No scanner files changed; skipping the test"
            echo "scanners=false" >> "$GITHUB_OUTPUT"
          fi
        ||| % std.join(' ', pathspecs),
      },
      actions.setup_python_step(semgrep.python_version) + {
        'if': "steps.changes.outputs.scanners == 'true'",
      },
      {
        name: 'Run scanners under AddressSanitizer',
        'if': "steps.changes.outputs.scanners == 'true'",
        'working-directory': if oss_root == '' then '.' else oss_root,
        // Same locked environment as `make ots-test-python`.
        run: 'uv run --project cli --locked pytest -v libs/ocaml-tree-sitter-semgrep/scripts/test_scanner_memory_safety.py',
      },
    ],
  };

{
  name: 'Scanner memory safety',
  on: {
    pull_request: {},
    push: { branches: ['develop'] },
  },
  jobs: {
    'scanner-memory-safety': job(''),
  },
  export:: {
    // reused in semgrep-pro
    job: job,
  },
}

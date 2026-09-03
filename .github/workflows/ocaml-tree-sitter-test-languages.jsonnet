// Per-language grammar build/test matrix for ocaml-tree-sitter-semgrep.
// For each language from lang/scripts/list-languages, runs `./test-lang <lang>`.
// All failures are fatal.

local gha = import 'libs/gha.libsonnet';
local uses = import 'libs/uses.libsonnet';
local lib = import 'ocaml-tree-sitter.libsonnet';

local workflow_paths = [
  '.github/workflows/ocaml-tree-sitter-test-languages.yml',
  '.github/workflows/ocaml-tree-sitter-test-languages.jsonnet',
  '.github/workflows/ocaml-tree-sitter.libsonnet',
];

local trigger_paths(ots_dir, ots_is_submodule=true, extra_paths=[]) =
  // TEMPORARY default — see ocaml-tree-sitter.libsonnet.
  local ots = lib.for_tree(ots_dir, ots_is_submodule);
  std.set(
    ots.core_paths
    + ots.grammar_paths
    + workflow_paths
    + extra_paths
    // TEMPORARY: URL/path edits in .gitmodules do not touch the gitlink.
    + (if ots_is_submodule then ['.gitmodules'] else [])
  );

// GitHub `foo/**` → git directory pathspec `foo`.
local git_pathspec(pat) =
  local p =
    if std.endsWith(pat, '/**') then
      std.substr(pat, 0, std.length(pat) - 3)
    else
      pat;
  "'%s'" % p;

local detect_changes_job(paths) = {
  name: 'Detect grammar or core changes',
  'runs-on': 'ubuntu-latest',
  'timeout-minutes': 5,
  permissions: gha.read_permissions,
  outputs: {
    should_test: '${{ steps.filter.outputs.should_test }}',
  },
  // fetch-depth 2: PR merge commit + first parent (the base). HEAD^ is base.
  steps: [
    {
      uses: uses.actions.checkout,
      with: { 'fetch-depth': 2 },
    },
    {
      id: 'filter',
      name: 'Filter changed paths',
      env: {
        EVENT_NAME: '${{ github.event_name }}',
      },
      run: |||
        set -euo pipefail
        # Push / workflow_dispatch: this workflow's on.push.paths already
        # narrowed the run; always test.
        if [ "$EVENT_NAME" != "pull_request" ]; then
          echo "should_test=true" >> "$GITHUB_OUTPUT"
          exit 0
        fi
        # --quiet exits 1 when any of these paths changed; 0 when none did.
        if git diff --quiet --name-only HEAD^ HEAD -- %(pathspecs)s; then
          echo "should_test=false" >> "$GITHUB_OUTPUT"
        else
          echo "should_test=true" >> "$GITHUB_OUTPUT"
        fi
      ||| % { pathspecs: std.join(' ', std.map(git_pathspec, paths)) },
    },
  ],
};

local enumerate_job(ots) = {
  name: 'Enumerate languages',
  needs: 'detect-changes',
  'if': "${{ needs.detect-changes.outputs.should_test == 'true' }}",
  'runs-on': 'ubuntu-latest',
  'timeout-minutes': 5,
  outputs: {
    languages: '${{ steps.list.outputs.languages }}',
  },
  steps: ots.checkout_steps + [
    {
      id: 'list',
      'working-directory': ots.ots_dir,
      run: |||
        set -euo pipefail
        echo "languages=$(lang/scripts/list-languages | jq -Rnc '[inputs]')" >> "$GITHUB_OUTPUT"
      |||,
    },
  ],
};

local test_language_job(ots) = {
  name: 'test-lang ${{ matrix.language }}',
  needs: 'enumerate',
  // c-sharp, c-sharp-pro, and hack OOM or get preempted on standard ~16GB
  // runners during 'tree-sitter generate' / 'make gen-c', so they get an org
  // larger runner. Label is singular: ubuntu-latest-8-core.
  'runs-on': "${{ (matrix.language == 'c-sharp-pro' || matrix.language == 'c-sharp' || matrix.language == 'hack') && fromJSON('{\"group\":\"Default Larger Runners\",\"labels\":\"ubuntu-latest-8-core\"}') || 'ubuntu-latest' }}",
  'timeout-minutes': 60,
  strategy: {
    'fail-fast': false,
    'max-parallel': 8,
    matrix: {
      language: '${{ fromJSON(needs.enumerate.outputs.languages) }}',
    },
  },
  steps:
    ots.checkout_steps
    + [
      ots.setup_ocaml_step,
    ]
    + ots.build_core_steps
    + [
      {
        name: 'test-lang ${{ matrix.language }}',
        'working-directory': ots.ots_dir,
        run: |||
          set -euo pipefail
          eval "$(opam env)"
          cd lang
          ./test-lang "${{ matrix.language }}"
        |||,
      },
    ],
};

// Single required check for branch protection (matrix expands to many jobs).
// Skipped matrix (no core/lang changes) is success; cancelled runs must not pass.
local test_languages_job = {
  name: 'Test Grammars',
  needs: ['detect-changes', 'test-language'],
  'if': 'always() && !cancelled()',
  'runs-on': 'ubuntu-latest',
  steps: [
    {
      name: 'All languages passed',
      env: {
        DETECT: '${{ needs.detect-changes.result }}',
        SHOULD_TEST: '${{ needs.detect-changes.outputs.should_test }}',
        RESULT: '${{ needs.test-language.result }}',
      },
      run: |||
        set -euo pipefail
        echo "detect-changes=$DETECT should_test=$SHOULD_TEST matrix=$RESULT"
        if [ "$DETECT" != "success" ]; then
          exit 1
        fi
        if [ "$SHOULD_TEST" != "true" ]; then
          echo "No grammar, core, or workflow changes; skipping"
          exit 0
        fi
        test "$RESULT" = "success"
      |||,
    },
  ],
};

local jobs(ots_dir, ots_is_submodule=true, extra_paths=[]) =
  // TEMPORARY default — see ocaml-tree-sitter.libsonnet.
  local ots = lib.for_tree(ots_dir, ots_is_submodule);
  {
    'detect-changes': detect_changes_job(trigger_paths(ots_dir, ots_is_submodule, extra_paths)),
    enumerate: enumerate_job(ots),
    'test-language': test_language_job(ots),
    'test-languages': test_languages_job,
  };

local concurrency = {
  group: '${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}',
  'cancel-in-progress': true,
};

local ots_dir = 'libs/ocaml-tree-sitter-semgrep';

{
  name: 'Test Grammars in ocaml-tree-sitter-semgrep',
  on: {
    // No path filter: a required check that never runs stays pending.
    pull_request: {},
    push: {
      branches: ['develop'],
      paths: trigger_paths(ots_dir),
    },
    workflow_dispatch: null,
  },
  // One matrix run per ref; newer pushes cancel the previous (saves minutes).
  concurrency: concurrency,
  jobs: jobs(ots_dir),
  export:: {
    // reused in semgrep-pro
    trigger_paths: trigger_paths,
    jobs: jobs,
    concurrency: concurrency,
    workflow_paths: workflow_paths,
  },
}

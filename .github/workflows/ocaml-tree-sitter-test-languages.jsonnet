// Per-language grammar build/test matrix for ocaml-tree-sitter-semgrep.
// For each language from scripts/list-languages, runs `./test-lang <lang>`.
// All failures are fatal.

local uses = import 'libs/uses.libsonnet';
local lib = import 'ocaml-tree-sitter.libsonnet';

local workflow_paths = [
  '.github/workflows/ocaml-tree-sitter-test-languages.yml',
  '.github/workflows/ocaml-tree-sitter-test-languages.jsonnet',
  '.github/workflows/ocaml-tree-sitter.libsonnet',
];

// Partition trigger paths by their effect on the test matrix.
local trigger_path_groups(ots_dir, extra_paths=[]) =
  local ots = lib.for_tree(ots_dir);
  {
    grammar: ots.language_paths,
    all_languages:
      std.set(
        ots.core_paths
        + ots.grammar_tool_paths
        + ots.integration_paths
        + [ots_dir + '/Makefile', ots_dir + '/dune']
        + workflow_paths
        + extra_paths
      ),
    ignored: [
      '!' + ots_dir + '/core/doc/**',
      '!' + ots_dir + '/core/test/**',
      '!' + ots_dir + '/core/README.md',
      '!' + ots_dir + '/scripts/test_*.py',
    ],
  };

local trigger_paths(ots_dir, extra_paths=[]) =
  local paths = trigger_path_groups(ots_dir, extra_paths);
  std.set(paths.grammar + paths.all_languages) + paths.ignored;

// Preserve GitHub's repository-rooted glob semantics in Git.
local git_pathspec(pat) =
  local excluded = std.startsWith(pat, '!');
  local glob = if excluded then std.substr(pat, 1, std.length(pat) - 1) else pat;
  local magic = if excluded then ':(top,exclude,glob)' else ':(top,glob)';
  "'%s%s'" % [magic, glob];

// Manual runs. auto diffs the branch against the default branch; all skips
// the diff. Pull requests and pushes ignore this input.
local dispatch = {
  inputs: {
    mode: {
      description: 'auto: grammars affected since the default branch. all: every grammar.',
      required: true,
      type: 'choice',
      options: ['auto', 'all'],
      default: 'auto',
    },
  },
};

local on_push_or_pull_request = "${{ github.event_name != 'workflow_dispatch' }}";
local on_workflow_dispatch = "${{ github.event_name == 'workflow_dispatch' }}";

local checkout_step(fetch_depth, when) = {
  'if': when,
  uses: uses.actions.checkout,
  with: {
    'fetch-depth': fetch_depth,
  },
};

// Shared shell functions. The caller passes a base that contains the registry.
local selection_script(paths) = |||
  set -euo pipefail

  write_all_languages_and_exit() {
    echo "$1"
    {
      printf "languages="
      "$OTS_DIR/scripts/list-languages" --json
    } >> "$GITHUB_OUTPUT"
    exit 0
  }

  # Caller responsible to ensure the registry exists.
  select_languages_changed_since() {
    local comparison_base="$1"
    local all_language_changes
    all_language_changes=$(
      git diff --no-renames --name-only "$comparison_base" HEAD -- %(all_language_pathspecs)s
    )
    if [ -n "$all_language_changes" ]; then
      printf 'Full-matrix trigger paths changed:\n%%s\n' "$all_language_changes"
      write_all_languages_and_exit 'Testing every grammar'
    fi

    # The runner's filesystem is ephemeral; an EXIT trap would outlive this function.
    base_registry=$(mktemp)
    git show "$comparison_base:$OTS_DIR/lang/upstream-grammars.json" \
      > "$base_registry"
    git diff --no-renames --name-only "$comparison_base" HEAD -- %(grammar_pathspecs)s \
      | sed "s|^$OTS_DIR/||" \
      | "$OTS_DIR/scripts/select_grammar_tests.py" "$base_registry" \
      >> "$GITHUB_OUTPUT"
  }
||| % {
  grammar_pathspecs: std.join(' ', std.map(git_pathspec, paths.grammar)),
  all_language_pathspecs: std.join(
    ' ',
    std.map(git_pathspec, paths.all_languages + paths.ignored),
  ),
};

// Pull requests use HEAD^; pushes use github.event.before when that commit exists.
local select_languages_for_push_or_pull_request(ots_dir, paths) = [
  checkout_step('2', on_push_or_pull_request),
  {
    id: 'push_or_pull_request',
    'if': on_push_or_pull_request,
    name: 'Select languages for push or pull request',
    env: {
      EVENT_NAME: '${{ github.event_name }}',
      BEFORE_SHA: '${{ github.event.before }}',
      OTS_DIR: ots_dir,
    },
    run: selection_script(paths) + |||

      # No diff base, or the registry did not exist yet: test every language.
      comparison_base=
      if [ "$EVENT_NAME" = "pull_request" ]; then
        comparison_base='HEAD^'
      else
        before="$BEFORE_SHA"
        # Branch creation has an all-zero before SHA and no diff base.
        if [[ -n "$before" && ! "$before" =~ ^0+$ ]]; then
          # A multi-commit push may put before outside checkout's depth.
          if git cat-file -e "$before^{commit}" 2>/dev/null \
            || git fetch --no-tags --depth=1 origin "$before"; then
            comparison_base="$before"
          fi
        fi
      fi
      if [ -z "$comparison_base" ]; then
        write_all_languages_and_exit 'No comparison commit; testing every grammar'
      fi
      if ! git show "$comparison_base:$OTS_DIR/lang/upstream-grammars.json" \
        >/dev/null; then
        write_all_languages_and_exit 'No base registry; testing every grammar'
      fi
      select_languages_changed_since "$comparison_base"
    |||,
  },
];

// auto diffs against the default branch; all tests every grammar.
local select_languages_for_workflow_dispatch(ots_dir, paths) = [
  // '0' is the full history, and only for auto: merge-base needs it.
  // A non-empty string is truthy, so this does not collapse to 2.
  checkout_step(
    "${{ github.event.inputs.mode == 'auto' && '0' || '2' }}",
    on_workflow_dispatch,
  ),
  {
    id: 'workflow_dispatch',
    'if': on_workflow_dispatch,
    name: 'Select languages for manual run',
    env: {
      DISPATCH_MODE: '${{ github.event.inputs.mode }}',
      DEFAULT_BRANCH: '${{ github.event.repository.default_branch }}',
      OTS_DIR: ots_dir,
    },
    run: selection_script(paths) + |||

      dispatch_mode="${DISPATCH_MODE:-auto}"
      case "$dispatch_mode" in
        all)
          write_all_languages_and_exit 'Manual mode all; testing every grammar'
          ;;
        auto)
          git fetch --no-tags origin "$DEFAULT_BRANCH"
          if ! comparison_base=$(git merge-base HEAD "origin/$DEFAULT_BRANCH"); then
            echo "error: no merge base with $DEFAULT_BRANCH; rerun with mode all" >&2
            exit 1
          fi
          # The registry did not exist yet: test every language.
          if ! git show "$comparison_base:$OTS_DIR/lang/upstream-grammars.json" \
            >/dev/null; then
            write_all_languages_and_exit 'No base registry; testing every grammar'
          fi
          select_languages_changed_since "$comparison_base"
          ;;
        *)
          echo "error: unknown mode '$dispatch_mode' (expected auto or all)" >&2
          exit 1
          ;;
      esac
    |||,
  },
];

local detect_changes_job(steps) = {
  name: 'Detect grammar or core changes',
  'runs-on': 'ubuntu-latest',
  'timeout-minutes': 5,
  permissions: { contents: 'read' },
  outputs: {
    languages: '${{ steps.push_or_pull_request.outputs.languages || steps.workflow_dispatch.outputs.languages }}',
  },
  steps: steps,
};

local test_language_job(ots) = {
  name: 'test-lang ${{ matrix.language }}',
  needs: 'detect-changes',
  'if': "${{ needs.detect-changes.outputs.languages != '[]' }}",
  // c-sharp, c-sharp-pro, and hack OOM or get preempted on standard ~16GB
  // runners during 'tree-sitter generate' / 'make gen-c', so they get an org
  // larger runner. Label is singular: ubuntu-latest-8-core.
  'runs-on': "${{ (matrix.language == 'c-sharp-pro' || matrix.language == 'c-sharp' || matrix.language == 'hack') && fromJSON('{\"group\":\"Default Larger Runners\",\"labels\":\"ubuntu-latest-8-core\"}') || 'ubuntu-latest' }}",
  'timeout-minutes': 60,
  strategy: {
    'fail-fast': false,
    'max-parallel': 8,
    matrix: {
      language: '${{ fromJSON(needs.detect-changes.outputs.languages) }}',
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
        LANGUAGES: '${{ needs.detect-changes.outputs.languages }}',
        RESULT: '${{ needs.test-language.result }}',
      },
      run: |||
        set -euo pipefail
        echo "detect-changes=$DETECT languages=$LANGUAGES matrix=$RESULT"
        if [ "$DETECT" != "success" ]; then
          exit 1
        fi
        if [ "$LANGUAGES" = "[]" ]; then
          echo "No grammars selected; skipping"
          exit 0
        fi
        test "$RESULT" = "success"
      |||,
    },
  ],
};

local jobs(ots_dir, extra_paths=[]) =
  local ots = lib.for_tree(ots_dir);
  local paths = trigger_path_groups(ots_dir, extra_paths);
  {
    'detect-changes': detect_changes_job(
      select_languages_for_push_or_pull_request(ots_dir, paths)
      + select_languages_for_workflow_dispatch(ots_dir, paths)
    ),
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
    workflow_dispatch: dispatch,
  },
  // One matrix run per ref; newer pushes cancel the previous (saves minutes).
  concurrency: concurrency,
  jobs: jobs(ots_dir),
  export:: {
    trigger_path_groups: trigger_path_groups,
    trigger_paths: trigger_paths,
    git_pathspec: git_pathspec,
    jobs: jobs,
    concurrency: concurrency,
    workflow_paths: workflow_paths,
    dispatch: dispatch,
  },
}

// Shared paths and steps for ocaml-tree-sitter-semgrep GHA workflows.
// Parameterized by ots_dir for reuse from semgrep-proprietary.

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local uses = import 'libs/uses.libsonnet';

// restore/save share the actions/cache pin; save only on a miss.
local cache_restore = std.strReplace(uses.actions.cache, '/cache@', '/cache/restore@');
local cache_save = std.strReplace(uses.actions.cache, '/cache@', '/cache/save@');

local for_tree(ots_dir) =
  local root = std.substr(ots_dir, 0, std.length(ots_dir) - std.length('libs/ocaml-tree-sitter-semgrep'));
  local build_paths = ['Makefile', 'dune-project', 'dune-workspace', '*.opam', 'opam-lockfiles/*.locked'];
  local integration_paths = std.set(build_paths + [root + p for p in build_paths + [
    'tree-sitter-config.sh',
    'tree-sitter-config.mk',
    'cli/pyproject.toml',
    'cli/uv.lock',
    '.github/workflows/libs/actions.libsonnet',
    '.github/workflows/libs/semgrep.libsonnet',
    '.github/workflows/libs/uses.libsonnet',
  ]]);
  local core_dir = ots_dir + '/core';
  local lang_dir = ots_dir + '/lang';
  local language_paths = [lang_dir + '/**'];
  local grammar_tool_paths = [ots_dir + '/scripts/**'];

  local ts_cache_path = core_dir + '/tree-sitter-*.*.*';
  // Include the tree SHA so provisioning changes invalidate the CLI cache.
  // Versions come from scripts/ts-versions (upstream-grammars.json pins).
  local ts_cache_key = 'tree-sitter-${{ runner.os }}-${{ steps.ts-versions.outputs.versions }}-${{ steps.cache-id.outputs.sha }}';

  local cache_id_step = {
    id: 'cache-id',
    run: |||
      echo "sha=$(git rev-parse HEAD:%s)" >> "$GITHUB_OUTPUT"
      echo "ocaml=$(opam exec -- ocamlc -version)" >> "$GITHUB_OUTPUT"
    ||| % core_dir,
  };

  // Run from source so tools install where grammar builds and caches expect.
  local provision_tree_sitter_steps = [
    {
      id: 'ts-versions',
      name: 'Resolve pinned tree-sitter versions',
      'working-directory': ots_dir,
      run: |||
        echo "versions=$(./scripts/ts-versions | tr '\n' '-')" >> "$GITHUB_OUTPUT"
      |||,
    },
    {
      id: 'ts-cache',
      uses: cache_restore,
      with: {
        // Semver installs only (tree-sitter-0.x.y) — not tree-sitter-out/repo.
        path: ts_cache_path,
        key: ts_cache_key,
      },
    },
    {
      name: 'Setup tree-sitter versions',
      'working-directory': ots_dir,
      run: './core/scripts/provision-tree-sitter-all',
    },
    {
      'if': "steps.ts-cache.outputs.cache-hit != 'true'",
      uses: cache_save,
      with: {
        path: ts_cache_path,
        key: ts_cache_key,
      },
    },
  ];

  // Reuse compiled workspace artifacts across grammar jobs; opam caches installed dependencies.
  local core_cache_paths = std.join('\n', ['_build', core_dir + '/bin']);
  local core_cache_key = 'grammar-core-v3-tools-${{ runner.os }}-${{ steps.cache-id.outputs.ocaml }}-${{ steps.cache-id.outputs.sha }}-${{ hashFiles(' + std.join(', ', ["'" + p + "'" for p in integration_paths]) + ') }}';

  local save_core_cache(suffix='') = {
    'if': "steps.core-cache.outputs.cache-hit != 'true'",
    uses: cache_save,
    with: {
      path: core_cache_paths,
      key: core_cache_key + suffix,
    },
  };

  local restore_core_cache(suffix='') = {
    id: 'core-cache',
    uses: cache_restore,
    with: {
      // _build for dune; bin/ for the promoted ocaml-tree-sitter binary
      // (lang/ looks at core/bin, not _build).
      path: core_cache_paths,
      key: core_cache_key + suffix,
    },
  };

  local install_deps_step = {
    name: 'Install grammar dependencies',
    'working-directory': if root == '' then '.' else std.rstripChars(root, '/'),
    run: 'opam exec -- make install-grammar-deps',
  };

  local build_install_core_steps = [
    restore_core_cache(),
    install_deps_step,
    {
      name: 'Build grammar tools',
      'working-directory': ots_dir + '/../..',
      run: 'opam exec -- make grammar-tools',
    },
    save_core_cache(),
  ];

  local build_core_steps =
    [cache_id_step]
    + build_install_core_steps
    + provision_tree_sitter_steps;

  {
    restore_core_cache: restore_core_cache,
    save_core_cache: save_core_cache,
    integration_paths: integration_paths,
    ots_dir: ots_dir,

    core_paths: [core_dir + '/**'],

    // lang/ is the grammar sources + test-lang harness; scripts/ holds the
    // registry / version / ABI tooling those builds invoke.
    language_paths: language_paths,
    grammar_tool_paths: grammar_tool_paths,
    grammar_paths: language_paths + grammar_tool_paths,

    // Grammar jobs install only the generator dependencies.
    setup_ocaml_step: semgrep.opam_setup(cache_profile='grammar'),

    // lfs=true: every caller builds the tree, and lib/parser.c is
    // LFS-tracked, so plain `git diff`/`git status` need real content.
    checkout_steps: actions.checkout(lfs=true),

    cache_id_step: cache_id_step,
    provision_tree_sitter_steps: provision_tree_sitter_steps,
    install_deps_step: install_deps_step,
    build_core_steps: build_core_steps,
  };

{
  for_tree:: for_tree,
}

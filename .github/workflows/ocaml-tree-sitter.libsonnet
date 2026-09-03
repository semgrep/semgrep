// Shared paths and steps for ocaml-tree-sitter-semgrep GHA workflows.
// Parameterized by ots_dir for reuse from semgrep-proprietary.
//
// TEMPORARY: ots_is_submodule — flip callers to false once ots_dir is not a
// git module, then delete submodule-only branches. While true:
// 1. path filters include bare gitlink (bump ≠ `<path>/**`);
// 2. checkout inits only public ots (+ nested grammar gitlinks);
// 3. cache key is HEAD:<ots_dir> (HEAD:<ots_dir>/core does not resolve).

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local uses = import 'libs/uses.libsonnet';

// restore/save share the actions/cache pin; save only on a miss.
local cache_restore = std.strReplace(uses.actions.cache, '/cache@', '/cache/restore@');
local cache_save = std.strReplace(uses.actions.cache, '/cache@', '/cache/save@');

local for_tree(ots_dir, ots_is_submodule=true) =
  local core_dir = ots_dir + '/core';
  local lang_dir = ots_dir + '/lang';

  // TEMPORARY: drop once ots is in-tree.
  local checkout_ots_submodule_step = {
    name: 'Checkout ocaml-tree-sitter-semgrep submodule',
    // Job-level working-directory would make this pathspec relative to ots_dir.
    'working-directory': '${{ github.workspace }}',
    run: 'git submodule update --init --depth 1 ' + ots_dir,
  };

  // gosu and requirements are public, but OTS .gitmodules pins SSH URLs
  // (git@github.com:...). GHA runners have no GitHub SSH key, so clone
  // fails with "Permission denied (publickey)". Rewrite to HTTPS.
  local rewrite_ssh_step = {
    name: 'Rewrite SSH grammar URLs to HTTPS',
    run: 'git config --global url."https://github.com/".insteadOf "git@github.com:"',
  };

  // Nested tree-sitter-<lang> gitlinks under lang/semgrep-grammars/src/.
  // --recursive --depth 1 is a shallow clone of those gitlinks (and unused
  // nested copies: bash-it, dart/hcl fuzz tree-sitter). After ots is in-tree,
  // skip this: there is no git repo at ots_dir to recurse.
  local provision_grammars_step = {
    name: 'Provision the upstream grammar submodules',
    'working-directory': ots_dir,
    run: 'git submodule update --init --recursive --depth 1 --jobs 8',
  };

  // TEMPORARY: gitlink SHA; after vendoring use core/ (lang/ churn ≠ bust).
  local core_tree_rev = if ots_is_submodule then ots_dir else core_dir;

  local ts_cache_path = core_dir + '/tree-sitter-*.*.*';
  // Include the tree SHA: provision-tree-sitter skips a complete install dir, so
  // a script/patch/checksum change with the same version numbers must miss.
  local ts_cache_key = 'tree-sitter-${{ runner.os }}-${{ steps.ts-versions.outputs.versions }}-${{ steps.cache-id.outputs.sha }}';

  local cache_id_step = {
    id: 'cache-id',
    run: |||
      echo "sha=$(git rev-parse HEAD:%s)" >> "$GITHUB_OUTPUT"
      echo "ocaml=$(opam exec -- ocamlc -version)" >> "$GITHUB_OUTPUT"
    ||| % core_tree_rev,
  };

  local provision_tree_sitter_steps = [
    {
      id: 'ts-versions',
      'working-directory': ots_dir,
      run: |||
        echo "versions=$(./lang/scripts/ts-versions | tr '\n' '-')" >> "$GITHUB_OUTPUT"
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

  local core_cache_paths = std.join('\n', [core_dir + '/_build', core_dir + '/bin']);
  local core_cache_key = 'core-${{ runner.os }}-${{ steps.cache-id.outputs.ocaml }}-${{ steps.cache-id.outputs.sha }}';

  local build_install_core_steps = [
    {
      id: 'core-cache',
      uses: cache_restore,
      with: {
        // _build for dune; bin/ for the promoted ocaml-tree-sitter binary
        // (lang/ looks at core/bin, not _build).
        path: core_cache_paths,
        key: core_cache_key,
      },
    },
    {
      name: 'Setup',
      'working-directory': ots_dir,
      run: 'opam exec -- make setup',
    },
    {
      name: 'Install',
      'working-directory': ots_dir,
      run: 'opam exec -- make install',
    },
    {
      'if': "steps.core-cache.outputs.cache-hit != 'true'",
      uses: cache_save,
      with: {
        path: core_cache_paths,
        key: core_cache_key,
      },
    },
  ];

  local build_core_steps =
    (if ots_is_submodule then [rewrite_ssh_step, provision_grammars_step] else [])
    + [cache_id_step]
    + provision_tree_sitter_steps
    + build_install_core_steps;

  {
    ots_dir: ots_dir,
    core_dir: core_dir,
    ots_is_submodule: ots_is_submodule,

    // TEMPORARY: same bare-gitlink rule for core/ triggers.
    core_paths:
      [core_dir + '/**']
      + (if ots_is_submodule then [ots_dir] else [core_dir]),

    // lang/ is the grammar sources + test-lang harness.
    // TEMPORARY: gitlink bump can be core or lang; parent diffs cannot tell.
    grammar_paths:
      [lang_dir + '/**']
      + (if ots_is_submodule then [ots_dir] else [lang_dir]),

    // core/ has its own opam files; hash those, not the top-level lockfiles.
    setup_ocaml_step: {
      uses: uses.semgrep.setup_ocaml,
      with: {
        'cache-prefix': "v5-${{ hashFiles('%s/*.opam') }}" % core_dir,
        'ocaml-compiler': semgrep.opam_switch,
        'opam-pin': false,
        'save-opam-post-run': true,
      },
    },

    // TEMPORARY: public ots only — full submodule checkout hits private deps.
    checkout_steps:
      actions.checkout()
      + (if ots_is_submodule then [checkout_ots_submodule_step] else []),

    build_core_steps: build_core_steps,
  };

{
  for_tree:: for_tree,
}

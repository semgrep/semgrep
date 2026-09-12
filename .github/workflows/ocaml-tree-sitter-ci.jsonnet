// Builds the ocaml-tree-sitter code generator and runs its unit tests
// Uses the Semgrep root ots-test-ocaml and ots-test-python targets.
// Reusable from semgrep-proprietary.
//
// Python tests stay in this workflow (not main build-test): they need
// per-pin tree-sitter CLIs under
// core/tree-sitter-<v>/ beyond ordinary Semgrep `make setup`. See the
// README "Python / ABI tests" section.

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local lib = import 'ocaml-tree-sitter.libsonnet';

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// Relative to the OSS workflow tree. Pro prefixes these with OSS/.
local workflow_paths = [
  '.github/workflows/ocaml-tree-sitter-ci.yml',
  '.github/workflows/ocaml-tree-sitter-ci.jsonnet',
  '.github/workflows/ocaml-tree-sitter.libsonnet',
];

local trigger_paths(ots_dir, extra_paths=[]) =
  std.set(
    [
      // Whole OTS tree: OCaml core, Python scripts, lang registry, Makefile.
      ots_dir + '/**',
    ]
    + lib.for_tree(ots_dir).integration_paths
    + workflow_paths
    + extra_paths
  );

local build_job(ots_dir) =
  local ots = lib.for_tree(ots_dir);
  {
    'runs-on': 'ubuntu-latest',
    steps: actions.checkout() + [
             // Same compiler / lockfile cache / pinned opam-repository as main CI.
             semgrep.opam_setup(),
             // Same uv + Python helper as main Semgrep CI (python_version 3.12).
             actions.setup_python_step(semgrep.python_version),
             // tree-sitter's CLI is built from source with cargo.
             // libclang is needed for its Rust bindgen step.
             // m4, pkg-config and cargo are built-into the image
             {
               name: 'Set up build tools',
               run: |||
                 sudo apt-get update
                 sudo apt-get install -y libclang-dev
               |||,
             },
             ots.cache_id_step,
             ots.restore_core_cache('-tests'),
             {
               name: 'Install repository dependencies',
               'working-directory': ots_dir + '/../..',
               run: 'opam exec -- make install-deps',
             },
             {
               name: 'Build grammar tools',
               'working-directory': ots_dir + '/../..',
               run: 'opam exec -- make grammar-tools',
             },
             {
               name: 'Test (OCaml)',
               'working-directory': ots_dir + '/../..',
               run: 'opam exec -- make ots-test-ocaml',
             },
             ots.save_core_cache('-tests'),
           ]
           // Cached CLI provisioning → core/tree-sitter-<v>/bin
           // (same layout Python ABI/version tests resolve).
           + ots.provision_tree_sitter_steps
           + [
             {
               name: 'Test (Python)',
               'working-directory': ots_dir + '/../..',
               // Reuse the CLI development environment and lockfile.
               run: 'make ots-test-python',
             },
           ],
  };

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

local ots_dir = 'libs/ocaml-tree-sitter-semgrep';

{
  name: 'Build and test ocaml tree sitter core',
  on: {
    pull_request: { paths: trigger_paths(ots_dir) },
    push: {
      branches: ['develop'],
      paths: trigger_paths(ots_dir),
    },
  },
  jobs: {
    build: build_job(ots_dir),
  },
  export:: {
    // reused in semgrep-pro
    build: build_job,
    trigger_paths: trigger_paths,
    workflow_paths: workflow_paths,
  },
}

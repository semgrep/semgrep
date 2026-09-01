// Builds the ocaml-tree-sitter code generator and runs its OCaml test suite.
// Reusable from semgrep-proprietary

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local uses = import 'libs/uses.libsonnet';

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local trigger_paths(ots_dir) =
  local core_dir = ots_dir + '/core';
  [
    core_dir + '/**',
    core_dir,
    // The gitlink itself: bumping the pinned submodule commit only changes
    // this single path, not anything under core/.
    ots_dir,
    '.github/workflows/ocaml-tree-sitter-ci.yml',
  ];

local build_job(ots_dir) =
  local core_dir = ots_dir + '/core';
  {
    'runs-on': 'ubuntu-latest',
    // Only init the public ots submodule
    steps: actions.checkout() + [
      {
        name: 'Checkout ocaml-tree-sitter-semgrep submodule',
        run: 'git submodule update --init --depth 1 %s' % ots_dir,
      },
      // This core/ build has its own opam files, entirely separate from the
      // main semgrep switch, so it keys its cache off core/*.opam rather
      // than semgrep.opam_setup's default (which hashes the top-level
      // opam-lockfiles and would never invalidate when core/'s deps change).
      {
        uses: uses.semgrep.setup_ocaml,
        with: {
          'cache-prefix': "v5-${{ hashFiles('%s/*.opam') }}" % core_dir,
          'ocaml-compiler': semgrep.opam_switch,
          'opam-pin': false,
          'save-opam-post-run': true,
        },
      },
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
        name: 'Test',
        'working-directory': ots_dir,
        run: 'opam exec -- make test',
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
  },
}

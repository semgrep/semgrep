// This workflow builds and test semgrep-core. It also generates an
// ocaml-build-artifacts.tgz file which is used in many other jobs
// such as test-cli in tests.yml or build-wheels-manylinux in
// build-test-manylinux-x86.yaml

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local build_test_core_job =
  // This container has opam already installed, as well as an opam switch 5.0.0
  // already created, and a big set of packages already installed. Thus,
  // the 'make install-deps-ALPINE-for-semgrep-core' below is very fast and
  // almost a noop.
  semgrep.ocaml_alpine_container
  {
    steps: [
      {
        name: 'Make checkout speedy',
        run: 'git config --global fetch.parallel 50',
      },
      actions.checkout_with_submodules(),
      {
        name: 'Build semgrep-core',
        run: |||
          eval $(opam env)
          make install-deps-ALPINE-for-semgrep-core
          make install-deps-for-semgrep-core
          make core
          mkdir -p ocaml-build-artifacts/bin
          cp bin/semgrep-core ocaml-build-artifacts/bin/
          tar czf ocaml-build-artifacts.tgz ocaml-build-artifacts
        |||,
      },
      {
        uses: 'actions/upload-artifact@v3',
        with: {
          path: 'ocaml-build-artifacts.tgz',
          name: 'ocaml-build-artifacts-release',
        },
      },
      {
        name: 'Test semgrep-core',
        run: 'opam exec -- make core-test',
      },
    ],
  };

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  on: {
    workflow_dispatch: null,
    // This is called from tests.yml and release.yml
    // TODO: just make this job a function so no need
    // to use this ugly GHA inherit/workflow_call thing
    workflow_call: null,
  },
  jobs: {
    'build-test-core-x86': build_test_core_job,
  },
}

// This workflow builds and test semgrep-core. It also generates an
// ocaml-build-artifacts.tgz file which is used in many other jobs
// such as test-cli in tests.yml or build-wheels-manylinux in
// build-test-manylinux-x86.yaml

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// exported for other workflows
local artifact_name = 'ocaml-build-artifacts-release';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job(container=semgrep.ocaml_alpine_container, artifact=artifact_name) =
  // This container has opam already installed, as well as an opam switch
  // already created, and a big set of packages already installed. Thus,
  // the 'make install-deps-ALPINE-for-semgrep-core' below is very fast and
  // almost a noop.
  container
  {
    steps: [
      gha.speedy_checkout_step,
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
          name: artifact,
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
  name: 'build-test-core-x86',
  on: {
    workflow_dispatch: null,
    // This is called from tests.yml and release.yml
    // TODO: just make this job a function so no need
    // to use this ugly GHA inherit/workflow_call thing
    workflow_call: null,
  },
  jobs: {
    job: job(),
  },
  // to be reused by other workflows
  export::{
    artifact_name: artifact_name,
    // used by build-test-core-x86-ocaml5.jsonnet
    job: job,
  },
}

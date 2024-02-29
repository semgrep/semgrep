// This workflow builds and test semgrep-core. It also generates an
// ocaml-build-artifacts.tgz file which is used in many other jobs
// such as test-cli in tests.jsonnet or build-wheels-manylinux in
// build-test-manylinux-x86.jsonnet

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// exported for other workflows
local artifact_name = 'ocaml-build-artifacts-release';

local container = semgrep.containers.ocaml_alpine;

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job =
  // This container has opam already installed, as well as an opam switch
  // already created, and a big set of packages already installed. Thus,
  // the 'make install-deps-ALPINE-for-semgrep-core' below is very fast and
  // almost a noop.
  // TODO: switch to setup-ocaml@v2 + GHA cache
  container.job
  {
    steps: [
      gha.speedy_checkout_step,
      actions.checkout_with_submodules(),
      gha.git_safedir,
      semgrep.cache_opam.step(
        key=container.opam_switch + "-${{hashFiles('semgrep.opam')}}"),
      {
        name: 'Install dependencies',
        run: |||
          eval $(opam env)
          make install-deps-ALPINE-for-semgrep-core
          make install-deps-for-semgrep-core
        |||,
      },
      {
        name: 'Build semgrep-core',
        run: 'opam exec -- make core',
      },
      {
        name: 'Make artifact',
        run: |||
          mkdir -p ocaml-build-artifacts/bin
          cp bin/semgrep-core ocaml-build-artifacts/bin/
          tar czf ocaml-build-artifacts.tgz ocaml-build-artifacts
        |||,
      },
      {
        uses: 'actions/upload-artifact@v3',
        with: {
          path: 'ocaml-build-artifacts.tgz',
          name: artifact_name,
        },
      },
      {
        name: 'Test semgrep-core',
        run: 'opam exec -- make core-test',
      }
    ]
  };

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'build-test-core-x86',
  // This is called from tests.jsonnet and release.jsonnet
  // TODO: just make this job a func so no need to use GHA inherit/call
  on: gha.on_dispatch_or_call,
  jobs: {
    job: job,
  },
  // to be reused by other workflows
  export:: {
    artifact_name: artifact_name,
  },
}

// This is the same than build-test-core-x86.jsonnet but using OCaml 5,
// just to make sure Semgrep is ready to be migrated to OCaml 5.
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local core_x86 = import 'build-test-core-x86.jsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job = core_x86.export.job(
  container=semgrep.containers.ocaml5_alpine,
  artifact='ocaml-build-artifacts-ocaml5-release',
  // TODO: some tests are currently failing with OCaml5! but at least
  // we can still check whether it builds
  run_test=false,
);

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'build-test-core-x86-ocaml5',
  // Here we differ from build-test-core-x86.jsonnet by not relying on
  // tests.yml for being included; this is an independent workflow instead.
  on: gha.on_classic,
  jobs: {
    job: job,
  },
}

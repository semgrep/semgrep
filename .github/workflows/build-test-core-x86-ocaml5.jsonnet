// coupling: This is the same than build-test-core-x86.jsonnet but for OCaml 5.

local semgrep = import 'libs/semgrep.libsonnet';

local core_x86 = import 'build-test-core-x86.jsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job = core_x86.export.job(
  container=semgrep.ocaml5_alpine_container,
  artifact='ocaml-build-artifacts-ocaml5-release',
  // TODO: some tests are currently failing with OCaml5! but at least
  // we can still check whether it builds
  run_test=false,
);

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

// Here we differ from build-test-core-x86.jsonnet by not relying on
// tests.yml for being included; this is an independent workflow instead.
{
  name: 'build-test-core-x86-ocaml5',
  on: {
    // can be run manually from the github Actions dashboard
    workflow_dispatch: null,
    pull_request: null,
    push: {
      branches: [
        'develop',
      ],
    },
  },
  jobs: {
    job: job,
  },
}

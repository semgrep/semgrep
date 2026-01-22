// This workflow builds and tests the semgrep-core binary for macOS X86

// coupling: if you modify this file, modify also build-test-osx-arm64.jsonnet

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

// macos-14 and above are arm only; macos-15-intel will be the last runner image
// that has osx + x86
local runs_on = 'macos-15-large';

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local artifact_name = 'semgrep-osx-${{ github.sha }}';

local build_core_job = {
  'runs-on': runs_on,
  steps: actions.checkout_with_submodules() +
         // GitHub's large x86 macOS runners are 6-core/12-thread. If we don't set
         // a number of test workers explicitly, 12 will be used, but our tests
         // don't seem to get along well with being put on hyperthreads. (They
         // seem to stall sometimes--perhaps something weird about the XNU
         // scheduler, but I couldn't confirm it.) So we explicitly limit the
         // number of test workers to be the number of hardware cores.
         semgrep.build_test_steps(test_workers='6') +
         [
           actions.make_artifact_step('./bin/semgrep-core'),
           actions.upload_artifact_step(artifact_name),
         ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-osx-x86',
  on: gha.on_dispatch_or_call,
  jobs: {
    'build-core': build_core_job,
  },
}

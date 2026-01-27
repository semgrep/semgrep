// This workflow builds and tests the semgrep-core binary for macOS arm64
// coupling: if you modify this file, modify also build-test-osx-x86.jsonnet

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------
local runs_on = 'macos-15-xlarge';

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// alt: we could factorize more with build-test-osx-x86.jsonnet by making
// the xxx_job functions, but let's copy paste a bit for now.
local artifact_name = 'semgrep-osx-arm64-${{ github.sha }}';

local build_core_job = {
  'runs-on': runs_on,
  steps: actions.checkout_with_submodules() +
         semgrep.build_test_steps() +
         [
           actions.make_artifact_step('./bin/semgrep-core'),
           actions.upload_artifact_step(artifact_name),
         ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-osx-arm64',
  on: gha.on_dispatch_or_call,
  jobs: {
    'build-core': build_core_job,
  },
}

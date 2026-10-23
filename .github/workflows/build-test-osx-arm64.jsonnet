// This workflow builds and tests the semgrep-core binary for macOS arm64

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

local artifact_name = 'semgrep-osx-arm64-${{ github.sha }}';

local build_core_job = {
  'runs-on': runs_on,
  steps: actions.checkout_with_submodules() +
         // Workaround for brew lock conflict on openssl@3 during opam depext installs.
         // https://linear.app/semgrep/issue/CODE-10023
         [semgrep.brew_unlink_openssl_step] +
         semgrep.build_test_steps() +
         [
           actions.upload_artifact_step(artifact_name, './bin/semgrep-core'),
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

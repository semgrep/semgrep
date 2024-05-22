// TODO: this workflow does not provide a full sync of OSS to Pro.
// It just takes what is in the HEAD in the OSS repo (e.g., the patch of the
// release) and create a PR with it in pro. This could be used later
// also to sync simple contributions to OSS from external contributors.
// TODO: call this workflow from the release workflow

local semgrep = import 'libs/semgrep.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// Main job
// ----------------------------------------------------------------------------

local job = {
  'runs-on': 'ubuntu-latest',
  steps: [
     {
       run: 'echo TODO'
     },
  ],
};

// ----------------------------------------------------------------------------
// Workflow
// ----------------------------------------------------------------------------

{
  name: 'sync-with-PRO',
  on: {
    workflow_dispatch: null,
  },
  jobs: {
    job: job,
  },
}

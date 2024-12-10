// This workflow dogfoods 'semgrep ci', which includes running rules
// for Code, Supply Chain, and Secrets set in the Semgrep WebApp.
// We're also dogfooding the semgrep/semgrep:canary docker image!
// (see https://www.notion.so/semgrep/returntocorp-semgrep-canary-docker-canary).

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The Job
// ----------------------------------------------------------------------------

local mk_job(steps) = {
  'runs-on': 'ubuntu-20.04',
  container: {
    // We're dogfooding the canary here!
    image: 'semgrep/semgrep:canary',
  },
  env: semgrep.secrets,
  steps: [ actions.checkout() ] + steps,
} + gha.dependabot_guard;

local semgrep_ci_job = mk_job([{ run: 'semgrep ci' } ]);
local semgrep_ci_oss_job = mk_job([{ run: 'semgrep ci --oss-only' } ]);
local semgrep_ci_debug_job = mk_job([{ run: 'semgrep ci --debug' } ]);

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  // without this name:, the workflow would look as ".github/workflows/semgrep.yml ..."
  name: 'semgrep',
  on: {
    // This workflow runs on 'pull_request_target' so that PRs from forks are able
    // to run an action that uses the SEMGREP_APP_TOKEN secret.
    // Note that any modification of this file in a PR does not reflect on said PR
    // Changes must be merged to develop first.
    pull_request_target: {},
    // can be run manually from the GHA dashboard
    workflow_dispatch: null,
    // this will run a full-scan
    push: {
      branches: [
        'develop',
      ],
    },
    schedule: [
      {
        // random HH:MM to avoid a load spike on GitHub Actions at 00:00
        cron: '50 15 * * *',
      },
    ],
  },
  jobs: {
    'semgrep-ci': semgrep_ci_job,
    'semgrep-ci-oss': semgrep_ci_oss_job,
    'semgrep-ci-debug': semgrep_ci_debug_job,
  },
}

// This workflow dogfoods 'semgrep ci' with Code, Supply Chain, Secrets
// and the Semgrep WebApp.
// We're also dogfooding the returntocorp.semgrep:canary docker image!
// See https://www.notion.so/semgrep/returntocorp-semgrep-canary-docker-canary

local gha = import 'libs/gha.libsonnet';
local actions = import "libs/actions.libsonnet";
local semgrep = import 'libs/semgrep.libsonnet';

local job = {
  name: 'semgrep ci',
  'runs-on': 'ubuntu-20.04',
  container: {
    // We're dogfooding the canary here!
    image: 'returntocorp/semgrep:canary',
  },
  env: semgrep.secrets,
  steps: [
    actions.checkout(),
    {
      // Dogfood!
      run: 'semgrep ci',
    },
  ],
} + gha.dependabot_guard;

{
  name: 'semgrep',
  on: {
    // This workflow runs on 'pull_request_target' so that PRs from forks are able
    // to run an action that uses the SEMGREP_APP_TOKEN secret.
    // Note that any modification of this file in a PR does not reflect on said PR
    // Changes must be merged to develop first.
    pull_request_target: {},
    push: {
      branches: [
        'develop',
      ],
      // ???
      paths: [
        '.github/workflows/semgrep.yml',
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
    semgrep: job,
  },
}

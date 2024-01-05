// Cron to verify that the release is still working (at least in dry-mode),
// and that the Semgrep Homebrew formula still work.

local release_homebrew = import 'release-homebrew.jsonnet';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------
// This will post on Slack on #logs-semgrep-release
// TODO: factorize with test-e2e-semgrep-ci.jsonnet using slack-github action
local curl_notify() = |||
        curl --request POST \
        --url  ${{ secrets.HOMEBREW_NIGHTLY_NOTIFICATIONS_URL }} \
        --header 'content-type: application/json' \
        --data '{
          "commit_sha": "${{ github.sha }}",
          "workflow_url": "https://github.com/${{github.repository}}/actions/runs/${{github.run_id}}"
        }'
|||;

// ----------------------------------------------------------------------------
// Failure notification
// ----------------------------------------------------------------------------

local notify_failure_job = {
  needs: [
    'brew-build',
    'release-dry-run',
  ],
  name: 'Notify of Failure',
  'runs-on': 'ubuntu-20.04',
  'if': 'failure()',
  steps: [
    {
      run: curl_notify(),
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'nightly',
  on: {
    workflow_dispatch: null,
    schedule: [
      {
        // every day at 9:26
        cron: '26 9 * * *',
      },
    ],
  },
  jobs: {
    'brew-build': release_homebrew.export.brew_build,
    'release-dry-run': {
      uses: './.github/workflows/release.yml',
      secrets: 'inherit',
      with: {
        'dry-run': true,
      },
    },
    'notify-failure': notify_failure_job,
  },
}

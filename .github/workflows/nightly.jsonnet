// Cron to verify that the release is still working (at least in dry-mode),
// and that the Semgrep Homebrew formula still work.

local semgrep = import 'libs/semgrep.libsonnet';
local release_homebrew = import 'release-homebrew.jsonnet';

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
    'notify-failure': semgrep.slack.notify_failure_nightly_job(
      "${{ github.sha }}",
      "https://github.com/${{github.repository}}/actions/runs/${{github.run_id}}"
      ) + { needs: ['brew-build', 'release-dry-run'] },
  },
}

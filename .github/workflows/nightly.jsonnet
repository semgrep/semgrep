// Cron to verify that the release is still working (at least in dry-mode),
// and that the Semgrep Homebrew formula still work.

local semgrep = import 'libs/semgrep.libsonnet';
local release_homebrew = import 'release-homebrew.jsonnet';

// Job to check whether Semgrep can still be built for Homebrew
// (https://brew.sh/).
//
// Note that the Semgrep Homebrew "formula" is not stored in this repo but at
// https://github.com/Homebrew/homebrew-core/blob/master/Formula/s/semgrep.rb
// The main goal of this workflow is to just check whether the current
// semgrep.rb is compatible with the current version of Semgrep.
//
// This job is used to test the Homebrew build against `develop`

local env = {
  // We've had issues with the job below in the past, and needed to ensure that
  // Homebrew wouldn't use the API.
  // See: https://github.com/orgs/Homebrew/discussions/4150, and
  // https://github.com/orgs/Homebrew/discussions/4136 as well as
  // other discussions on this topic on Github.
  HOMEBREW_NO_INSTALL_FROM_API: 1,
};

// This is called from nightly.jsonnet
// The Semgrep formula is bumped by Homebrew's automations, however
// we also want to double check that the formula still builds with
// the 'develop' branch. This serves two purposes:
//  - verifies that our changes in develop don't break brew
//  - gives us time before release to fix these issues and adjust our
//    Homebrew formula if needed.
local brew_build_job = {
  name: 'Build Semgrep via Brew from HEAD',
  'runs-on': 'macos-latest',
  steps: [
    {
      run: 'brew update --debug --verbose',
      env: env,
    },
    {
      // See https://github.com/Homebrew/brew/issues/1742 for context on the
      // brew link step.
      run: 'brew install semgrep --HEAD --debug || brew link --overwrite semgrep',
      env: env {
        NONINTERACTIVE: 1,
      },
    },
    {
      name: 'Check installed correctly',
      run: 'brew test semgrep --HEAD',
      env: env,
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
    'brew-build': brew_build_job,
    'release-dry-run': {
      uses: './.github/workflows/release.yml',
      secrets: 'inherit',
      with: {
        'dry-run': true,
      },
    },
    'notify-failure': semgrep.slack.notify_failure_job(
      'The nightly cron failed on ${{ github.sha }}. See https://github.com/${{github.repository}}/actions/runs/${{github.run_id}} for more information.'
    ) + { needs: ['brew-build', 'release-dry-run'] },
  },
}

// Cron to verify that the Homebrew Core Formula Works.
// This formula is stored in https://github.com/Homebrew/homebrew-core/blob/HEAD/Formula/s/semgrep.rb
// and "bumped" in release.yml by dawidd6/action-homebrew-bump-formula@v3
//
// This formula is created by our release process with the PR to homebrew/homebrew-core.
// What this workflow does is uses the latest version of the formula at that repo, but
// 'develop' branch source code from our PR. This serves two purposes:
//  - verifies that our changes don't break Brew
//  - gives us time before release to fix these issues and adjust our homebrew formula
//    if needed.

// ----------------------------------------------------------------------------
// The main job
// ----------------------------------------------------------------------------

local env = {
  // We've had issues with this workflow in the past, and needed to ensure that
  // homebrew wouldn't use the API.
  // See: https://github.com/orgs/Homebrew/discussions/4150, and
  // https://github.com/orgs/Homebrew/discussions/4136
  // There's also much other discussion on this topic available on GH and in the
  // brew discussions.
  HOMEBREW_NO_INSTALL_FROM_API: 1,
};

local brew_build_job = {
  name: 'Build Semgrep via Brew from `returntocorp/semgrep:develop`',
  'runs-on': 'macos-12',
  steps: [
    {
      run: 'brew update --debug --verbose',
      env: env,
    },
    {
      // See https://github.com/Homebrew/brew/issues/1742 for context on the brew link step.
      run: 'brew install semgrep --HEAD --debug || brew link --overwrite semgrep',
      env: env + {
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
// Failure notification
// ----------------------------------------------------------------------------

// TODO: factorize with test-e2e-semgrep-ci.jsonnet using slack-github action
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
      name: 'Notify Failure',
      run: |||
        curl --request POST \
        --url  ${{ secrets.HOMEBREW_NIGHTLY_NOTIFICATIONS_URL }} \
        --header 'content-type: application/json' \
        --data '{
          "commit_sha": "${{ github.sha }}",
          "workflow_url": "https://github.com/${{github.repository}}/actions/runs/${{github.run_id}}"
        }'
      |||,
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
    'notify-failure': notify_failure_job,
  },
}

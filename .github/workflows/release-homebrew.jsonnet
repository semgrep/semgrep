// This file contains jobs to release Semgrep on Homebrew (https://brew.sh/).
// Note that the Semgrep homebrew "recipe" is not stored in this repo but at
// https://github.com/Homebrew/homebrew-core/blob/master/Formula/s/semgrep.rb
// The goal of the jobs below is to modify semgrep.rb after a new release
// and to open a PR to the homebrew-core repo with the modified semgrep.rb
// (e.g., https://github.com/Homebrew/homebrew-core/pull/157891 for 1.54.1)
//
// The jobs in this file are used from release.jsonnet and nightly.jsonnet,
// but it's also useful to have a separate workflow to trigger the
// homebrew release manually as we often get issues with homebrew.
//
// You can also call 'brew bump-formula-pr ...' locally on your Mac to
// debug issues. However, you'll first need to run:
//
//    $ brew tap Homebrew/core --force
//
// to set the right Homebrew environment otherwise the
// 'brew bump-formula-pr ... semgrep' command will fail with:
//
//    Error: No available formula with the name "semgrep".

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------

local input = {
  inputs: {
    'dry-run': {
      type: 'boolean',
      description: |||
        Check the box for a dry-run.
        A dry-run will not push any external state.
      |||,
      default: false,
      required: true,
    },
  },
};

// TODO: hardcoded for now
local version = "1.55.2";
local tag = "v1.55.2";

local unless_dry_run = {
  'if': '${{ ! inputs.dry-run }}',
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// Note that this job needs to run after Semgrep has been released on Pypi so
// brew bump-formula-pr below can update Pypi dependency hashes in semgrep.rb
local homebrew_core_pr_job = {
  'runs-on': 'macos-12',
  steps: [
    {
      run: 'brew update',
    },
    // ugly:  'brew bump-formula-pr' below internally calls
    // /path/to/python -m pip install -q ... semgrep==1.xxx.yyy
    // to fetch Semgrep python dependencies from Pypi but this path to python
    // seems currently wrong hence this ugly fix below
    {
      name: 'ugly: fix the python path for brew bump-formula-pr',
      run: 'cd /usr/local/Cellar/python@3.11; ln -s 3.11.6_1 3.11.7'
    },
    {
      name: 'Bump semgrep.rb',
      // Note that we use '--write-only' below so the command does not
      // open a new PR; it just modifies .../Formula/s/semgrep.rb
      // The step below will make the commit, and the step further below
      // will open the PR. We do things in 3 steps to help debug issues.
      run: |||
        brew bump-formula-pr --force --no-audit --no-browse --write-only \
          --message="semgrep %s" \
          --tag="%s" semgrep --debug
      ||| % [version, tag],
    },
    {
      name: 'Prepare commit',
      env: {
        GITHUB_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
      },
      run: |||
        cd "$(brew --repository)/Library/Taps/homebrew/homebrew-core"
        git status
        git diff
        git config user.name ${{ github.actor }}
        git config user.email ${{ github.actor }}@users.noreply.github.com
        gh auth setup-git
        git remote add r2c https://github.com/semgrep-release/homebrew-core.git
        git checkout -b bump-semgrep-%s
        git add Formula/s/semgrep.rb
        git commit -m "semgrep %s"
      ||| % [version, version],
    },
    {
      name: 'Push commit to our fork of homebrew-core',
      env: {
        GITHUB_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
      },
      run: |||
        cd "$(brew --repository)/Library/Taps/homebrew/homebrew-core"
        git push --set-upstream r2c --force "bump-semgrep-%s"
      ||| % version,
    } + unless_dry_run,
    {
      name: 'Open PR to official homebrew-core repo',
      env: {
        GITHUB_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
      },
      // 'semgrep-release' below corresponds to r2c homebrew core owner
      run: |||
        gh pr create --repo homebrew/homebrew-core \
          --base master --head "semgrep-release:bump-semgrep-%s" \
          --title="semgrep %s" \
          --body "Bump semgrep to version %s"
      ||| % [version, version, version],
    } + unless_dry_run,
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'release-homebrew',
  on: {
    workflow_dispatch: input,
  },
  jobs: {
    'homebrew-core-pr': homebrew_core_pr_job,
  },
}

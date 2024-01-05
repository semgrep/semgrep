// Workflow to test/release Semgrep on Homebrew (https://brew.sh/).
//
// Note that the Semgrep Homebrew "formula" is not stored in this repo but at
// https://github.com/Homebrew/homebrew-core/blob/master/Formula/s/semgrep.rb
// The main goal of this workflow is to modify semgrep.rb after a new release
// and to open a PR to the homebrew-core repo with the modified semgrep.rb
// (e.g., https://github.com/Homebrew/homebrew-core/pull/157891 for 1.54.1)
//
// The jobs in this file are used from release.jsonnet and nightly.jsonnet,
// but it's also useful to have a separate workflow to trigger the
// Homebrew release manually as we often get issues with Homebrew.
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
    version: {
      type: 'string',
      description: |||
        The version of Semgrep to release on Homebrew (e.g., 1.55.2)
      |||,
      required: true,
    },
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

local unless_dry_run = {
  'if': '${{ ! inputs.dry-run }}',
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// This is also called from release.jsonnet.
// Note that this job needs to run after Semgrep has been released on Pypi so
// brew bump-formula-pr below can update Pypi dependency hashes in semgrep.rb
local homebrew_core_pr_job(version, unless_dry_run) = {
  'runs-on': 'macos-12',
  steps: [
    {
      run: 'brew update',
    },
    // ugly:  'brew bump-formula-pr' below internally calls
    // /path/to/python -m pip install -q ... semgrep==1.xxx.yyy
    // to fetch Semgrep python dependencies from Pypi but this path to python
    // seems currently broken hence the ugly fix below
    {
      name: 'ugly: fix the python path for brew bump-formula-pr',
      run: 'cd /usr/local/Cellar/python@3.11; ln -s 3.11.6_1 3.11.7'
    },
    {
      name: 'Bump semgrep.rb',
      // Note that we use '--write-only' below so the command does not open a
      // new PR; it just modifies /usr/local/.../homebrew-core/.../s/semgrep.rb
      // The step below will make the commit, and the step further below
      // will open the PR. We do things in 3 steps to help debug issues.
      //
      // alt: use dawidd6/action-homebrew-bump-formula@v3
      run: |||
        brew bump-formula-pr --force --no-audit --no-browse --write-only \
          --message="semgrep %s" --tag="v%s" semgrep --debug
      ||| % [version, version],
    },
    {
      name: 'Make the commit',
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
        # TODO: we should move this repo in the semgrep org
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
      // 'semgrep-release' below corresponds to r2c Homebrew core owner
      run: |||
        gh pr create --repo homebrew/homebrew-core \
          --base master --head "semgrep-release:bump-semgrep-%s" \
          --title="semgrep %s" \
          --body "Bump semgrep to version %s"
      ||| % [version, version, version],
    } + unless_dry_run,
  ],
};


local env = {
  // We've had issues with the below in the past, and needed to ensure that
  // Homebrew wouldn't use the API.
  // See: https://github.com/orgs/Homebrew/discussions/4150, and
  // https://github.com/orgs/Homebrew/discussions/4136
  // There's also much other discussion on this topic available on GH and in
  // the brew discussions.
  HOMEBREW_NO_INSTALL_FROM_API: 1,
};

// This is called from nightly.jsonnet
// The Semgrep formula is bumped by homebrew_core_pr_job(), however
// we also want to double check that the formula still works with
// the 'develop' branch. This serves two purposes:
//  - verifies that our changes don't break Brew
//  - gives us time before release to fix these issues and adjust our
//    Homebrew formula if needed.
local brew_build_job = {
  name: 'Build Semgrep via Brew from HEAD',
  'runs-on': 'macos-12',
  steps: [
    {
      run: 'brew update --debug --verbose',
      env: env,
    },
    {
      // See https://github.com/Homebrew/brew/issues/1742 for context on the
      // brew link step.
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
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'release-homebrew',
  on: {
    workflow_dispatch: input,
  },
  jobs: {
    'homebrew-core-pr':
       homebrew_core_pr_job('${{ inputs.version }}', unless_dry_run),
    'brew-build': brew_build_job,
  },
  export:: {
     homebrew_core_pr: homebrew_core_pr_job,
     brew_build: brew_build_job,
  },
}

// Workflow to check whether Semgrep can still be built for Homebrew
// (https://brew.sh/).
//
// Note that the Semgrep Homebrew "formula" is not stored in this repo but at
// https://github.com/Homebrew/homebrew-core/blob/master/Formula/s/semgrep.rb
// The main goal of this workflow is to just check whether the current
// semgrep.rb is compatible with the current version of Semgrep.
//
// The jobs in this file are used from nightly.jsonnet,
// but it's also useful to have a separate workflow (this file) to trigger the
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

local gha = import "libs/gha.libsonnet";

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
      required: true,
    },
  },
};

local unless_dry_run = {
  'if': '${{ ! inputs.dry-run }}',
};

// ----------------------------------------------------------------------------
// The (deprecated) release job
// ----------------------------------------------------------------------------

// Note that this is *not* called anymore from release.jsonnet.
// Updating semgrep.rb is now handled by Homebrew's folk directly using an
// autobump mechanism. If you still want to run this job, note
// that this job needs to run after Semgrep has been released on Pypi so
// brew bump-formula-pr below can update Pypi dependency hashes in semgrep.rb
// This job assumes the presence of a workflow with a 'inputs.dry-mode'
local homebrew_core_pr_job(version) = {
  'runs-on': 'macos-latest',
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
      name: 'Dry Run bump semgrep.rb',
      // This step does some brew oddities (setting a fake version, and
      // setting a revision) to allow the brew PR prep to succeed.
      // The `brew bump-formula-pr` does checks to ensure your PR is legit,
      // but we want to do a phony PR (or at least prep it) for Dry Run only
      env: {
        HOMEBREW_GITHUB_API_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
      },
      // this is run only in dry-mode
      'if': "${{ inputs.dry-run }}",
      run: |||
        brew bump-formula-pr --force --no-audit --no-browse --write-only \
          --message="semgrep 99.99.99" \
          --tag="v99.99.99" --revision="${GITHUB_SHA}" semgrep --python-exclude-packages semgrep
      |||,
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
    } + unless_dry_run,
    {
      name: 'Make the commit',
      env: {
        GITHUB_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
      },
      // We're using a weird org below, https://github.com/semgrep-release/ instead of
      // https://github.com/semgrep/ (which was https://github.com/returntocorp before)
      // because Homebrew doesn't allow to use API tokens that were created by
      // an "Organization" account to create PRs to homebrew/homebrew-core; it has to
      // come from an individual account. The cleanest solution at the time was to
      // create a separate account for doing this.
      // See https://github.com/semgrep/semgrep/pull/7484 for more context.
      run: |||
        cd "$(brew --repository)/Library/Taps/homebrew/homebrew-core"
        git status
        git diff
        %s
        gh auth setup-git
        git remote add r2c https://github.com/semgrep-release/homebrew-core.git
        git checkout -b bump-semgrep-%s
        git add Formula/s/semgrep.rb
        git commit -m "semgrep %s"
      ||| % [gha.git_config_user, version, version],
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

// ----------------------------------------------------------------------------
// The test job
// ----------------------------------------------------------------------------

local env = {
  // We've had issues with the job below in the past, and needed to ensure that
  // Homebrew wouldn't use the API.
  // See: https://github.com/orgs/Homebrew/discussions/4150, and
  // https://github.com/orgs/Homebrew/discussions/4136 as well as
  // other discussions on this topic on Github.
  HOMEBREW_NO_INSTALL_FROM_API: 1,
};

// This is called from nightly.jsonnet
// The Semgrep formula is bumped by homebrew_core_pr_job(), however
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
       homebrew_core_pr_job('${{ inputs.version }}'),
    'brew-build': brew_build_job,
  },
  export:: {
     homebrew_core_pr: homebrew_core_pr_job,
     brew_build: brew_build_job,
  },
}

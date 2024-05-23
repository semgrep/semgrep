// Workflow to create a PR to update the Pro repo with changes in OSS.
//
// Note that this workflow does not provide a full sync of OSS to Pro;
// it just takes what is in the HEAD in develop in the OSS repo
// (e.g., the patch of the release that bumps the version) and create a PR
// with it in pro.
// This could be used later also to sync simple contributions to OSS from
// external contributors.
// TODO? in theory we could even move this workflow in Pro? (which makes
// it easier to iterate on)

local semgrep = import 'libs/semgrep.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// Main job
// ----------------------------------------------------------------------------

local job = {
  'runs-on': 'ubuntu-latest',
  permissions: gha.write_permissions,
  steps: semgrep.github_bot.get_token_steps + [
     {
      name: 'Checkout OSS',
      uses: 'actions/checkout@v3',
       with: {
        ref: 'develop',
        // fetch all history, seems needed to reference develop^ below
        'fetch-depth': 0,
        // Use the token provided by the JWT token getter above
        token: semgrep.github_bot.token_ref,
      },
     },
     {
      name: 'Checkout PRO',
      uses: 'actions/checkout@v3',
      with: {
        repository: 'semgrep/semgrep-proprietary',
        path: 'PRO',
        token: semgrep.github_bot.token_ref,
       },
     },
     {
      name: 'Creating the branch and commiting to it',
      env: {
        BRANCHNAME: 'sync-with-PRO-${{ github.run_id }}-${{ github.run_attempt }}',
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
       // the git config are needed otherwise GHA complains about
       // unknown identity
       run: |||
         if git show --stat develop | grep -q "synced from Pro"; then
            echo "error: HEAD commit already comes from Pro and cannot be synced"
            exit 1
         fi
         # will generate a 0001-xxx patch
         git format-patch develop^
         OSSREF=`git rev-parse develop`
         cd PRO
         git config --global user.name "GitHub Actions Bot"
         git config --global user.email "<>"
         git checkout -b $BRANCHNAME
         git am --directory=OSS ../0001-*
         git log -1 --pretty=%B >message
         echo "" >>message
         echo "synced from OSS $OSSREF" >>message
         git commit --amend -F message
         git push origin $BRANCHNAME
       |||,
     },
     {
      name: 'Create the Pull request with gh',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run : |||
         cd PRO
         gh pr create --fill --base develop
       |||,
     },

  ],
};

// ----------------------------------------------------------------------------
// Workflow
// ----------------------------------------------------------------------------

{
  name: 'sync-with-PRO',
  on: {
    // TODO: call this workflow from the release workflow
    workflow_dispatch: null,
  },
  jobs: {
    job: job,
  },
}

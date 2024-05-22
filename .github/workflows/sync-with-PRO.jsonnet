// Workflow to create a PR to update the Pro repo with changes in OSS.
// Note that this workflow does not provide a full sync of OSS to Pro;
// it just takes what is in the HEAD in the OSS repo (e.g., the patch of the
// release) and create a PR with it in pro. This could be used later
// also to sync simple contributions to OSS from external contributors.
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
        ref: '${{ github.event.repository.default_branch }}',
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
        BRANCHNAME: 'sync-with-PRO-x3-${{ github.run_id }}-${{ github.run_attempt }}',
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
       run: |||
         git format-patch develop^
         cd PRO
         git config --global user.name "GitHub Actions Bot"
         git config --global user.email "<>"
         git checkout -b $BRANCHNAME
         #TODO: apply patch from OSS HEAD to this branch
         cd OSS
         patch -p1 ../../0001-*
         git commit -a -m"sync OSS -> PRO"
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
         gh pr create --title 'sync OSS -> Pro' --body 'Please confirm correctness of the changes here and ensure all tests pass. This PR was autogenerated by OSS/.github/workflows/sync-with-PRO.jsonnet' --base develop
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

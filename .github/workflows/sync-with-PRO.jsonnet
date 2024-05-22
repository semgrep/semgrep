// Workflow to create a PR to update the Pro repo with changes in OSS.
// Note that this workflow does not provide a full sync of OSS to Pro;
// it just takes what is in the HEAD in the OSS repo (e.g., the patch of the
// release) and create a PR with it in pro. This could be used later
// also to sync simple contributions to OSS from external contributors.

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
      uses: 'actions/checkout@v3',
      with: {
        ref: '${{ github.event.repository.default_branch }}',
        // Use the token provided by the JWT token getter above
        token: semgrep.github_bot.token_ref,
      },
     },
     {
      uses: 'actions/checkout@v3',
      with: {
        repository: 'semgrep/semgrep-proprietary',
        path: 'PRO',
        token: semgrep.github_bot.token_ref,
       },
     },
     {
      env: {
        BRANCHNAME: 'sync-with-PRO-${{ github.run_id }}-${{ github.run_attempt }}',
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
       run: |||
         cd PRO
         git checkout -b $BRANCHNAME
         rm -f README.md
         git config --global user.name "GitHub Actions Bot"
         git commit -a -m"sync OSS -> PRO"
         git push origin $BRANCHNAME
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

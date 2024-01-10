// Factorize Github Actions (GHA) boilerplate.
// See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
// for more information on GHA or our Notion page on "Github actions".

{
  on_classic: {
    // can be run manually from the GHA dashboard
    workflow_dispatch: null,
    // on the PR
    pull_request: null,
    // and another time once the PR is merged on develop
    push: {
      branches: [
        'develop',
      ],
    },
  },
  on_dispatch_or_call: {
    workflow_dispatch: null,
    workflow_call: null,
  },
  write_permissions: {
    // needed when we want to upload data to s3
    'id-token': 'write',
    // needed when the job modifies the repository
    contents: 'write',
  },
  // Why this is not the default? GHA ...
  speedy_checkout_step: {
    name: 'Make checkout speedy',
    run: 'git config --global fetch.parallel 50',
  },
  // When we use git directly instead of gh.
  git_config_user: |||
    git config user.name ${{ github.actor }}
    git config user.email ${{ github.actor }}@users.noreply.github.com
  |||,
  git_safedir: {
    name: 'Configure git safedir properly',
    run: "git config --global --add safe.directory $(pwd)",
  },
  // stay away dependabot, bad dog.
  dependabot_guard: {
    'if': "(github.actor != 'dependabot[bot]')",
  },

}

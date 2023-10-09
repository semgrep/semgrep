// Factorize Github Actions (GHA) boilerplate.
// See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
// for more information on GHA or our Notion page on "Github actions".


{
  // Why this is not the default? GHA ...
  speedy_checkout_step: {
    name: 'Make checkout speedy',
    run: 'git config --global fetch.parallel 50',
  },
  // stay away dependabot, bad dog.
  dependabot_guard: {
    'if': "(github.actor != 'dependabot[bot]')",
  },

}

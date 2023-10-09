// Factorize Github Actions (GHA) boilerplate.
// See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
// for more information on GHA or our Notion page on "Github actions".


{
  dependabot_guard: {
    'if': "(github.actor != 'dependabot[bot]')",
  },

}

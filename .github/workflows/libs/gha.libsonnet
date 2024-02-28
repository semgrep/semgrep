// Factorize Github Actions (GHA) boilerplate.
// See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
// for more information on GHA or our Notion page on "Github actions".

{
  // Workflow helpers

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
    // Needed when we want to upload data to s3 or more generally
    // when connecting to cloud services that use Open ID Connect.
    // More details at
    // https://docs.github.com/en/actions/deployment/security-hardening-your-deployments/about-security-hardening-with-openid-connect
    'id-token': 'write',
    // Needed when the job modifies the repository such as performing
    // gh release commands.
    contents: 'write',
  },

  // Git helpers

  // Speed up checkout by running multiple fetches in parallel.
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
  # Git's filename limit is 4096 characters, except on Windows when Git is
  # compiled with msys. It uses an older version of the Windows API and
  # there's a limit of 260 characters for a filename. You can force the
  # longer limit (and avoid git issues) by enabling core.longpaths in your
  # git config.
  # More info: https://stackoverflow.com/a/22575737
  git_longpaths_step: {
    run: 'git config --system core.longpaths true'
  },

  // stay away dependabot, bad dog.
  dependabot_guard: {
    'if': "(github.actor != 'dependabot[bot]')",
  },
}

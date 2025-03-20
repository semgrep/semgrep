// Factorize Github Actions (GHA) boilerplate.
// See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
// for more information on GHA or our Notion page on "Github actions".

local on_pull_request_config = {
  types: ['opened', 'reopened', 'synchronize'],
  // https://graphite.dev/docs/merge-pull-requests#ignoring-graphites-temporary-branches-in-your-ci
  'branches-ignore': ['**/graphite-base/**'],
};

{
  // Workflow helpers
  on_classic: {
    // can be run manually from the GHA dashboard
    workflow_dispatch: null,
    // on the PR
    pull_request: on_pull_request_config,
    // and another time once the PR is merged on develop
    push: {
      branches: [
        'develop',
      ],
    },
  },
  // Prefer `on_classic` typically. But some jobs really only make sense on pull
  // request.
  on_pull_request: {
    pull_request: on_pull_request_config,
  },
  on_dispatch_or_call: {
    workflow_dispatch: null,
    workflow_call: null,
  },
  on_schedule(cron): {
    workflow_dispatch: null,
    schedule: [{
      cron: cron,
    }],
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
  read_permissions: {
    'id-token': 'write',
    contents: 'read',
  },
  pull_request_permissions: {
    'id-token': 'write',
    contents: 'write',
    // needed when we want to modify the pull-request (e.g., for snapshot update)
    'pull-requests': 'write',
  },

  // For making matrix jobs, i.e. one job running on multiple OSes.
  os_matrix(oss=['ubuntu-latest', 'macos-latest', 'windows-latest'],steps): {
    strategy: {
      matrix: {
        os: oss,
      },
    },
    'runs-on': '${{ matrix.os }}',
    steps: steps,
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

  // stay away dependabot, bad dog.
  dependabot_guard: {
    'if': "(github.actor != 'dependabot[bot]')",
  },
  basic_needs_job(needs): {
    needs: needs,
    'runs-on': 'ubuntu-latest',
    steps: [
      {
        name: 'Wait for jobs: %s' % std.join(', ', needs),
        run: 'echo "jobs %s are done"' % std.join(', ', needs),
      },
    ],

  },
}

// Factorize Github Actions (GHA) boilerplate.
// See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
// for more information on GHA or our Notion page on "Github actions".
local uses = import './uses.libsonnet';

local on_pull_request_config = {
  types: ['opened', 'reopened', 'synchronize'],
  // https://graphite.dev/docs/merge-pull-requests#ignoring-graphites-temporary-branches-in-your-ci
  'branches-ignore': ['**/graphite-base/**'],
};

local write_permissions = {
  // Needed when we want to upload data to s3 or more generally
  // when connecting to cloud services that use Open ID Connect.
  // More details at
  // https://docs.github.com/en/actions/deployment/security-hardening-your-deployments/about-security-hardening-with-openid-connect
  'id-token': 'write',
  // Needed when the job modifies the repository such as performing
  // gh release commands.
  contents: 'write',
};

// More restrictive permissions for jobs that only need to read the repository contents and not modify it.
local read_permissions = write_permissions {
  contents: 'read',
};

// needed when we want to modify the pull-request (e.g., for snapshot update)
local pull_request_permissions = read_permissions {
  'pull-requests': 'write',
};

local pull_request_write_permissions = pull_request_permissions {
  contents: 'write',
};

// needed to check workflow runs
local check_run_permissions = read_permissions {
  actions: 'read',
};

// needed to check workflow runs and comment on PRs with results
local check_and_comment_permissions = pull_request_permissions {
  actions: 'read',
};

// Most permissive permissions, for jobs that call other jobs and comment on PRs with results
local check_comment_and_write_permissions = check_and_comment_permissions {
  contents: 'write',
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
        'release-*',
      ],
    },
  },
  // Prefer `on_classic` typically. But some jobs really only make sense on pull
  // request.
  on_pull_request: {
    pull_request: on_pull_request_config,
  },
  // `on_classic` with a paths-ignore filter on the pull_request trigger only.
  // Push triggers stay unfiltered so develop/release branches keep full coverage.
  on_classic_paths_ignore_pr(paths_ignore):
    self.on_classic {
      pull_request: on_pull_request_config { 'paths-ignore': paths_ignore },
    },
  // `on_pull_request` with a paths-ignore filter.
  on_pull_request_paths_ignore(paths_ignore):
    { pull_request: on_pull_request_config { 'paths-ignore': paths_ignore } },
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
  read_permissions: read_permissions,
  write_permissions: write_permissions,
  pull_request_permissions: pull_request_permissions,
  pull_request_write_permissions: pull_request_write_permissions,
  check_run_permissions: check_run_permissions,
  check_and_comment_permissions: check_and_comment_permissions,
  check_comment_and_write_permissions: check_comment_and_write_permissions,


  // For making matrix jobs, i.e. one job running on multiple OSes.
  os_matrix(oss=['ubuntu-latest', 'macos-latest', 'windows-2025'], steps): {
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
    run: 'git config --global --add safe.directory $(pwd)',
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
  // GHA expression that evaluates to the input ref or, if that is empty, the sha
  // associated with the trigger for this action.
  ref_expr: "${{ inputs.ref != '' && inputs.ref || github.sha }}",
  ref_input: {
    description: 'Git ref to checkout. Defaults to github.sha',
    required: false,
    type: 'string',
    default: '',
  },
  sticky_pull_request_comment(message, header): {
    uses: uses.marocchino.sticky_pull_request_comment,
    with: {
      header: header,
      message: message,
      recreate: true,
      skip_unchanged: true,
    },
  },
  delete_sticky_pull_request_comment(header): {
    uses: uses.marocchino.sticky_pull_request_comment,
    with: {
      header: header,
      delete: true,
    },
  },

  // Make a job conditional on an expression. Errors if the job already has an `if`.
  // The expression should be the full GHA expression including '${{ ... }}'.
  make_job_conditional(job, expr)::
    if std.objectHas(job, 'if') then
      error 'job is already conditional'
    else
      job { 'if': expr },
}

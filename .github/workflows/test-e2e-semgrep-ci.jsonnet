// Daily cron to check end-to-end (e2e) that the 'semgrep ci' subcommand,
// ran from our docker 'develop' semgrep image, works correctly.
// This is very important because 'semgrep ci' and our 'develop' docker image
// are the main things users of Semgrep WebApp are using in their CIs.
// It also allows us to confirm "fail-open" behavior is still functioning as
// expected, that is we aren't crashing when we can't upload findings.

local actions = import "libs/actions.libsonnet";
local semgrep = import "libs/semgrep.libsonnet";

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------

local docker_tag_input = {
      inputs: {
        docker_tag: {
          description: 'Docker Tag to Run. Default: develop',
          required: false,
          //TODO? could propose choice with 'canary' also
          default: 'develop',
        },
      }
};

// just validate and reexport the input in docker_tag_input above.
// TODO? not sure we need this intermediate job ... we should
// use directly inputs.docker_tag in the other jobs
local get_inputs_job = {
  name: 'Get Inputs',
  'runs-on': 'ubuntu-22.04',
  outputs: {
    docker_tag: '${{ steps.get-inputs.outputs.docker_tag }}',
  },
  steps: [
    {
      name: 'Set variables',
      id: 'get-inputs',
      //TODO? why do we need that given we set a default value above?
      run: 'echo "docker_tag=${{ inputs.docker_tag || develop }}" >> $GITHUB_OUTPUT',
    },
    {
      name: 'Debug',
      run: 'echo "${{ steps.get-inputs.outputs.docker_tag }}"',
    },
  ],
};

// to be used below once the job added needs: 'get-inputs'
local needs_docker_tag = '${{ needs.get-inputs.outputs.docker_tag }}';

// ----------------------------------------------------------------------------
// Basic semgrep CI checks
// ----------------------------------------------------------------------------

local semgrep_ci_job = {
  'runs-on': 'ubuntu-22.04',
  env: {
   // TODO: why not semgrep.secrets.SEMGREP_APP_TOKEN? Why a different token?
   // We use a different ruleboard for that?
    SEMGREP_APP_TOKEN: semgrep.secrets.E2E_APP_TOKEN,
  },
  needs: 'get-inputs',
  container: {
    image: 'returntocorp/semgrep:' + needs_docker_tag,
  },
  steps: [
    actions.checkout(),
    // dogfooding! we run semgrep ci on the semgrep repo itself
    // and it should not find any blocking findings and so
    // should exit with code 0
    {
      run: 'semgrep ci',
    },
  ],
};

local semgrep_ci_fail_open_job = {
  'runs-on': 'ubuntu-22.04',
  env: {
    SEMGREP_APP_TOKEN: semgrep.secrets.E2E_APP_TOKEN,
    // TODO? Why those settings?
    SEMGREP_APP_URL: 'https://staging.semgrep.dev',
    SEMGREP_USER_AGENT_APPEND: 'semgrep-ci-e2e',
  },
  needs: 'get-inputs',
  container: {
    image: 'returntocorp/semgrep:' + needs_docker_tag,
  },
  steps: [
    actions.checkout(),
    // we remove git to test whether its absence cause the whole thing
    // to return an error. Note that 'semgrep ci' rely internally
    // on git compute the project_metadata information so the absence
    // of git should generate some errors internally.
    // However, in fail-open mode (--suppress-errors) we
    // should not crash if something internally fails
    {
      name: 'Remove Git Exe',
      run: |||
        which git
        rm /usr/bin/git
      |||,
    },
    {
      // should still exit with code 0
      run: 'semgrep ci --suppress-errors',
    },
  ],
};

// Note that this job is not in the notify_failure_job.needs.
// TODO: not sure why we do this test then
local semgrep_ci_fail_open_blocking_findings_job = {
  'runs-on': 'ubuntu-22.04',
  env: {
    SEMGREP_APP_TOKEN: semgrep.secrets.E2E_APP_TOKEN,
    SEMGREP_USER_AGENT_APPEND: 'semgrep-ci-e2e',
  },
  needs: 'get-inputs',
  container: {
    image: 'returntocorp/semgrep:' + needs_docker_tag,
  },
  steps: [
    actions.checkout(),
    {
      name: 'Create code under test',
      id: 'create-code',
      // we should report a finding here
      // (see use-click-secho.yml in semgrep-rules)
      run: |||
        cat > ./test.py <<- EOF
        import click
        click.echo(click.style("foo"))
        EOF
      |||,
    },
    {
      name: 'Run CI',
      id: 'run-ci',
      // If we get error code 0: unexpected, return an error.
      // Otherwise, continue on.
      // Other tests ensure that error code >=2 are handled appropriately.
      //TODO: pad: I don't understand this test ...
      run: |||
        if semgrep ci --suppress-errors; then
           exit 2
        else
           exit 0
        fi
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// PR checks
// ----------------------------------------------------------------------------

// dependencies: semgrep-ci-on-pr -> pr-url -> wait-for-checks

// ?? What does this test?
local semgrep_ci_on_pr_job = {
  uses: './.github/workflows/open-bump-pr.yml',
  secrets: 'inherit',
  needs: 'get-inputs',
  with: {
    version: needs_docker_tag,
    // ??
    repository: 'returntocorp/e2e',
    base_branch: 'develop',
    new_branch_name: 'e2e-test-${{ github.run_id }}',
    // ??
    bump_script_path: 'scripts/change-version.sh',
  },
};

// ??
local pr_url_job = {
  'runs-on': 'ubuntu-22.04',
  needs: 'semgrep-ci-on-pr',
  steps: [
    {
      run: 'echo ${{ needs.semgrep-ci-on-pr.outputs.pr-url }}',
    },
    {
      run: 'echo ${{ needs.semgrep-ci-on-pr.outputs.pr-number }}',
    },
  ],
};

local wait_for_checks_job = {
  'runs-on': 'ubuntu-22.04',
  needs: 'semgrep-ci-on-pr',
  steps: [
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
    {
      name: 'Wait for checks to register',
      env: semgrep.github_bot.github_token,
      run: |||
        LEN_CHECKS=$(gh pr -R returntocorp/e2e view "${{ needs.semgrep-ci-on-pr.outputs.pr-number }}" --json statusCheckRollup --jq '.statusCheckRollup | length');

        # Immediately after creation, the PR doesn't have any checks attached
        # yet, wait until this is not the case
        # If you immediately start waiting for checks, then it just fails
        # saying there's no checks.
        while [ ${LEN_CHECKS} = "0" ]; do
          echo "No checks available yet"
          sleep 30
          LEN_CHECKS=$(gh pr -R returntocorp/e2e view "${{ needs.semgrep-ci-on-pr.outputs.pr-number }}" --json statusCheckRollup --jq '.statusCheckRollup | length');
        done
        echo "checks are valid"

        echo ${LEN_CHECKS}

        gh pr -R returntocorp/e2e view "${{ needs.semgrep-ci-on-pr.outputs.pr-number }}" --json statusCheckRollup
      |||,
    },
    {
      name: 'Wait for checks to complete',
      env: semgrep.github_bot.github_token,
      run: |||
        # Wait for PR checks to finish
        gh pr -R returntocorp/e2e checks "${{ needs.semgrep-ci-on-pr.outputs.pr-number }}" --interval 30 --watch
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// Failure notification
// ----------------------------------------------------------------------------

local notify_failure_job = {
  needs: [
    'semgrep-ci',
    'semgrep-ci-on-pr',
    'semgrep-ci-fail-open',
    'wait-for-checks',
    'get-inputs',
  ],
  name: 'Notify of Failure',
  'runs-on': 'ubuntu-20.04',
  'if': 'failure()',
  steps: [
    {
      name: 'Notify Failure',
      run: |||
        curl --request POST \
        --url  ${{ secrets.SEMGREP_CI_E2E_NOTIFICATIONS_URL }} \
        --header 'content-type: application/json' \
        --data '{
          "workflow_run_url": "https://github.com/${{github.repository}}/actions/runs/${{github.run_id}} for more details!",
          "docker_tag": "${{ needs.get-inputs.outputs.docker_tag }}",
          "message": "The PR in `returntocorp/e2e` that had the failure was ${{ needs.semgrep-ci-on-pr.outputs.pr-number }}"
         }
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'e2e-semgrep-ci',
  on: {
    workflow_dispatch: docker_tag_input,
    schedule: [
      {
        // At 20:43 every day
        cron: '43 20 * * *',
      },
    ],
  },
  jobs: {
    'get-inputs': get_inputs_job,
    'semgrep-ci': semgrep_ci_job,
    'semgrep-ci-fail-open': semgrep_ci_fail_open_job,
    'semgrep-ci-fail-open-blocking-findings': semgrep_ci_fail_open_blocking_findings_job,
    'semgrep-ci-on-pr': semgrep_ci_on_pr_job,
    'pr-url': pr_url_job,
    'wait-for-checks': wait_for_checks_job,
    'notify-failure': notify_failure_job,
  },
}

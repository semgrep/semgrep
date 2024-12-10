// Daily cron to check end-to-end (e2e) that the 'semgrep ci' subcommand,
// run from a semgrep/semgrep:develop docker image, works correctly.
// This is very important because 'semgrep ci' and our docker images
// are the main things the users of our Semgrep WebApp are using in their CIs.
// This cron also double checks that "fail-open" is working as expected, that is
// we aren't failing the CI check when we can't upload findings because of
// networking errors (or for other reasons such as a 'git' execution failure).

local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Constants
// ----------------------------------------------------------------------------

// This is computed by the get_inputs_job (example of value: "develop")
// and can be referenced from other jobs.
local docker_tag = "${{ needs.get-inputs.outputs.docker_tag }}";

// This is computed by semgrep_ci_on_pr_job (example of value: "9543")
// and can be referenced from other jobs
local pr_number = "${{ needs.semgrep-ci-on-pr.outputs.pr-number }}";

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
  },
};

// This validates and reexport the input in docker_tag_input above.
// Why this intermediate job? Can't we use directly inputs.docker_tag
// in the other jobs? It's because this workflow can be called
// interactively (workflow_dispatch), or via a cron, and for a cron
// we don't have a way to say docker_tag should be 'develop', hence
// this intermediate job.
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
      env: {
        DOCKER_TAG: 'develop',
      },
      run: 'echo "docker_tag=${{ inputs.docker_tag || env.DOCKER_TAG }}" >> $GITHUB_OUTPUT',
    },
    {
      name: 'Debug',
      run: 'echo "${{ steps.get-inputs.outputs.docker_tag }}"',
    },
  ],
};

// ----------------------------------------------------------------------------
// Basic semgrep CI checks (on the semgrep repo itself)
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
    image: 'semgrep/semgrep:' + docker_tag,
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
    image: 'semgrep/semgrep:' + docker_tag,
  },
  steps: [
    actions.checkout(),
    // we remove git to test whether its absence cause the whole thing
    // to return an error. Note that 'semgrep ci' relies internally
    // on git to compute the project_metadata information so the absence
    // of git should generate some errors internally.
    // However, in fail-open mode (--suppress-errors) we
    // should not fail the CI check if something internally failed.
    {
      name: 'Remove Git Exe',
      run: |||
        which git
        rm /usr/bin/git
      |||,
    },
    {
      // should still exit with code 0 (ugly, but the backend
      // is sometimes unaivalable for a few minutes and we don't
      // want all our customers to suddenly fail their CI check)
      run: 'semgrep ci --suppress-errors',
    },
  ],
};

local semgrep_ci_fail_open_blocking_findings_job = {
  'runs-on': 'ubuntu-22.04',
  env: {
    SEMGREP_APP_TOKEN: semgrep.secrets.E2E_APP_TOKEN,
    SEMGREP_USER_AGENT_APPEND: 'semgrep-ci-e2e',
  },
  needs: 'get-inputs',
  container: {
    image: 'semgrep/semgrep:' + docker_tag,
  },
  steps: [
    actions.checkout(),
    {
      name: 'Create code under test',
      id: 'create-code',
      // we should report a (blocking) finding here
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
      // Here we fail-open (with --suppress-errors), but that does not mean
      // we should go through if semgrep finds blocking findings!
      // We "fail-open" only on unexpected internal errors (e.g., networking
      // issues). This is not the case here, so semgrep should return
      // an exit code > 0.
      //
      // If we get exit code 0, meaning semgrep didn't find
      // any blocking finding, then we take the 'then' branch
      // (remember that in bash exit code '0' means everything is fine
      // so exit 2 below is taken only 'if 0')
      // and we return an error (exit 2).
      // Otherwise, semgrep found an error (exit code > 0), meaning
      // we take the else branch, and exit 0 as this test is passing.
      //
      // Other tests ensure that error code >=2 are handled appropriately.
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
// PR check on another repo
// ----------------------------------------------------------------------------

// The two jobs below work together and with the semgrep/e2e repo
// to check that 'semgrep ci' reports only new findings on PRs.
// This 'e2e' repo contains:
//  - a file-under-test.py with a '10 == 10' that
//    should be reported by semgrep (by the '$X == $X' semgrep rule)
//  - a .github/workflows/semgrep-ci-e2e.yml that runs 'semgrep ci'
//    with the docker image specified in semgrep-docker-image-tag.txt
//  - a scripts/change-version.sh that updates semgrep-docker-image-tag.txt
//    and modifies a file that is not file-under-test.py
//
// Because semgrep ci reports only new findings, the goal of the jobs below is to
// make sure no finding are reported since we don't modify file-under-test.py

// just open a PR on the semgrep/e2e repo
local semgrep_ci_on_pr_job = {
  'runs-on': 'ubuntu-22.04',
  needs: 'get-inputs',
  outputs: {
    'pr-number': "${{ steps.open-pr.outputs.pr-number }}",
  },
  steps: semgrep.github_bot.get_token_steps + [
    {
      uses: 'actions/checkout@v3',
      with: {
        repository: 'semgrep/e2e',
        ref: '${{ github.event.repository.default_branch }}',
        token: semgrep.github_bot.token_ref,
      },
    },
    {
      name: 'Prepare the PR',
      run: |||
        git checkout -b e2e-test-pr-${{ github.run_id }}
        scripts/change-version.sh %s
        %s
        git add --all
        git commit -m "chore: Bump version to %s"
        git push --set-upstream origin e2e-test-pr-${{ github.run_id }}
      ||| % [ docker_tag, gha.git_config_user, docker_tag],
    },
    {
      name: 'Make the PR',
      id: 'open-pr',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: |||
          PR_URL=$(gh pr create --title "chore: fake PR for %s" --body "Fake PR" --base "develop" --head "e2e-test-pr-${{ github.run_id }}")
          PR_NUMBER=$(echo $PR_URL | sed 's|.*pull/\(.*\)|\1|')
          echo "pr-number=$PR_NUMBER" >> $GITHUB_OUTPUT
      ||| % [docker_tag],
    },
  ],
};


// TODO: factorize with start-release.jsonnet
local len_checks = "$(gh pr -R semgrep/e2e view %s --json statusCheckRollup --jq '.statusCheckRollup | length')" % pr_number;

// TODO: factorize with start-release.jsonnet
local wait_for_checks_job = {
  'runs-on': 'ubuntu-22.04',
  needs: 'semgrep-ci-on-pr',
  steps: semgrep.github_bot.get_token_steps + [
    {
      name: 'Wait for checks to register',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: |||
        LEN_CHECKS=%s;
        while [ ${LEN_CHECKS} = "0" ]; do
          echo "No checks available yet"
          sleep 30
          LEN_CHECKS=%s;
        done
        echo "checks are valid"
        echo ${LEN_CHECKS}
        gh pr -R semgrep/e2e view %s --json statusCheckRollup
      ||| % [len_checks, len_checks, pr_number],
    },
    {
      name: 'Wait for checks to complete',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: 'gh pr -R semgrep/e2e checks %s --interval 30 --watch' % pr_number,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'test-e2e-semgrep-ci',
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
    // the two jobs below work together
    'semgrep-ci-on-pr': semgrep_ci_on_pr_job,
    'wait-for-checks': wait_for_checks_job,
    'notify-failure':
       semgrep.slack.notify_failure_job
        ("The End to end semgrep ci workflow failed with docker tag %s. The PR in `semgrep/e2e` that had the failure was %s. See https://github.com/semgrep/semgrep/actions/workflows/test-e2e-semgrep-ci.yml for more info" % [docker_tag, pr_number])
       + { needs: [
          'semgrep-ci',
          'semgrep-ci-on-pr',
          'semgrep-ci-fail-open',
          'semgrep-ci-fail-open-blocking-findings',
          'wait-for-checks',
          'get-inputs',
          ] },
  },
}

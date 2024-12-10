// Release workflow part 1 (part 2 is release.jsonnet).
//
// This workflow is used interactively when we want to trigger a new release;
// most of the release is automated thanks to this workflow and release.jsonnet
// which is triggered when start-release.jsonnet creates a PR for a new release.
// See also:
//  - the 'release:' rule in the toplevel Makefile which is used here to
//     prepare the release branch
//  - scripts/release/ which contains helper scripts for the release
//    (e.g., towncrier to manage the changelog)
//  - scripts/validate-docker-release.sh

local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Constants
// ----------------------------------------------------------------------------

// This is one of the inputs to the workflow.
local version = '${{ github.event.inputs.semgrep-version }}';
// This is computed by the release_setup_job (e.g., "9545")
// and can be referenced from other jobs.
local pr_number = '"${{ needs.release-setup.outputs.pr-number }}"';

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------
// To be used by the workflow.

local input = {
  inputs: {
    'semgrep-version': {
      type: 'string',
      description: 'This is the version that is about to be released and should be what the previous version bump step set the OSS version to in the previous step. This is only really required as a safety check, failing to get the version correct here will only cause this step to fail and should not break anything. For example: "1.42.0"',
      required: true,
    },
    'dry-run': {
      required: true,
      type: 'boolean',
      description: |||
        Check the box for a dry-run - A dry-run will not push any external state
        (branches, tags, images, or PyPI packages).
      |||,
      default: false,
    },
  },
};

local unless_dry_run = {
  'if': '${{ ! inputs.dry-run }}',
};

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

// Open PRs to bump the semgrep version in semgrep-app, semgrep-rpc,
// and semgrep-action. Indeed, those repos reference a particular
// version of Semgrep.
//alt: could move to libs/semgrep.libsonnet
local bump_job(repository) = {
  'runs-on': 'ubuntu-22.04',
  needs: [
    'check-version',
    // we open PRs only when we're sure the release is valid
    'validate-release-trigger',
    'release-setup',
  ],
  steps: semgrep.github_bot.get_token_steps + [
    {
      name: 'Bump semgrep version in %s' % repository,
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
	VERSION: version,
      },
      // Initiate run of workflow (non-blocking)
      // bump_version.yml is defined in semgrep-app/semgrep-rpc/semgrep-action
      run: 'gh workflow run bump_version.yml --repo "%s" --raw-field version="$VERSION"' % repository,
    },
  ],
} + unless_dry_run;

// ----------------------------------------------------------------------------
// Wait PR checks helpers
// ----------------------------------------------------------------------------

local len_checks = "$(gh pr -R semgrep/semgrep view %s --json statusCheckRollup --jq '.statusCheckRollup | length')" % pr_number;

local wait_pr_checks_to_register(while_cond) = |||
  LEN_CHECKS=%s;
  while [ ${LEN_CHECKS} = "%s" ]; do
    echo "No checks available yet"
    sleep 1
    LEN_CHECKS=%s;
  done
  echo "checks are valid"
  echo ${LEN_CHECKS}
  gh pr -R semgrep/semgrep view %s --json statusCheckRollup
||| % [len_checks, while_cond, len_checks, pr_number];

local wait_pr_checks_to_register_step(while_cond) = {
  name: 'Wait for checks to register',
  env: {
    GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
  },
  run: wait_pr_checks_to_register(while_cond),
};

local wait_pr_checks_to_complete_step = {
  name: 'Wait for checks to complete',
  id: 'wait-checks',
  env: {
    GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
  },
  // Wait for PR checks to finish
  run: 'gh pr -R semgrep/semgrep checks %s --interval 90 --watch' % pr_number,
};

local get_current_num_checks_step = {
  name: 'Get Current Num Checks',
  id: 'num-checks',
  env: {
    GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
  },
  run: |||
    LEN_CHECKS=%s;
    echo "num-checks=${LEN_CHECKS}" >> $GITHUB_OUTPUT
  ||| % len_checks,
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------
local check_version_job = {
  name: "Check Semgrep Version",
  'runs-on': 'ubuntu-22.04',
  permissions: gha.write_permissions,
  env: {
    VERSION: version,
  },
  steps: [
    {
      uses: 'actions/checkout@v4',
      with: {
	'sparse-checkout': |||
           src/core/Version.ml
        |||,
      },
    },
    {
      run: |||
        grep -F "let version = \"${VERSION}\"" src/core/Version.ml
      |||
    },
  ],
};

// make sure semgrep-pro was released and that we have the right
// semgrep-core-proprietary in the right S3 bucket
local check_semgrep_pro_job = {
  needs: [
    'check-version',
  ],
  name: 'Check Semgrep Pro Manifest',
  secrets: 'inherit',
  uses: './.github/workflows/check-semgrep-pro-version.yml',
  // coupling: with pro-release.jsonnet in semgrep-pro
  with: {
    'bucket-name': 'deep-semgrep-artifacts',
    'manifest-key': 'versions-manifest.json',
    'semgrep-version': version,
    'dry-run': '${{ inputs.dry-run }}',
  },
};

local release_branch = 'release-%s' % version;

// make the Release PR
local release_setup_job = {
  needs: [
    'check-version',
    'check-semgrep-pro',
  ],
  'runs-on': 'ubuntu-20.04',
  outputs: {
    // other jobs can refer to this output via 'pr_number' constant above
    'pr-number': '${{ steps.open-pr.outputs.pr-number }}',
  },
  env: {
    BRANCH: release_branch,
    VERSION: version,
  },
  // TODO: again why we need this token? we release from
  // the repo of the workflow, can't we just checkout?
  steps: semgrep.github_bot.get_token_steps + [
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: 'recursive',
        ref: '${{ github.event.repository.default_branch }}',
        token: semgrep.github_bot.token_ref,
      },
    },
    {
      run: 'git checkout -b "${BRANCH}"'
    },
    {
      name: 'Push release branch',
      run: |||
        %s
        git add --all
        git commit --allow-empty -m "chore: release version ${VERSION}"
        git push --set-upstream origin ${BRANCH}
      ||| % gha.git_config_user,
    } + unless_dry_run,
    {
      name: 'Create PR',
      id: 'open-pr',
      env: {
        SOURCE: release_branch,
        TARGET: '${{ github.event.repository.default_branch }}',
        TITLE: 'Release Version %s' % version,
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: |||
        # check if the branch already has a pull request open

        if gh pr list --head ${SOURCE} | grep -vq "no pull requests"; then
            echo "pull request from SOURCE ${SOURCE} to TARGET ${TARGET} is already open";
            echo "cancelling release"
            exit 1
        fi

        # open new pull request with the body of from the local template.
        PR_URL=$(gh pr create --title "${TITLE}" --body-file ./.github/PULL_REQUEST_TEMPLATE/release_pr_template.md \
          --base "${TARGET}" --head "${SOURCE}")

        echo $PR_URL

        # GHA doesn't provide an easy way to determine this, so we
        # capture the number and go from there.
        PR_NUMBER=$(echo $PR_URL | sed 's|.*pull/\(.*\)|\1|')

        echo "pr-number=$PR_NUMBER" >> $GITHUB_OUTPUT
      |||,
    } + unless_dry_run,
  ],
};

local wait_for_pr_checks_job = {
  'runs-on': 'ubuntu-20.04',
  needs: [
    'check-version',
    'check-semgrep-pro',
    'release-setup',
  ],
  outputs: {
    // to be used by wait_for_release_checks_job
    'num-checks': '${{ steps.num-checks.outputs.num-checks }}',
  },
  steps: [
    // Immediately after creation, the PR doesn't have any checks attached
    // yet, wait until this is not the case. If you immediately start waiting
    // for checks, then it just fails saying there's no checks.
    wait_pr_checks_to_register_step('0'),
    wait_pr_checks_to_complete_step,
    // Once all checks are complete on the PR, find the number of checks
    // so that we can wait for the new checks to register. We can't do this
    // above because sometimes all checks aren't yet ready by the time the
    // first one is, so we end up in a case where we aren't getting waiting
    // for all checks.
    get_current_num_checks_step,
  ],
} + unless_dry_run;

local create_tag_job = {
  'runs-on': 'ubuntu-20.04',
  needs: [
    'check-version',
    'check-semgrep-pro',
    'release-setup',
    'wait-for-pr-checks',
  ],
  // TODO? why special token again?
  steps: semgrep.github_bot.get_token_steps + [
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
        // checkout the release branch this time
        ref: release_branch,
        token: semgrep.github_bot.token_ref,
      },
    },
    // pushing on a vxxx branch will trigger the release.jsonnet workflow?
    {
      name: 'Create semgrep release version tag',
      env: {
	VERSION: version
      },
      run: |||
        %s
        git tag -a -m "Release ${VERSION}" "v${VERSION}"
        git push origin "v${VERSION}"
      ||| % gha.git_config_user,
    },
    {
      name: 'Create semgrep-interfaces release version tag',
      env: {
	VERSION: version
      },
      run: |||
        cd cli/src/semgrep/semgrep_interfaces
        %s
        git tag -a -m "Release ${VERSION}" "v${VERSION}"
        git push origin "v${VERSION}"
      ||| % gha.git_config_user,
    },
  ],
} + unless_dry_run;

local create_draft_release_job = {
  'runs-on': 'ubuntu-20.04',
  needs: [
    'check-version',
    'release-setup',
    'create-tag',
    'wait-for-pr-checks',
  ],
  steps: semgrep.github_bot.get_token_steps + [
    // TODO I (Andre) couldn't figure out how to save the
    // release_changes.md as an asset in one workflow and download in
    // another. I am sure it is possible, but it isn't easy.  Instead
    // I just made an additional file contained in the repo called
    // OSS/release_changes.md .
    {
      uses: 'actions/checkout@v3',
      with: {
        ref: release_branch,
        token: semgrep.github_bot.token_ref,
      },
    },
    {
      name: 'Create Draft Release Semgrep',
      uses: 'softprops/action-gh-release@v1',
      with: {
        tag_name: 'v%s' % version,
        name: 'Release v%s' % version,
        body_path: 'release_changes.md',
	// Why not use the semgrep.github_bot.token_ref?
        token: '${{ secrets.GITHUB_TOKEN }}',
        prerelease: false,
        draft: true,
      },
    },
    {
      name: 'Create Draft Release Semgrep Interfaces',
      uses: 'softprops/action-gh-release@v1',
      with: {
        tag_name: 'v%s' % version,
        name: 'Release v%s' % version,
        body_path: 'release_changes.md',
        token: semgrep.github_bot.token_ref,
        prerelease: false,
        draft: true,
        repository: 'semgrep/semgrep-interfaces',
      },
    },
  ],
} + unless_dry_run;

// similar to wait_for_pr_checks_job
local wait_for_release_checks_job = {
  'runs-on': 'ubuntu-20.04',
  needs: [
    'release-setup',
    'wait-for-pr-checks',
    'create-tag',
  ],
  steps: [
    // We need to wait for the new checks to register when the release tag is pushed
    wait_pr_checks_to_register_step('${{ needs.wait-for-pr-checks.outputs.num-checks }}'),
    wait_pr_checks_to_complete_step,
  ],
} + unless_dry_run;

local validate_release_trigger_job = {
  needs: [
    'check-version',
    'wait-for-release-checks',
    'release-setup',
  ],
  'runs-on': 'ubuntu-latest',
  env: {
    VERSION: version
  },
  steps: [
    {
      uses: 'actions/checkout@v3',
      // I don't think it's important to checkout here this specific version;
      // the actual important check is done in the script below.
      with: {
        ref: 'v%s' % version,
      },
    },
    {
      run: './scripts/validate-docker-release.sh $VERSION',
    },
  ],
} + unless_dry_run;

// ----------------------------------------------------------------------------
// Success/failure notifications
// ----------------------------------------------------------------------------

local notify_success_job = {
  'if': '${{ success() && ! inputs.dry-run }}',
  needs: [
    'check-version',
    'release-setup',
    'validate-release-trigger',
    'bump-semgrep-action',
    'bump-semgrep-rpc',
    'bump-semgrep-app',
    'bump-semgrep-vscode',
    'bump-semgrep-intellij',
  ],
  'runs-on': 'ubuntu-20.04',
  env: {
    VERSION: version,
  },
  steps: [
    {
      run: 'echo "${VERSION}"',
    },
    {
      name: 'Notify Success on Twitter',
      // TODO: seems to not work anymore; the last release announced
      // on twitter on the "semgrep release notifications" account are
      // for 1.38.3
      env: {
	VERSION: version
      },
      run: |||
        # POST a webhook to Zapier to allow for public notifications to our users via Twitter
        curl "${{ secrets.ZAPIER_WEBHOOK_URL }}" \
          -d '{"version":"${VERSION}","changelog_url":"https://github.com/semgrep/semgrep/releases/tag/v${VERSION}"}'
      |||
    },
    {
      name: 'Notify Success on Slack',
      run: semgrep.slack.curl_notify("Release Validation for ${VERSION} has succeeded! Please review the PRs in semgrep-app, semgrep-rpc, and semgrep-action that were generated by this workflow."),
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'start-release',
  on: {
    workflow_dispatch: input,
  },
  // These extra permissions are needed by some of the jobs, e.g. check-semgrep-pro.
  permissions: gha.write_permissions,
  jobs: {
    'check-version': check_version_job,
    'check-semgrep-pro': check_semgrep_pro_job,
    'release-setup': release_setup_job,
    'wait-for-pr-checks': wait_for_pr_checks_job,
    'create-tag': create_tag_job,
    'create-draft-release': create_draft_release_job,
    'wait-for-release-checks': wait_for_release_checks_job,
    'validate-release-trigger': validate_release_trigger_job,
    'bump-semgrep-app': bump_job('semgrep/semgrep-app'),
    'bump-semgrep-action': bump_job('semgrep/semgrep-action'),
    'bump-semgrep-pre-commit': bump_job('semgrep/pre-commit'),
    'bump-semgrep-rpc': bump_job('semgrep/semgrep-rpc'),
    'bump-semgrep-vscode': bump_job('semgrep/semgrep-vscode'),
    'bump-semgrep-intellij': bump_job('semgrep/semgrep-intellij'),
    'notify-success': notify_success_job,
    'notify-failure':
      semgrep.slack.notify_failure_job(
        "Release Validation has failed for version ${VERSION}. Please see https://github.com/${{github.repository}}/actions/runs/${{github.run_id}} for more details!"
         ) + {
         env: {
	  VERSION: version
         },
         'if': '${{ failure() && ! inputs.dry-run }}',
          needs: [
            'check-version',
            'release-setup',
            'validate-release-trigger',
            'bump-semgrep-action',
            'bump-semgrep-pre-commit',
            'bump-semgrep-rpc',
            'bump-semgrep-app',
            'bump-semgrep-vscode',
            'bump-semgrep-intellij',
        ],
      },
  },
}

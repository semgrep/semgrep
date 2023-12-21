// This workflow is used interactively when we want to trigger a new release.
// Most of the release is automated thanks to this workflow and release.jsonnet
// which is triggered when start-release.jsonnet creates a PR for a new release.
// See also the 'release:' rule in the toplevel Makefile which is used here to
// prepare the release branch and scripts/release/ which contains helper scripts
// for the release (e.g., towncrier to manage the changelog).
//
// LATER:
//  - remove intermediate SEMGREP_RELEASE_NEXT_VERSION, use ref to the step
//  - remove step.release-branch, use directly release-%s % version

local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Constants
// ----------------------------------------------------------------------------

// This is computed by the get_version_job (e.g., "1.55.0")
// and can be referenced from other jobs.
local version = '${{ needs.get-version.outputs.version }}';
// This is computed by the release_setup_job (e.g., "9545")
// and can be referenced from other jobs.
local pr_number = '"${{ needs.release-setup.outputs.pr-number }}"';
// When we use git directly instead of gh.
local git_config_user = |||
        git config user.name ${{ github.actor }}
        git config user.email ${{ github.actor }}@users.noreply.github.com
|||;
// For towncrier setup in scripts/release/
local pipenv_setup = |||
        pip3 install pipenv==2022.6.7
        pipenv install --dev
 |||;

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------
// To be used by the workflow.
local input = {
  inputs: {
    bumpVersionFragment: {
      description: 'Version fragment to bump',
      required: true,
      // These options are passed directly into
      // christian-draeger/increment-semantic-version in the `next-vesion`
      // step to decide which of X.Y.Z to increment
      type: 'choice',
      options: [
        // Many folks are concerned about a mis-click and releasing 2.0 which
        // is why we commented the 'major' option below
        // 'major', // x.0.0
        'feature',  // 1.x.0
        'bug',  // 1.1.x
      ],
      default: 'feature',
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
// this will post on Slack
local curl_notify(message) = |||
    curl --request POST \
    --url  ${{ secrets.NOTIFICATIONS_URL }} \
    --header 'content-type: application/json' \
    --data '{
      "version": "%s",
      "message": "%s"
    }'
  ||| % [version, message];

local bump_job(repository) = {
  needs: [
    'get-version',
    'validate-release-trigger',
    'release-setup',
  ],
  // TODO: jsonnetify this and use simple function instead of call/inherit
  uses: './.github/workflows/call-bump-pr-workflow.yml',
  secrets: 'inherit',
  with: {
    version: version,
    repository: repository,
  },
} + unless_dry_run;

// ----------------------------------------------------------------------------
// Wait PR checks helpers
// ----------------------------------------------------------------------------

local len_checks = "$(gh pr -R returntocorp/semgrep view %s --json statusCheckRollup --jq '.statusCheckRollup | length')" % pr_number;

local wait_pr_checks_to_register(while_cond) = |||
        LEN_CHECKS=%s;
          while [ ${LEN_CHECKS} = "%s" ]; do
            echo "No checks available yet"
            sleep 1
            LEN_CHECKS=%s;
          done
          echo "checks are valid"

          echo ${LEN_CHECKS}

          gh pr -R returntocorp/semgrep view %s --json statusCheckRollup
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
  run: 'gh pr -R returntocorp/semgrep checks %s --interval 90 --watch' % pr_number,
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
// This job just infers the next release version (e.g., 1.53.1) based on past
// git tags in the semgrep repo and the workflow input choice (feature vs bug)
// and using christian-draeger/increment-semantic-version to compute it.
local get_version_job = {
  'runs-on': 'ubuntu-20.04',
  outputs: {
    // other jobs can refer to this output via the 'version' constant above
    version: '${{ steps.next-version.outputs.next-version }}',
  },
  steps: [
    //TODO: why we need a special token? Can't we just do the default checkout?
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: 'recursive',
        ref: '${{ github.event.repository.default_branch }}',
        token: semgrep.github_bot.token_ref,
      },
    },
    // Note that checkout@v3 does not get the tags by default. It does if you do
    // "full" checkout, which is too heavyweight. We don't want all branches and
    // everything that ever existed on the repo, so we just do a lightweight checkout
    // and then get the tags ourselves. Also we don't need the tags in submodules.
    {
      name: 'Pull Tags',
      run: "git fetch --no-recurse-submodules origin 'refs/tags/*:refs/tags/*'",
    },
    {
      name: 'Get latest version',
      id: 'latest-version',
      run: |||
        LATEST_TAG=$(git tag --list "v*.*.*" | sort -V | tail -n 1 | cut -c 2- )
        echo "latest-version=${LATEST_TAG}" >> $GITHUB_OUTPUT
      |||,
    },
    {
      name: 'Bump release version',
      id: 'next-version',
      uses: 'christian-draeger/increment-semantic-version@68f14f806a9800fe17433287c35226fd8fd60201',
      with: {
        'current-version': '${{ steps.latest-version.outputs.latest-version }}',
        'version-fragment': '${{ github.event.inputs.bumpVersionFragment }}',
      },
    },
  ],
};

// make sure semgrep-pro was released and that we have the right
// semgrep-core-proprietary in the right S3 bucket
local check_semgrep_pro_job = {
  needs: [
    'get-version',
  ],
  name: 'Check Semgrep Pro Manifest',
  secrets: 'inherit',
  uses: './.github/workflows/check-semgrep-pro-version.yml',
  with: {
    'bucket-name': 'deep-semgrep-artifacts',
    'manifest-key': 'versions-manifest.json',
    'semgrep-version': version,
    'dry-run': '${{ inputs.dry-run }}',
  },
};

// make the Release PR
local release_setup_job = {
  needs: [
    'get-version',
    'check-semgrep-pro',
  ],
  'runs-on': 'ubuntu-20.04',
  outputs: {
    // other jobs can refer to this output via 'pr_number' constant above
    'pr-number': '${{ steps.open-pr.outputs.pr-number }}',
    // TODO: delete, can be derived from version, it's release-<version>
    'release-branch': '${{ steps.release-branch.outputs.release-branch }}',
  },
  steps: [
    // TODO: again why we need this token? we release from
    // the repo of the workflow, can't we just checkout?
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: 'recursive',
        ref: '${{ github.event.repository.default_branch }}',
        token: semgrep.github_bot.token_ref,
      },
    },
    {
      name: 'Create release branch',
      id: 'release-branch',
      run: |||
        RELEASE_BRANCH="release-%s"
        git checkout -b ${RELEASE_BRANCH}
        echo "release-branch=${RELEASE_BRANCH}" >> $GITHUB_OUTPUT
      ||| % version,
    },
    {
      env: {
        SEMGREP_RELEASE_NEXT_VERSION: version,
      },
      run: 'make release',
    },
    // for towncrier
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.10',
        cache: 'pipenv',
        'cache-dependency-path': 'scripts/release/Pipfile.lock',
      },
    },
    {
      name: 'Create GitHub Release Body',
      'working-directory': 'scripts/release',
      run: |||
        %s
        pipenv run towncrier build --draft --version %s > release_body.txt
      ||| % [pipenv_setup, version],
    } + unless_dry_run,
    {
      name: 'Upload Changelog Body Artifact',
      uses: 'actions/upload-artifact@v3',
      with: {
        name: 'release_body_%s' % version,
        path: 'scripts/release/release_body.txt',
      },
    } + unless_dry_run,
    {
      name: 'Update Changelog',
      'working-directory': 'scripts/release',
      // use || true below since modifications mean exit code != 0
      run: |||
        %s
        pipenv run towncrier build --yes --version %s
        pipenv run pre-commit run --files ../../CHANGELOG.md --config ../../.pre-commit-config.yaml || true
      ||| % [pipenv_setup, version],
    },
    {
      name: 'Push release branch',
      // LATER: can probably simplify and use directly version instead
      // of intermediate env
      env: {
        SEMGREP_RELEASE_NEXT_VERSION: version,
      },
      run: |||
        %s
        git add --all
        git commit -m "chore: Bump version to ${SEMGREP_RELEASE_NEXT_VERSION}"
        git push --set-upstream origin ${{ steps.release-branch.outputs.release-branch }}
      ||| % git_config_user,
    } + unless_dry_run,
    {
      name: 'Create PR',
      id: 'open-pr',
      env: {
        SOURCE: '${{ steps.release-branch.outputs.release-branch }}',
        TARGET: '${{ github.event.repository.default_branch }}',
        TITLE: 'Release Version %s' % version,
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: |||
        # check if the branch already has a pull request open

        if gh pr list --head ${SOURCE} | grep -vq "no pull requests"; then
            # pull request already open
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
    'get-version',
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
    wait_pr_checks_to_register_step("0"),
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
    'get-version',
    'check-semgrep-pro',
    'release-setup',
    'wait-for-pr-checks',
  ],
  steps: [
    // TODO? why special token again?
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
        // checkout the release branch this time
        ref: '${{ needs.release-setup.outputs.release-branch }}',
        token: semgrep.github_bot.token_ref,
      },
    },
    // pushing on a vxxx branch will trigger the release.jsonnet workflow?
    {
      name: 'Create semgrep release version tag',
      run: |||
        %s
        git tag -a -m "Release %s" "v%s"
        git push origin "v%s"
      ||| % [git_config_user, version, version, version],
    },
    {
      name: 'Create semgrep-interfaces release version tag',
      run: |||
        cd cli/src/semgrep/semgrep_interfaces
        %s
        git tag -a -m "Release %s" "v%s"
        git push origin "v%s"
      ||| % [git_config_user, version, version, version],
    },
  ],
} + unless_dry_run;

local create_draft_release_job = {
  'runs-on': 'ubuntu-20.04',
  needs: [
    'get-version',
    'release-setup',
    'create-tag',
    'wait-for-pr-checks',
  ],
  steps: [
    {
      name: 'Download Release Body Artifact',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'release_body_%s' % version,
        path: 'scripts/release',
      },
    },
    {
      name: 'Create Draft Release Semgrep',
      uses: 'softprops/action-gh-release@v1',
      with: {
        tag_name: 'v%s' % version,
        name: 'Release v%s' % version,
        body_path: 'scripts/release/release_body.txt',
        token: '${{ secrets.GITHUB_TOKEN }}',
        prerelease: false,
        draft: true,
      },
    },
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
    {
      name: 'Create Draft Release Semgrep Interfaces',
      uses: 'softprops/action-gh-release@v1',
      with: {
        tag_name: 'v%s' % version,
        name: 'Release v%s' % version,
        body_path: 'scripts/release/release_body.txt',
        token: semgrep.github_bot.token_ref,
        prerelease: false,
        draft: true,
        repository: 'returntocorp/semgrep-interfaces',
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
    wait_pr_checks_to_register_step("${{ needs.wait-for-pr-checks.outputs.num-checks }}"),
    wait_pr_checks_to_complete_step,
  ],
} + unless_dry_run;

local validate_release_trigger_job = {
  needs: [
    'get-version',
    'wait-for-release-checks',
    'release-setup',
  ],
  uses: './.github/workflows/validate-release.yml',
  secrets: 'inherit',
  with: {
    version: version,
  },
} + unless_dry_run;

// ----------------------------------------------------------------------------
// Success/failure notifications
// ----------------------------------------------------------------------------

local notify_success_job = {
  'if': '${{ success() && ! inputs.dry-run }}',
  needs: [
    'get-version',
    'release-setup',
    'validate-release-trigger',
    'bump-semgrep-action',
    'bump-semgrep-rpc',
    'bump-semgrep-app',
  ],
  'runs-on': 'ubuntu-20.04',
  steps: [
    {
      run: 'echo "%s"' % version,
    },
    {
      name: 'Notify Success on Twitter',
      // TODO: seems to not work anymore; the last release announced
      // on twitter on the "semgrep release notifications" account are
      // for 1.38.3
      run: |||
        # POST a webhook to Zapier to allow for public notifications to our users via Twitter
        curl "${{ secrets.ZAPIER_WEBHOOK_URL }}" \
          -d '{"version":"%s","changelog_url":"https://github.com/returntocorp/semgrep/releases/tag/v%s"}'
      ||| % [version, version]
    },
    {
      name: 'Notify Success on Slack',
      run: curl_notify("Release Validation has succeeded! Please review the PRs in semgrep-app, semgrep-rpc, and semgrep-action that were generated by this workflow."),
    },
  ],
};

local notify_failure_job = {
  'if': '${{ failure() && ! inputs.dry-run }}',
  needs: [
    'get-version',
    'release-setup',
    'validate-release-trigger',
    'bump-semgrep-action',
    'bump-semgrep-rpc',
    'bump-semgrep-app',
  ],
  'runs-on': 'ubuntu-20.04',
  steps: [
    {
      name: 'Notify Failure',
      run: curl_notify("Release Validation has failed. Please see https://github.com/${{github.repository}}/actions/runs/${{github.run_id}} for more details!"),
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
  jobs: {
    'get-version': get_version_job,
    'check-semgrep-pro': check_semgrep_pro_job,
    'release-setup': release_setup_job,
    'wait-for-pr-checks': wait_for_pr_checks_job,
    'create-tag': create_tag_job,
    'create-draft-release': create_draft_release_job,
    'wait-for-release-checks': wait_for_release_checks_job,
    'validate-release-trigger': validate_release_trigger_job,
    'bump-semgrep-app': bump_job("semgrep/semgrep-app"),
    'bump-semgrep-action': bump_job("semgrep/semgrep-action"),
    'bump-semgrep-rpc': bump_job("semgrep/semgrep-rpc"),
    'notify-success': notify_success_job,
    'notify-failure': notify_failure_job,
  },
}

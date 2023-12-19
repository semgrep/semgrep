// This workflow is used interactively when we want to trigger a new
// release. Most of the release is automated thanks to this workflow.
// See also release.jsonnet which is triggered when start-release.jsonnet
// creates a PR for a new release.
// See also the 'release:' rule in the toplevel Makefile which is used here to
// prepare the release branch.

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------
local get_version_job = {
  name: 'Get Next version',
  'runs-on': 'ubuntu-20.04',
  outputs: {
    version: '${{ steps.next-version.outputs.next-version }}',
  },
  steps: [
    {
      name: 'Get JWT for semgrep-ci GitHub App',
      id: 'jwt',
      uses: 'docker://public.ecr.aws/y9k7q4m1/devops/cicd:latest',
      env: {
        EXPIRATION: 600,
        ISSUER: '${{ secrets.SEMGREP_CI_APP_ID }}',
        PRIVATE_KEY: '${{ secrets.SEMGREP_CI_APP_KEY }}',
      },
    },
    {
      name: 'Get token for semgrep-ci GitHub App',
      id: 'token',
      run: 'TOKEN="$(curl -X POST \\\n-H "Authorization: Bearer ${{ steps.jwt.outputs.jwt }}" \\\n-H "Accept: application/vnd.github.v3+json" \\\n"https://api.github.com/app/installations/${{ secrets.SEMGREP_CI_APP_INSTALLATION_ID }}/access_tokens" | \\\njq -r .token)"\necho "::add-mask::$TOKEN"\necho "token=$TOKEN" >> $GITHUB_OUTPUT\n',
    },
    {
      name: 'Check out code',
      uses: 'actions/checkout@v3',
      id: 'checkout',
      with: {
        submodules: 'recursive',
        ref: '${{ github.event.repository.default_branch }}',
        token: '${{ steps.token.outputs.token }}',
      },
    },
    {
      name: 'Pull Tags',
      id: 'pull-tags',
      run: "git fetch --no-recurse-submodules origin 'refs/tags/*:refs/tags/*'",
    },
    {
      name: 'Get latest version',
      id: 'latest-version',
      run: 'LATEST_TAG=$(git tag --list "v*.*.*" | sort -V | tail -n 1 | cut -c 2- )\necho "latest-version=${LATEST_TAG}" >> $GITHUB_OUTPUT\n',
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
    'semgrep-version': '${{ needs.get-version.outputs.version }}',
    'dry-run': '${{ inputs.dry-run }}',
  },
};

local release_setup_job = {
  name: 'Setup Release',
  needs: [
    'get-version',
    'check-semgrep-pro',
  ],
  'runs-on': 'ubuntu-20.04',
  outputs: {
    'pr-number': '${{ steps.open-pr.outputs.pr-number }}',
    'release-branch': '${{ steps.release-branch.outputs.release-branch }}',
  },
  steps: [
    {
      name: 'Get JWT for semgrep-ci GitHub App',
      id: 'jwt',
      uses: 'docker://public.ecr.aws/y9k7q4m1/devops/cicd:latest',
      env: {
        EXPIRATION: 600,
        ISSUER: '${{ secrets.SEMGREP_CI_APP_ID }}',
        PRIVATE_KEY: '${{ secrets.SEMGREP_CI_APP_KEY }}',
      },
    },
    {
      name: 'Get token for semgrep-ci GitHub App',
      id: 'token',
      run: 'TOKEN="$(curl -X POST \\\n-H "Authorization: Bearer ${{ steps.jwt.outputs.jwt }}" \\\n-H "Accept: application/vnd.github.v3+json" \\\n"https://api.github.com/app/installations/${{ secrets.SEMGREP_CI_APP_INSTALLATION_ID }}/access_tokens" | \\\njq -r .token)"\necho "::add-mask::$TOKEN"\necho "token=$TOKEN" >> $GITHUB_OUTPUT\n',
    },
    {
      name: 'Check out code',
      uses: 'actions/checkout@v3',
      id: 'checkout',
      with: {
        submodules: 'recursive',
        ref: '${{ github.event.repository.default_branch }}',
        token: '${{ steps.token.outputs.token }}',
      },
    },
    {
      name: 'Create release branch',
      id: 'release-branch',
      run: 'RELEASE_BRANCH="release-${{ needs.get-version.outputs.version }}"\ngit checkout -b ${RELEASE_BRANCH}\necho "release-branch=${RELEASE_BRANCH}" >> $GITHUB_OUTPUT\n',
    },
    {
      name: 'Run `make release`',
      id: 'make-release',
      env: {
        SEMGREP_RELEASE_NEXT_VERSION: '${{ needs.get-version.outputs.version }}',
      },
      run: 'make release',
    },
    {
      name: 'Setup Python',
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.10',
        cache: 'pipenv',
        'cache-dependency-path': 'scripts/release/Pipfile.lock\n',
      },
    },
    {
      name: 'Create GitHub Release Body',
      'if': '${{ ! inputs.dry-run }}',
      'working-directory': 'scripts/release',
      id: 'changelog_output',
      run: 'pip3 install pipenv==2022.6.7\npipenv install --dev\npipenv run towncrier build --draft --version ${{ needs.get-version.outputs.version }} > release_body.txt\n',
    },
    {
      name: 'Upload Changelog Body Artifact',
      'if': '${{ ! inputs.dry-run }}',
      id: 'upload-changelog-artifact',
      uses: 'actions/upload-artifact@v3',
      with: {
        name: 'release_body_${{ needs.get-version.outputs.version }}',
        path: 'scripts/release/release_body.txt',
      },
    },
    {
      name: 'Update Changelog',
      'working-directory': 'scripts/release',
      run: 'pip3 install pipenv==2022.6.7\npipenv install --dev\npipenv run towncrier build --yes --version ${{ needs.get-version.outputs.version }}\n# use || true since modifications mean exit code != 0\npipenv run pre-commit run --files ../../CHANGELOG.md --config ../../.pre-commit-config.yaml || true\n',
    },
    {
      name: 'Push release branch',
      'if': '${{ ! inputs.dry-run }}',
      id: 'push-release-branch',
      env: {
        SEMGREP_RELEASE_NEXT_VERSION: '${{ needs.get-version.outputs.version }}',
      },
      run: 'git config user.name ${{ github.actor }}\ngit config user.email ${{ github.actor }}@users.noreply.github.com\ngit add --all\ngit commit -m "chore: Bump version to ${SEMGREP_RELEASE_NEXT_VERSION}"\ngit push --set-upstream origin ${{ steps.release-branch.outputs.release-branch }}\n',
    },
    {
      name: 'Create PR',
      'if': '${{ ! inputs.dry-run }}',
      id: 'open-pr',
      env: {
        SOURCE: '${{ steps.release-branch.outputs.release-branch }}',
        TARGET: '${{ github.event.repository.default_branch }}',
        TITLE: 'Release Version ${{ needs.get-version.outputs.version }}',
        GITHUB_TOKEN: '${{ steps.token.outputs.token }}',
      },
      run: "# check if the branch already has a pull request open\n\nif gh pr list --head ${SOURCE} | grep -vq \"no pull requests\"; then\n    # pull request already open\n    echo \"pull request from SOURCE ${SOURCE} to TARGET ${TARGET} is already open\";\n    echo \"cancelling release\"\n    exit 1\nfi\n\n# open new pull request with the body of from the local template.\nPR_URL=$(gh pr create --title \"${TITLE}\" --body-file ./.github/PULL_REQUEST_TEMPLATE/release_pr_template.md \\\n  --base \"${TARGET}\" --head \"${SOURCE}\")\n\necho $PR_URL\n\n#GH actions doesn't provide an easy way to determine this, so we capture the number and go from there.\nPR_NUMBER=$(echo $PR_URL | sed 's|.*pull/\\(.*\\)|\\1|')\n\necho \"pr-number=$PR_NUMBER\" >> $GITHUB_OUTPUT\n",
    },
  ],
};

local wait_for_pr_checks_job = {
  'if': '${{ ! inputs.dry-run }}',
  name: 'Wait for PR Checks',
  'runs-on': 'ubuntu-20.04',
  needs: [
    'get-version',
    'check-semgrep-pro',
    'release-setup',
  ],
  outputs: {
    'num-checks': '${{ steps.num-checks.outputs.num-checks }}',
  },
  steps: [
    {
      name: 'Wait for checks to register',
      id: 'register-checks',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
      run: "LEN_CHECKS=$(gh pr -R returntocorp/semgrep view \"${{ needs.release-setup.outputs.pr-number }}\" --json statusCheckRollup --jq '.statusCheckRollup | length');\n\n# Immediately after creation, the PR doesn't have any checks attached yet, wait until this is not the case\n# If you immediately start waiting for checks, then it just fails saying there's no checks.\nwhile [ ${LEN_CHECKS} = \"0\" ]; do\n  echo \"No checks available yet\"\n  sleep 1\n  LEN_CHECKS=$(gh pr -R returntocorp/semgrep view \"${{ needs.release-setup.outputs.pr-number }}\" --json statusCheckRollup --jq '.statusCheckRollup | length');\ndone\necho \"checks are valid\"\n\necho ${LEN_CHECKS}\n\ngh pr -R returntocorp/semgrep view \"${{ needs.release-setup.outputs.pr-number }}\" --json statusCheckRollup\n",
    },
    {
      name: 'Wait for checks to complete',
      id: 'wait-checks',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
      run: '# Wait for PR checks to finish\ngh pr -R returntocorp/semgrep checks "${{ needs.release-setup.outputs.pr-number }}" --interval 90 --watch\n',
    },
    {
      name: 'Get Current Num Checks',
      id: 'num-checks',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
      run: "# Once all checks are complete on the PR, find the number of checks so that we can wait for the new checks to register.\n# We can't do this above because sometimes all checks aren't yet ready by the time the first one is, so we end up in a case where\n# we aren't getting waiting for all checks.\nLEN_CHECKS=$(gh pr -R returntocorp/semgrep view \"${{ needs.release-setup.outputs.pr-number }}\" --json statusCheckRollup --jq '.statusCheckRollup | length');\necho \"num-checks=${LEN_CHECKS}\" >> $GITHUB_OUTPUT\n",
    },
  ],
};

local create_tag_job = {
  'if': '${{ ! inputs.dry-run }}',
  name: 'Create Release Tag',
  'runs-on': 'ubuntu-20.04',
  needs: [
    'get-version',
    'check-semgrep-pro',
    'release-setup',
    'wait-for-pr-checks',
  ],
  steps: [
    {
      name: 'Get JWT for semgrep-ci GitHub App',
      id: 'jwt',
      uses: 'docker://public.ecr.aws/y9k7q4m1/devops/cicd:latest',
      env: {
        EXPIRATION: 600,
        ISSUER: '${{ secrets.SEMGREP_CI_APP_ID }}',
        PRIVATE_KEY: '${{ secrets.SEMGREP_CI_APP_KEY }}',
      },
    },
    {
      name: 'Get token for semgrep-ci GitHub App',
      id: 'token',
      run: 'TOKEN="$(curl -X POST \\\n-H "Authorization: Bearer ${{ steps.jwt.outputs.jwt }}" \\\n-H "Accept: application/vnd.github.v3+json" \\\n"https://api.github.com/app/installations/${{ secrets.SEMGREP_CI_APP_INSTALLATION_ID }}/access_tokens" | \\\njq -r .token)"\necho "::add-mask::$TOKEN"\necho "token=$TOKEN" >> $GITHUB_OUTPUT\n',
    },
    {
      name: 'Check out code',
      uses: 'actions/checkout@v3',
      id: 'checkout',
      with: {
        submodules: true,
        ref: '${{ needs.release-setup.outputs.release-branch }}',
        token: '${{ steps.token.outputs.token }}',
      },
    },
    {
      name: 'Create semgrep release version tag',
      id: 'create-semgrep-tag',
      run: 'git config user.name ${{ github.actor }}\ngit config user.email ${{ github.actor }}@users.noreply.github.com\ngit tag -a -m "Release ${{ needs.get-version.outputs.version }}" "v${{ needs.get-version.outputs.version }}"\ngit push origin "v${{ needs.get-version.outputs.version }}"\n',
    },
    {
      name: 'Create semgrep-interfaces release version tag',
      id: 'create-interfaces-tag',
      run: 'cd cli/src/semgrep/semgrep_interfaces\ngit config user.name ${{ github.actor }}\ngit config user.email ${{ github.actor }}@users.noreply.github.com\ngit tag -a -m "Release ${{ needs.get-version.outputs.version }}" "v${{ needs.get-version.outputs.version }}"\ngit push origin "v${{ needs.get-version.outputs.version }}"\n',
    },
  ],
};

local create_draft_release_job = {
  'if': '${{ ! inputs.dry-run }}',
  name: 'Create Draft Release',
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
      id: 'download_body',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'release_body_${{ needs.get-version.outputs.version }}',
        path: 'scripts/release',
      },
    },
    {
      name: 'Create Draft Release Semgrep',
      id: 'create_release_semgrep',
      uses: 'softprops/action-gh-release@v1',
      with: {
        tag_name: 'v${{ needs.get-version.outputs.version }}',
        name: 'Release v${{ needs.get-version.outputs.version }}',
        body_path: 'scripts/release/release_body.txt',
        token: '${{ secrets.GITHUB_TOKEN }}',
        prerelease: false,
        draft: true,
      },
    },
    {
      name: 'Get JWT for semgrep-ci GitHub App',
      id: 'jwt',
      uses: 'docker://public.ecr.aws/y9k7q4m1/devops/cicd:latest',
      env: {
        EXPIRATION: 600,
        ISSUER: '${{ secrets.SEMGREP_CI_APP_ID }}',
        PRIVATE_KEY: '${{ secrets.SEMGREP_CI_APP_KEY }}',
      },
    },
    {
      name: 'Get token for semgrep-ci GitHub App',
      id: 'token',
      run: 'TOKEN="$(curl -X POST \\\n-H "Authorization: Bearer ${{ steps.jwt.outputs.jwt }}" \\\n-H "Accept: application/vnd.github.v3+json" \\\n"https://api.github.com/app/installations/${{ secrets.SEMGREP_CI_APP_INSTALLATION_ID }}/access_tokens" | \\\njq -r .token)"\necho "::add-mask::$TOKEN"\necho "token=$TOKEN" >> $GITHUB_OUTPUT\n',
    },
    {
      name: 'Create Draft Release Semgrep Interfaces',
      id: 'create_release_semgrep_interfaces',
      uses: 'softprops/action-gh-release@v1',
      with: {
        tag_name: 'v${{ needs.get-version.outputs.version }}',
        name: 'Release v${{ needs.get-version.outputs.version }}',
        body_path: 'scripts/release/release_body.txt',
        token: '${{ steps.token.outputs.token }}',
        prerelease: false,
        draft: true,
        repository: 'returntocorp/semgrep-interfaces',
      },
    },
  ],
};

local wait_for_release_checks_job = {
  'if': '${{ ! inputs.dry-run }}',
  name: 'Wait for Release Checks',
  'runs-on': 'ubuntu-20.04',
  needs: [
    'release-setup',
    'wait-for-pr-checks',
    'create-tag',
  ],
  steps: [
    {
      name: 'Wait for release checks to register',
      id: 'register-release-checks',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
      run: "LEN_CHECKS=$(gh pr -R returntocorp/semgrep view \"${{ needs.release-setup.outputs.pr-number }}\" --json statusCheckRollup --jq '.statusCheckRollup | length');\n\n# We need to wait for the new checks to register when the release tag is pushed\nwhile [ ${LEN_CHECKS} = ${{ needs.wait-for-pr-checks.outputs.num-checks }} ]; do\n  echo \"No checks available yet\"\n  sleep 1\n  LEN_CHECKS=$(gh pr -R returntocorp/semgrep view \"${{ needs.release-setup.outputs.pr-number }}\" --json statusCheckRollup --jq '.statusCheckRollup | length');\ndone\necho \"checks are valid\"\n\necho ${LEN_CHECKS}\n\ngh pr -R returntocorp/semgrep view \"${{ needs.release-setup.outputs.pr-number }}\" --json statusCheckRollup\n",
    },
    {
      name: 'Wait for release checks',
      id: 'wait-release-checks',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
      run: '# Wait for PR checks to finish\ngh pr -R returntocorp/semgrep checks "${{ needs.release-setup.outputs.pr-number }}" --interval 90 --watch\n',
    },
  ],
};

local validate_release_trigger_job = {
  'if': '${{ ! inputs.dry-run }}',
  needs: [
    'get-version',
    'wait-for-release-checks',
    'release-setup',
  ],
  name: 'Trigger Release Validation',
  uses: './.github/workflows/validate-release.yml',
  secrets: 'inherit',
  with: {
    version: '${{needs.get-version.outputs.version}}',
  },
};

local bump_semgrep_app_job = {
  'if': '${{ ! inputs.dry-run }}',
  needs: [
    'get-version',
    'validate-release-trigger',
    'release-setup',
  ],
  name: 'Bump Semgrep App Semgrep Version',
  uses: './.github/workflows/call-bump-pr-workflow.yml',
  secrets: 'inherit',
  with: {
    version: '${{needs.get-version.outputs.version}}',
    repository: 'semgrep/semgrep-app',
  },
};

local bump_semgrep_action_job = {
  'if': '${{ ! inputs.dry-run }}',
  needs: [
    'get-version',
    'validate-release-trigger',
    'release-setup',
  ],
  name: 'Bump Semgrep Action Semgrep Version',
  uses: './.github/workflows/call-bump-pr-workflow.yml',
  secrets: 'inherit',
  with: {
    version: '${{needs.get-version.outputs.version}}',
    repository: 'semgrep/semgrep-action',
  },
};

local bump_semgrep_rpc_job = {
  'if': '${{ ! inputs.dry-run }}',
  needs: [
    'get-version',
    'validate-release-trigger',
    'release-setup',
  ],
  name: 'Bump Semgrep RPC Semgrep Version',
  uses: './.github/workflows/call-bump-pr-workflow.yml',
  secrets: 'inherit',
  with: {
    version: '${{needs.get-version.outputs.version}}',
    repository: 'semgrep/semgrep-rpc',
  },
};

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
  name: 'Notify of Success',
  'runs-on': 'ubuntu-20.04',
  steps: [
    {
      run: 'echo "${{needs.get-version.outputs.version}}"',
    },
    {
      name: 'Notify Success',
      run: "# POST a webhook to Zapier to allow for public notifications to our users via Twitter\ncurl \"${{ secrets.ZAPIER_WEBHOOK_URL }}\" \\\n  -d '{\"version\":\"${{needs.get-version.outputs.version}}\",\"changelog_url\":\"https://github.com/returntocorp/semgrep/releases/tag/v${{needs.get-version.outputs.version}}\"}'\n\ncurl --request POST \\\n--url  ${{ secrets.NOTIFICATIONS_URL }} \\\n--header 'content-type: application/json' \\\n--data '{\n  \"version\": \"${{needs.get-version.outputs.version}}\",\n  \"message\": \"Release Validation has succeeded! Please review the PRs in semgrep-app, semgrep-rpc, and semgrep-action that were generated by this workflow.\"\n}'\n",
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
  name: 'Notify of Failure',
  'runs-on': 'ubuntu-20.04',
  steps: [
    {
      name: 'Notify Failure',
      run: "curl --request POST \\\n--url  ${{ secrets.NOTIFICATIONS_URL }} \\\n--header 'content-type: application/json' \\\n--data '{\n  \"version\": \"${{needs.get-version.outputs.version}}\",\n  \"message\": \"Release Validation has failed. Please see https://github.com/${{github.repository}}/actions/runs/${{github.run_id}} for more details!\"\n}'\n",
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'start-release',
  on: {
    workflow_dispatch: {
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
            'feature', // 1.x.0
            'bug', // 1.1.x
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
    },
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
    'bump-semgrep-app': bump_semgrep_app_job,
    'bump-semgrep-action': bump_semgrep_action_job,
    'bump-semgrep-rpc': bump_semgrep_rpc_job,
    'notify-success': notify_success_job,
    'notify-failure': notify_failure_job,
  },
}

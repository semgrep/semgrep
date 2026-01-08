// Workflow to create a PR to update the Pro repo with changes in OSS.
//
// Note that this workflow does not provide a full sync of OSS to Pro;
// it just takes what is in the HEAD in develop in the OSS repo
// (e.g., the patch of the release that bumps the version) and create a PR
// with it in pro.
// This could be used later also to sync simple contributions to OSS from
// external contributors.
// TODO? in theory we could even move this workflow in Pro? (which makes
// it easier to iterate on)

local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local uses = import 'libs/uses.libsonnet';

// ----------------------------------------------------------------------------
// Main job
// ----------------------------------------------------------------------------

local job = {
  'runs-on': 'ubuntu-latest',
  permissions: gha.write_permissions,
  steps: semgrep.github_bot.get_token_steps + [
    {
      name: 'Checkout OSS',
      uses: uses.actions.checkout,
      with: {
        ref: 'refs/pull/${{ inputs.pr_number }}/head',
        // fetch all history to get base branch and all PR commits
        'fetch-depth': 0,
        // Use the token provided by the JWT token getter above
        token: semgrep.github_bot.token_ref,
      },
    },
    {
      name: 'Checkout PRO',
      uses: uses.actions.checkout,
      with: {
        repository: 'semgrep/semgrep-proprietary',
        path: 'PRO',
        token: semgrep.github_bot.token_ref,
      },
    },
    {
      name: 'Creating the branch and commiting to it',
      env: {
        BRANCHNAME: 'sync-with-PRO-${{ github.run_id }}-${{ github.run_attempt }}',
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
        PR_NUMBER: '${{ inputs.pr_number }}',
      },
      // the git config are needed otherwise GHA complains about
      // unknown identity
      run: |||
        # Get PR information and store it for later
        gh pr view $PR_NUMBER --json title --jq .title > /tmp/pr_title
        gh pr view $PR_NUMBER --json body --jq .body > /tmp/pr_body
        AUTHOR=$(gh pr view $PR_NUMBER --json author --jq .author.login)
        # Add original attribution
        echo "" >> /tmp/pr_body
        echo "Synced from OSS PR https://github.com/semgrep/semgrep/pull/$PR_NUMBER" >> /tmp/pr_body
        echo "Closes https://github.com/semgrep/semgrep/pull/$PR_NUMBER" >> /tmp/pr_body
        echo "Author: @$AUTHOR" >> /tmp/pr_body
        echo "Imported by: @${{ github.actor }}" >> /tmp/pr_body

        BASE_BRANCH=$(gh pr view $PR_NUMBER --json baseRefName --jq .baseRefName)

        # Get the merge base and current HEAD
        git fetch origin $BASE_BRANCH
        MERGE_BASE=$(git merge-base origin/$BASE_BRANCH HEAD)

        # Check if any commits are already synced from Pro
        if git log $MERGE_BASE..HEAD --oneline | grep -q "synced from Pro"; then
           echo "error: PR contains commits that already come from Pro and cannot be synced"
           exit 1
        fi

        # Generate patches for all commits in the PR
        PATCHES=$(git format-patch $MERGE_BASE..HEAD)

        cd PRO
        git config --global user.name "GitHub Actions Bot"
        git config --global user.email "<>"
        git checkout -b $BRANCHNAME

        # Apply all patches
        for patch in $PATCHES; do
          git am --directory=OSS "../$patch"
        done

        git push origin $BRANCHNAME
      |||,
    },
    {
      name: 'Create the Pull request with gh',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: |||
        cd PRO
        PR_TITLE=$(cat /tmp/pr_title)
        PR_BODY=$(cat /tmp/pr_body)

        # Append PR template if it exists
        if [ -f .github/pull_request_template.md ]; then
          TEMPLATE_CONTENT=$(cat .github/pull_request_template.md)
          PR_BODY="$PR_BODY$TEMPLATE_CONTENT"
        fi

        gh pr create --title "$PR_TITLE" --body "$PR_BODY" --base develop
      |||,
    },

  ],
};

// ----------------------------------------------------------------------------
// Workflow
// ----------------------------------------------------------------------------

{
  name: 'sync-with-PRO',
  on: {
    workflow_dispatch: {
      inputs: {
        pr_number: {
          description: 'PR number to sync to PRO (e.g. "11420")',
          required: true,
          type: 'number',
        },
      },
    },
  },
  jobs: {
    job: job,
  },
}

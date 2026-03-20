// Factorize GHA "actions" (=~ plugins) boilerplate.
local gha = import './gha.libsonnet';
local uses = import './uses.libsonnet';

local download_artifact_step(artifact_name, run_id=null, path=null) = {
  uses: uses.actions.download_artifact,
  with: {
    [if path != null then 'path']: path,
    [if artifact_name != null then 'name']: artifact_name,
    [if run_id != null then 'run-id']: run_id,
    [if run_id != null then 'github-token']: '${{ secrets.GITHUB_TOKEN }}',
  },
};

// Gets the run id of a workflow from a specific ref. This is useful for if you
// want to wait for checks on a specific commit to complete, but it's not part
// of a PR
local
  get_workflow_run_id_step(
    sha,
    workflow_file,
    repo='${{ github.repository }}'
  ) = {
    name: 'Get latest workflow id',
    id: 'get_workflow_run_id',

    env: {
      GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      SHA: sha,
      WORKFLOW_FILE: workflow_file,
      REPO: repo,
    },
    // Get the most recent workflow run id from a specific commit sha.
    //
    // This assumes that the workflows are sorted newest first and it
    // appears to be the case in practice. But if for some unusual reason
    // the latest workflow run attempt failed, it will be picked up here.
    // In this case, an easy fix is to identify the failed
    // workflow run on the GitHub website and right-click to delete it. It
    // will then disappear from the list of workflow runs returned by
    // the 'gh api' command below.
    run: |||
      workflow_run_id=$(gh api /repos/${REPO}/actions/workflows/${WORKFLOW_FILE}/runs \
        --method GET -f head_sha=${SHA} \
        -q '.workflow_runs[0].id')
      echo "workflow_run_id=$workflow_run_id" >> $GITHUB_OUTPUT
    |||,
  };
// output from the above step
local workflow_run_id_output = '${{ steps.get_workflow_run_id.outputs.workflow_run_id }}';

// Wait for a workflow to complete successfully
// Use get_workflow_run_id_step above to get the run id if needed
local wait_for_workflow_run(run_id, interval=3, repo='${{ github.repository }}') = {
  name: 'Wait for %s' % run_id,
  env: {
    GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
  },
  run: 'gh run watch "%s" --exit-status -i %s -R "%s"' % [run_id, interval, repo],
};

// Wait for a specific job within a workflow to complete on a given commit
// Unlike wait_for_workflow_run above which waits for an entire workflow run to complete,
// this utility finds the workflow run for a commit and waits for just one job within it
local wait_for_workflow_job_on_commit_step(commit_sha, workflow_name, job_name, timeout_minutes=120) = {
  name: 'Wait for %s job in %s workflow on commit' % [job_name, workflow_name],
  'timeout-minutes': timeout_minutes,
  run: |||
    # Wait for the %(job_name)s job to complete on the given commit
    echo "Waiting for %(job_name)s job to complete on commit: $COMMIT_SHA"

    # Wait for the job to complete
    while true; do
      # Get the run ID for the %(workflow_name)s workflow on the given commit
      RUN_ID=$(gh run list --repo "${{ github.repository }}" --commit "$COMMIT_SHA" --workflow="%(workflow_name)s" --json databaseId --jq ".[0].databaseId" || echo "")

      if [ -n "$RUN_ID" ] && [ "$RUN_ID" != "null" ]; then
        # Check the status of the %(job_name)s job specifically
        JOB_STATUS=$(gh api --paginate repos/${{ github.repository }}/actions/runs/$RUN_ID/jobs --jq ".jobs[] | select(.name == \"%(job_name)s\") | .status" || echo "not_found")

        if [ "$JOB_STATUS" = "completed" ]; then
          echo "%(job_name)s job completed on commit"
          break
        elif [ "$JOB_STATUS" = "not_found" ]; then
          echo "%(job_name)s job not found yet, waiting..."
        else
          echo "%(job_name)s job status: $JOB_STATUS, waiting..."
        fi
      else
        echo "No %(workflow_name)s workflow run found yet for commit $COMMIT_SHA, waiting..."
      fi
      sleep 30
    done
  ||| % {
    commit_sha: commit_sha,
    workflow_name: workflow_name,
    job_name: job_name,
  },
  env: {
    GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
    COMMIT_SHA: commit_sha,
  },
};

local get_commit_with_message(sha, message, repo='${{ github.repository }}') = {
  name: 'Get commit from with message %s' % message,
  id: 'get_commit_with_message',
  env: {
    GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
    REPO: repo,
    SHA: sha,
    MESSAGE: message,
  },
  run: |||
    commit_with_message_sha=$(gh api "/repos/${REPO}/commits" -f "sha=${SHA}" --method GET \
    --jq ".[] | select(.commit.message | contains(\"${MESSAGE}\")) | .sha")
    echo $commit_with_message_sha
    echo "sha=$commit_with_message_sha" >> $GITHUB_OUTPUT
  |||,
};
local commit_with_message_output = '${{ steps.get_commit_with_message.outputs.sha }}';

// Find the merge-base commit between current commit and a base branch
local find_merge_base_step(base_branch='develop') = {
  name: 'Find merge-base with %s' % base_branch,
  id: 'get-merge-base',
  run: |||
    merge_base=$(gh api repos/${{ github.repository }}/compare/%s...${{ github.sha }} --jq .merge_base_commit.sha)
    echo "Merge base with %s: $merge_base"
    echo "commit=$merge_base" >> $GITHUB_OUTPUT
  ||| % [base_branch, base_branch],
  env: {
    GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
  },
};
local merge_base_output = '${{ steps.get-merge-base.outputs.commit }}';

{
  // ---------------------------------------------------------
  // Checkout
  // ---------------------------------------------------------

  // TODO: default to submodules=true, and a flexible with={}?
  // What about 'persist-credentials': false? needed? A few of
  // our workflows was using that, but not consistently
  checkout: function(ref='') (
    [
      {
        uses: uses.actions.checkout,
      } + (if ref == '' then {} else { with: { ref: ref } }),
    ]
  ),
  // The right checkout to call in most cases; slower but correct.
  // There is also 'submodules: "recursive" (which is even slower).
  checkout_with_submodules: function(ref='')
    [
      gha.git_safedir,
      gha.speedy_checkout_step,
      {
        uses: uses.actions.checkout,
        with: {
          submodules: true,
        } + (if ref == '' then {} else { ref: ref }),
      },
    ],

  // ---------------------------------------------------------
  // Python stuff
  // ---------------------------------------------------------

  // We also pin uv version in the Dockerfile. So, if you are updating
  // the version here, update it there too.
  default_uv_version: '0.9.26',
  setup_python_step: function(version='3.11', cache='auto') {
    uses: uses.astral_sh.setup_uv,
    with: {
      'python-version': version,
      version: $.default_uv_version,
      'enable-cache': cache,
    },
  },
  install_python_deps(directory, group=''): {
    name: 'Install Python dependencies',
    'working-directory': directory,
    run: (if group != '' then 'uv sync --group %s' % group else 'uv sync'),
  },

  // ---------------------------------------------------------
  // Docker
  // ---------------------------------------------------------

  // alt: run: docker-login -u USER -p PASS
  // alt: run a .github/docker-login
  docker_login_step: {
    uses: uses.docker.login_action,
    with: {
      username: '${{ secrets.DOCKER_USERNAME }}',
      password: '${{ secrets.DOCKER_PASSWORD }}',
    },
  },
  ecr_login_steps: [
    // The configure-aws-credentials and login-ecr steps are needed so
    // we can push to Amazon ECR successfully.
    {
      id: 'configure-aws-credentials',
      name: 'Configure AWS credentials',
      uses: uses.aws_actions.configure_aws_credentials,
      // The aws role we use to push to ecr is different than the role in
      // semgrep_pro.libsonnet because it's used for a different purpose.

      // Here the role is used to access ECR, while in semgrep_pro.libjsonnet
      // the role is used to upload pro binary buckets to a specific S3
      // location.
      with: {
        'role-to-assume': 'arn:aws:iam::338683922796:role/semgrep-semgrep-proprietary-deploy-role',
        'role-duration-seconds': 900,
        'role-session-name': 'semgrep-proprietary-build-test-docker-gha',
        'aws-region': 'us-west-2',
      },
    },
    {
      id: 'login-ecr',
      name: 'Login to Amazon ECR',
      uses: uses.aws_actions.amazon_ecr_login,
    },
  ],

  // ---------------------------------------------------------
  // Artifact management
  // ---------------------------------------------------------

  // works with upload_artifact_step() below by relying on an artifacts.tgz
  make_artifact_step(path): {
    name: 'Make artifact for %s' % path,
    run: |||
      mkdir artifacts
      cp -LR %s artifacts/
      tar czf artifacts.tgz artifacts
      # so that we can untar later and not get a message
      # about existing artifacts/ directory
      rm -rf artifacts
    ||| % path,
  },
  upload_artifact_step: function(artifact_name, path='artifacts.tgz') {
    uses: uses.actions.upload_artifact,
    with: {
      path: path,
      name: artifact_name,
    },
  },
  download_artifact_step: download_artifact_step,
  get_workflow_run_id_step: get_workflow_run_id_step,
  workflow_run_id_output: workflow_run_id_output,
  wait_for_workflow_run: wait_for_workflow_run,
  wait_for_workflow_job_on_commit_step: wait_for_workflow_job_on_commit_step,
  get_commit_with_message_step: get_commit_with_message,
  commit_with_message_output: commit_with_message_output,
  find_merge_base_step: find_merge_base_step,
  merge_base_output: merge_base_output,
  // See semgrep.libjsonnet cache_opam for inspiration here
  //
  guard_cache_hit: {
    step(path, key='${{ github.sha}}', bump_cache=1): {
      name: 'Set GHA cache for ' + key + ' in ' + path,
      uses: uses.actions.cache,
      env: {
        SEGMENT_DOWNLOAD_TIMEOUT_MINS: 2,
      },
      with: {
        path: path,
        key: '${{ runner.os }}-${{ runner.arch }}-v%d-opam-%s' % [bump_cache, key],
      },
    },
    // to be used with workflow_dispatch and workflow_call in the workflow
    inputs(required, step): {
      inputs: {
        'use-cache': {
          description: 'Use Github Cache for ' + step + '- uncheck the box to disable use of the cache for this step, meaning a long-running but completely from-scratch build.',
          required: required,
          type: 'boolean',
          default: true,
        },
      },
    },
    if_cache_inputs: {
      'if': '${{ inputs.use-cache}}',
    },
  },
  check_patch_release_step(version, id='check-patch-release'):
    {
      name: 'Check if patch release',
      id: id,
      env: {
        VERSION: version,
      },
      run: |||
        if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.0$ ]]; then
          echo "${VERSION} is a patch release."
          echo "patch_release=true" >> $GITHUB_OUTPUT
        else
          echo "${VERSION} is a minor or major release."
          echo "patch_release=false" >> $GITHUB_OUTPUT
        fi
      |||,
    },
  // ??
  inc_version_steps: function(id='inc-version', fragment, ref='develop', target_feature_version='') [
    {
      uses: uses.actions.checkout,
      with: {
        ref: ref,
      },
    },
    // Note that checkout@v4 does not get the tags by default. It does
    // if you do "full" checkout, which is too heavyweight. We don't
    // want all branches and everything that ever existed on the repo,
    // so we just do a lightweight checkout and then get the tags
    // ourselves. Also we don't need the tags in submodules.
    {
      name: 'Pull Tags',
      run: |||
        git fetch --no-recurse-submodules origin 'refs/tags/*:refs/tags/*'
      |||,
    },
    {
      name: 'Get latest version',
      id: 'latest-version-%s' % id,
      env: {
        TARGET_FEATURE_VERSION: target_feature_version,
        FRAGMENT: fragment,
      },
      run: |||
        # Get the latest tag by default
        version_pattern="v*.*.*"
        # Check if we are doing a bug patch and if target feature is set, then
        # get whatever the next bug patch is
        if [[ "$TARGET_FEATURE_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] && [ "$FRAGMENT" = "bug" ]; then
          echo "${TARGET_FEATURE_VERSION} is a valid patch release."
          major_minor=$(echo "$TARGET_FEATURE_VERSION" | cut -d '.' -f 1-2)
          # if $TARGET_FEATURE_VERSION is 1.0.0, this will be v1.0.*
          version_pattern="v${major_minor}.*"
        fi
        echo "version_pattern=${version_pattern}"
        LATEST_TAG=$(git tag --list "${version_pattern}" | sort -V | tail -n 1 | cut -c 2- )
        echo "LATEST_TAG=${LATEST_TAG}"
        echo "version=${LATEST_TAG}" >> $GITHUB_OUTPUT
      |||,
    },
    {
      name: 'Bump Feature',
      id: id,
      uses: uses.christian_draeger.increment_semantic_version,
      with: {
        'current-version': '${{ steps.latest-version-%s.outputs.version }}' % id,
        'version-fragment': fragment,
      },
    },
  ],
}

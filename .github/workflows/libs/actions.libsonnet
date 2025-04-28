// Factorize GHA "actions" (=~ plugins) boilerplate.
local gha = import './gha.libsonnet';

local download_artifact_step(artifact_name, run_id=null) = {
  uses: 'actions/download-artifact@v4',
  with: {
  } + (if artifact_name == '' then {} else {

         name: artifact_name,
       }) + (if run_id != null then {
               'run-id': run_id,
               'github-token': '${{ secrets.GITHUB_TOKEN }}',
             } else {}),
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
    // get the most recent workflow run id from a specific commit sha
    // This is not as hacky as it seems I promise
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
        uses: 'actions/checkout@v4',
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
        uses: 'actions/checkout@v4',
        with: {
          submodules: true,
        },
      } + (if ref == '' then {} else { ref: ref }),
    ],

  // ---------------------------------------------------------
  // Python stuff
  // ---------------------------------------------------------

  // Small wrapper around https://github.com/actions/setup-python
  // TODO: maybe simplify callers now that has default version to 3.11
  setup_python_step: function(version='3.11', cache='pipenv') {
    uses: 'actions/setup-python@v5',
    with: {
      'python-version': version,
    } + (if (cache == false) then {} else {
           // TODO where is this cache created?
           // TODO at least force to specify the key?
           // like 'cache-dependency-path': 'scripts/release/Pipfile.lock' ?
           cache: cache,
         }),
  },
  // We pin to a specific version just to prevent things from breaking randomly.
  // This has been a source of breakage in the past.
  pipenv_version: '2024.0.1',
  pipenv_install_step: {
    run: 'pip install pipenv==%s' % $.pipenv_version,
  },
  install_python_deps(directory): {
    name: 'Install Python dependencies',
    'working-directory': directory,
    run: 'pipenv install --dev',
  },

  // ---------------------------------------------------------
  // Docker
  // ---------------------------------------------------------

  // alt: run: docker-login -u USER -p PASS
  // alt: run a .github/docker-login
  docker_login_step: {
    uses: 'docker/login-action@v3',
    with: {
      username: '${{ secrets.DOCKER_USERNAME }}',
      password: '${{ secrets.DOCKER_PASSWORD }}',
    },
  },

  // ---------------------------------------------------------
  // Artifact management
  // ---------------------------------------------------------

  // works with upload_artifact_step() below by relying on an artifacts.tgz
  make_artifact_step(path): {
    name: 'Make artifact for %s' % path,
    run: |||
      mkdir artifacts
      cp %s artifacts/
      tar czf artifacts.tgz artifacts
      # so that we can untar later and not get a message
      # about existing artifacts/ directory
      rm -rf artifacts
    ||| % path,
  },
  upload_artifact_step: function(artifact_name, path='artifacts.tgz') {
    uses: 'actions/upload-artifact@v4',
    with: {
      path: path,
      name: artifact_name,
    },
  },
  download_artifact_step: download_artifact_step,
  get_workflow_run_id_step: get_workflow_run_id_step,
  workflow_run_id_output: workflow_run_id_output,
  wait_for_workflow_run: wait_for_workflow_run,
  get_commit_with_message_step: get_commit_with_message,
  commit_with_message_output: commit_with_message_output,
  // See semgrep.libjsonnet cache_opam for inspiration here
  //
  guard_cache_hit: {
    step(path, key='${{ github.sha}}', bump_cache=1): {
      name: 'Set GHA cache for ' + key + ' in ' + path,
      uses: 'actions/cache@v4',
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
      uses: 'actions/checkout@v4',
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
      uses: 'christian-draeger/increment-semantic-version@1.1.0',
      with: {
        'current-version': '${{ steps.latest-version-%s.outputs.version }}' % id,
        'version-fragment': fragment,
      },
    },
  ],
}

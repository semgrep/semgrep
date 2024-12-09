// Factorize GHA "actions" (=~ plugins) boilerplate.
{
  // ---------------------------------------------------------
  // Checkout
  // ---------------------------------------------------------

  // TODO: default to submodules=true, and a flexible with={}?
  // What about 'persist-credentials': false? needed? A few of
  // our workflows was using that, but not consistently
  checkout: function() {
    uses: 'actions/checkout@v3',
  },
  // The right checkout to call in most cases; slower but correct.
  // There is also 'submodules: "recursive" (which is even slower).
  checkout_with_submodules: function()
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
      },
    },

  // ---------------------------------------------------------
  // Python stuff
  // ---------------------------------------------------------

  // Small wrapper around https://github.com/actions/setup-python
  // TODO: maybe simplify callers now that has default version to 3.11
  setup_python_step: function(version='3.11', cache='pipenv') {
    uses: 'actions/setup-python@v4',
    with: {
      'python-version': version,
      // TODO where is this cache created?
      // TODO at least force to specify the key?
      // like 'cache-dependency-path': 'scripts/release/Pipfile.lock' ?
      cache: cache,
    },
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
     }
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
        ||| % path,
  },
  upload_artifact_step: function(artifact_name, path='artifacts.tgz') {
       uses: 'actions/upload-artifact@v4',
       with: {
          path: path,
          name: artifact_name,
        },
  },
  download_artifact_step(artifact_name): {
      uses: 'actions/download-artifact@v4',
      with: {
        name: artifact_name,
      },
  },
  // See semgrep.libjsonnet cache_opam for inspiration here
  //
  guard_cache_hit: {
    step(path, key='${{ github.sha}}', bump_cache=1): {
      name: 'Set GHA cache for ' + key +' in ' + path,
      uses: 'actions/cache@v3',
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
      }
    },
    if_cache_inputs: {
      'if': '${{ inputs.use-cache}}'
    },
  },

  // ??
  inc_version_steps: function(id='inc-version', fragment) [
    {
      uses: 'actions/checkout@v4',
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
      id: 'latest-version',
      run: |||
        LATEST_TAG=$(git tag --list "v*.*.*" | sort -V | tail -n 1 | cut -c 2- )
        echo "version=${LATEST_TAG}" >> $GITHUB_OUTPUT
      |||,
    },
    {
      name: 'Bump Feature',
      id: id,
      uses: 'christian-draeger/increment-semantic-version@1.1.0',
      with: {
	'current-version': '${{ steps.latest-version.outputs.version }}',
	'version-fragment': fragment,
      },
    },
  ],
}

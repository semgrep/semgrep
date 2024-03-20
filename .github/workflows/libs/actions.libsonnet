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

  // TODO: maybe simplify callers now that has default version to 3.11
  setup_python_step: function(version='3.11') {
    uses: 'actions/setup-python@v4',
    with: {
      'python-version': version,
      // TODO where is this cache created?
      // TODO at least force to specify the key?
      // like 'cache-dependency-path': 'scripts/release/Pipfile.lock' ?
      cache: 'pipenv',
    },
  },
  // TODO? can we pin a more recent version?
  pipenv_install_step: {
    run: 'pip install pipenv==2022.6.7',
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
       uses: 'actions/upload-artifact@v3',
       with: {
          path: path,
          name: artifact_name,
        },
  },
  download_artifact_step(artifact_name): {
      uses: 'actions/download-artifact@v3',
      with: {
        name: artifact_name,
      },
  },
}

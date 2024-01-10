// Factorize GHA Action plugins boilerplate.
{
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
  // TODO: add _step suffix and maybe simplify callers now that
  // has default version to 3.11
  setup_python: function(version='3.11') {
    uses: 'actions/setup-python@v4',
    with: {
      'python-version': version,
      // TODO where is this cache created?
      // TODO at least force to specify the key?
      // like 'cache-dependency-path': 'scripts/release/Pipfile.lock' ?
      cache: 'pipenv',
    },
  },
}

// Factorize GHA Action plugins boilerplate.

{
  // TODO: default to submodules=true, and a flexible with={}?
  // What about 'persist-credentials': false? needed? A few of
  // our workflows was using that, but not consistently
  checkout: function() {
    uses: 'actions/checkout@v4',
  },
  // the right checkout to call in most cases; slower but correct.
  checkout_with_submodules: function()
    {
      uses: 'actions/checkout@v4',
      with: {
        submodules: true,
      },
    },
  setup_python: function(version) {
    uses: 'actions/setup-python@v4',
    with: {
      'python-version': version,
      // ??? where is this cache created?
      cache: 'pipenv',
    },
  },
}

// Factorize GHA Action plugins boilerplate.

{
  // TODO: default to submodules=true, and a flexible with={}?
  // What about 'persist-credentials': false? needed?
  checkout: function() {
    uses: 'actions/checkout@v3',
  },
  checkout_with_submodules: function()
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
      },
    },
}

// Factorize GHA Action plugins boilerplate.

{
  # TODO: default to submodules=true, and a flexible with={}?
  checkout: (function() {
               uses: 'actions/checkout@v3',
             }),
}

{
  exclude_paths: [
    'TCB/*',
    'tools/*',
    'scripts/*',
    'stats/*',
    '*_main.ml',
    'Main.ml',
    'Test*.ml',
    'Unit_*.ml',
    // TODO: we should check those modules are only
    // called from testing code
    '*_test_utils.ml',
  ],

  exclude: {
    exclude: self.exclude_paths,
  },
}

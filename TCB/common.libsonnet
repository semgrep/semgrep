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
  ],

  exclude: {
    exclude: self.exclude_paths,
  },
}

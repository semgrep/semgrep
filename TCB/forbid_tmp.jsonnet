// helpers
local funcs = [
  "temp_file",
  "open_temp_file",
  "get_temp_dir_name",
  "set_temp_dir_name",
  "temp_dir_name",
];

// The rule
{
  rules: [
    {
      id: 'forbid-tmp',
      match: {
        any:
          [('Filename.' + p) for p in funcs] +
          [('UFilename.' + p) for p in funcs] +
          # TODO: UTmp.$F at some point, especially new_temp_file()
          ["UTmp.with_tmp_file"],
      },
      languages: ['ocaml'],
      paths: {
        exclude: [
           "UTmp.ml", // TODO: remove at some point
	   "CapTmp.ml",
           'TCB/*',
           'tools/*', 'scripts/*', 'stats/*',
           'Test*.ml',  'Unit_*.ml',
        ],
      },
      severity: 'ERROR',
      //TODO: tell to use CapTmp.ml
      message: |||
        Do not use directly Filename. Use UTmp.ml instead.
      |||,
    },
  ],
}

local common = import 'common.libsonnet';

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
          ["UTmp.$F"],
      },
      languages: ['ocaml'],
      paths: {
        exclude: common.exclude_paths + ["UTmp.ml", "CapTmp.ml"],
      },
      severity: 'ERROR',
      message: |||
        Do not use directly Filename or UTmp. Try to use CapTmp instead.
      |||,
    },
  ],
}

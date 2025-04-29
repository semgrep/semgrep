local common = import 'common.libsonnet';

local unix_funcs = [
  'readdir',
  //TODO: open_in, open_out, ...
];

local sys_funcs = [
  'readdir',
];

{
  rules: [
    {
      id: 'forbid-fs',
      match: {
        any:
          // Unix
          [('Unix.' + p) for p in unix_funcs] +
          [('UUnix.' + p) for p in unix_funcs] +
          // Sys
          [('Sys.' + p) for p in sys_funcs] +
          [('USys.' + p) for p in sys_funcs] +
          //TODO anything from UFile
          //TODO Other libs?
          [],
      },
      languages: ['ocaml'],
      paths: {
        exclude: common.exclude_paths +
                 ['CapFS.ml', 'spacegrep/src/lib/Find_files.ml'],
      },
      severity: 'ERROR',
      message: |||
        Do not use Unix or Sys filesystem functions. Use the
        safer CapFS module.
      |||,
    },
  ],

}

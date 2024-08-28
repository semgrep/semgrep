// See also forbid_{exec,exit,chdir}.jsonnet
local common = import 'common.libsonnet';

local unix_funcs = [
  'fork',
  //TODO: alarm, signal, kill, waitpid
];

{
  rules: [
    {
      id: 'forbid-process',
      match: { any:
        // Unix
        [('Unix.' + p) for p in unix_funcs] +
        [('UUnix.' + p) for p in unix_funcs] +
        //TODO Other libs?
	[]
      },
      languages: ['ocaml'],
      paths: {
        exclude: common.exclude_paths
      },
      severity: 'ERROR',
      message: |||
        Do not use directly process functions. Use the
        safer CapProcess module.
      |||,
    },
  ],

}

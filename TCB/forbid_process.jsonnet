// See also forbid_{exec,exit,chdir}.jsonnet
local common = import 'common.libsonnet';

local unix_funcs = [
  'fork',
  'alarm',
  'setitimer',
  //TODO: alarm, signal, kill, waitpid
];

local sys_funcs = [
  //TODO: set_signal? but then need different capability depending
  // on the int argument of set_signal
];

{
  rules: [
    {
      id: 'forbid-process',
      match: { any:
        // Unix
        [('Unix.' + p) for p in unix_funcs] +
        [('UUnix.' + p) for p in unix_funcs] +
        // Sys
        [('Sys.' + p) for p in sys_funcs] +
        [('USys.' + p) for p in sys_funcs] +
	// Parmap
	['Parmap.$F'] +
        //TODO Other libs?
	[]
      },
      languages: ['ocaml'],
      paths: {
        exclude: common.exclude_paths +
	['Parmap_.ml']
      },
      severity: 'ERROR',
      message: |||
        Do not use directly process functions. Use the
        safer CapProcess or CapSys or CapUnix modules.
      |||,
    },
  ],

}

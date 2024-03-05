local common = import 'common.libsonnet';

{
  rules: [
    {
      id: 'forbid-chdir',
      match: { any: [
#TODO
#        'Unix.chdir','UUnix.chdir',
	#	'Sys.chdir', 'USys.chdir',
	'TODO.chdir',
      ] },
      languages: ['ocaml'],
      paths: common.exclude,
      severity: 'ERROR',
      message: |||
        Do not use directly chdir. Use the safer CapSys.chdir().
      |||,
    },
  ],

}

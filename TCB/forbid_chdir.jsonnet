local common = import 'common.libsonnet';

{
  rules: [
    {
      id: 'forbid-chdir',
      match: { any: [
        'Unix.chdir','UUnix.chdir',
        'Sys.chdir', 'USys.chdir',
        // TODO: could forbid CapSys.chdir and tell people to use
        // instead CapFS.with_chdir
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

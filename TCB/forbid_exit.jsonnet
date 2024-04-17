local common = import 'common.libsonnet';

{
  rules: [
    {
      id: 'forbid-exit',
      match: { any: [
        'exit $N',
        'Stdlib.exit',
        'UStdlib.exit',
        'Unix._exit',
        'UUnix._exit'
      ] },
      languages: ['ocaml'],
      paths: common.exclude,
      severity: 'ERROR',
      message: |||
        Do not use directly exit(). Either raise Common.UnixExit or use the
        safer CapStdlib.exit().
      |||,
    },
  ],

}

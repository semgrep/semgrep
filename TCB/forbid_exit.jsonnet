{
  rules: [
    {
      id: 'forbid-exit',
      match: { any: [
	'exit $N',
        'Stdlib.exit $N',
        'UStdlib.exit $N',
	'Unix._exit $N',
	'UUnix._exit $N'
      ] },
      languages: ['ocaml'],
      paths: {
        exclude: ['tools/*', 'scripts/*', '*_main.ml', 'Main.ml', 'Test.ml', 'Tests.ml'],
      },
      severity: 'ERROR',
      message: |||
        Do not use directly exit(). Either raise Common.UnixExit or use the
        safer CapStdlib.exit().
      |||,
    },
  ],

}

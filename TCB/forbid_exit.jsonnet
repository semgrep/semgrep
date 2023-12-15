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
      paths: {
        exclude: [
           'TCB/*',
           'tools/*', 'scripts/*',
            '*_main.ml', 'Main.ml',
            'Test*.ml',
         ],
      },
      severity: 'ERROR',
      message: |||
        Do not use directly exit(). Either raise Common.UnixExit or use the
        safer CapStdlib.exit().
      |||,
    },
  ],

}

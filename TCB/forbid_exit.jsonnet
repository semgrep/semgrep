{
  rules: [
    {
      id: 'forbid-exit',
      match: { any: ['exit $N', 'Unix._exit $N', 'UStdlib.exit $N'] },
      languages: ['ocaml'],
      severity: 'ERROR',
      message: |||
        Do not use directly exit(). Either raise Common.UnixExit or use the
        safer CapStdlib.exit().
      |||,
    },
  ],

}

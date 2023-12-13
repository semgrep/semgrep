{
  rules: [
    {
      id: 'forbid-exit',
      match: { any: ['exit $N', 'Unix._exit $N', 'UStdlib.exit $N'] },
      languages: ['ocaml'],
      paths: { exclude: ['tools/*', 'scripts/*', '*_main.ml'] },
      severity: 'ERROR',
      message: |||
        Do not use directly exit(). Either raise Common.UnixExit or use the
        safer CapStdlib.exit().
      |||,
    },
  ],

}

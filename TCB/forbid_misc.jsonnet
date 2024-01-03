{
  rules: [
    {
      id: 'forbid-random',
      match: { any: [
	'Random.$F',
      ] },
      languages: ['ocaml'],
      paths: {
        exclude: [
           'TCB/*',
           'tools/*', 'scripts/*',
            '*_main.ml', 'Main.ml',
            'Test*.ml', 'Unit*.ml',
         ],
      },
      severity: 'ERROR',
      message: |||
        Do not use directly Random; use the safer CapRandom module.
      |||,
    },
  ],

}

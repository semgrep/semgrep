local common = import 'common.libsonnet';

{
  rules: [
    {
      id: 'forbid-random',
      match: { any: [
        'Random.$F',
      ] },
      languages: ['ocaml'],
      paths: common.exclude,
      severity: 'ERROR',
      message: |||
        Do not use directly Random; use the safer CapRandom module.
      |||,
    },
    {
      id: 'forbid-obj-magic',
      match: { any: [
        'Obj.magic',
      ] },
      languages: ['ocaml'],
      paths: {
        exclude: [
         ],
      },
      severity: 'ERROR',
      message: |||
        Do not use Obj.magic. Period.
      |||,
    },
  ],

}

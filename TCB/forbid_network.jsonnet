{
  rules: [
    {
      id: 'forbid-network',
      match: { any: [
        'Client.get ...',
        'Client.post ...',
        'Cohttp_lwt_unix.get ...',
        'Cohttp_lwt_unix.post ...',
        'Unix.socket ...',
        'Unix.socketpair ...',
        'Unix.accept ...',
        'Unix.bind ...',
        'Unix.connect ...',
        'Unix.listen ...',
      ] },
      paths: {
        exclude: ['http_helpers.ml'],
      },
      languages: ['ocaml'],
      severity: 'ERROR',
      message: |||
        Do not use directly the network. Use Http_helpers.ml instead.
      |||,
    },
  ],

}

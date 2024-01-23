local unix_funcs_network = [
  'socket',
  'socketpair',
  'accept',
  'bind',
  'connect',
  'listen',
  'establish_server',
];

{
  rules: [
    {
      id: 'forbid-network',
      match: {
        any:
          // Cohttp
          [
            'Client.get',
            'Client.post',
            'Cohttp_lwt_unix.get',
            'Cohttp_lwt_unix.post',
          ] +
          // Unix
          [('Unix.' + p) for p in unix_funcs_network] +
          [('UUnix.' + p) for p in unix_funcs_network] +
          [],
      },
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

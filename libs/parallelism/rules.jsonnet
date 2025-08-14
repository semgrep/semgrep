{
  rules: [
    {
      id: 'no-ref-declarations-at-top-scope',
      paths: { exclude: [
        // We use naked refs for Cli arguments.  These are only read in a current
        // setting so these are fine.
        'src/*core_cli/*',

        // Menhir parsers are racy, but deprecated.
        '*/menhir/*',

        // Not sure if this the best idea, but for now...
        '*/experiments/*',

        // Tools won't run in parallel.
        'lsp_legacy/*',
        'osemgrep/*',
        'tools/*',
      ] },
      languages: ['ocaml'],
      severity: 'ERROR',
      patterns: [
        { pattern: 'let $VAL = ref $INIT' },
        { 'pattern-not-inside': 'fun $ARG -> ...' },
        { 'pattern-not-inside': 'let $FUNC ... = ...' },
        // TODO: Why isn't this syntactically valid?
        //{ 'pattern-not-inside': 'let $OBJ = object ... end' },
      ],
      message: |||
        Please do not introduce mutable state at the top scope, as different threads will be able to concurrently mutate their underlying value.

        Consider using a Hook or Fiber-local value if you intend this value to be "local" to each thread.  Alternatively, if you have thought through the consequences _and_ you really want this shared between threads, change this ref to an Atomic.
      |||,
    },
  ],
}

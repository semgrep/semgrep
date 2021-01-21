{
  rules: [
    {
      id: 'dangerous_eval',
      match: {
        and: [
          'eval(...)',
          { not: 'eval("...")' },
        ],
      },
      message: 'Dangerous eval',
      languages: ['javascript'],
      severity: 'ERROR',
    },
  ],
}

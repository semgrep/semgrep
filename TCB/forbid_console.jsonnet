local common = import 'common.libsonnet';

local stdlib_funcs = [
  'print_string'
  //TODO: many more
];

local unix_funcs = [
  'stdout',
  //TODO: many more
];

{
  rules: [
    {
      id: 'forbid-console',
      match: { any:
        // Stdlib
        [p for p in stdlib_funcs] +
        [('Stdlib.' + p) for p in stdlib_funcs] +
        [('UStdlib.' + p) for p in stdlib_funcs] +
        // Unix
        [('Unix.' + p) for p in unix_funcs] +
        [('UUnix.' + p) for p in unix_funcs] +
        // Printf
        ['Printf.printf'] +
        // Format
        ['Format.std_formatter'] +
        // Fmt
        ['Fmt.pr', 'Fmt.stdout'] +
        // Ocolor
        ['Ocolor_format.printf']
        // TODO: ANSITerminal.print_string prerr_string printf eprintf
        //TODO 'UConsole.$F'
        //TODO 'UCommon.pr' 'UCommon.pr2', ...
      },
      languages: ['ocaml'],
      paths: {
        exclude: common.exclude_paths +
        ['UConsole.ml', 'CapConsole.ml'] +
        // TODO: remove!
        ['common2.ml','UCommon.ml',
         'Console_Spinner.ml',
         'Dump.ml', 'Print_match.ml', 'Dump_match.ml', 'Trace_matching.ml',
         'test/Matcher.ml',
         'test_py_python_str_repr.ml',
         'Boilerplate_tree_sitter_typescript.ml'
        ]
        ,
      },
      severity: 'ERROR',
      message: |||
        Do not output directly to the console. Either use a formatter or use the
        safer CapConsole.print() function.
      |||,
    },
  ],

}

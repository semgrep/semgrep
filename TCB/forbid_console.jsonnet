local common = import 'common.libsonnet';

// we could also forbid stderr and the prerr_xxx but those
// are less important usually
local stdlib_funcs = [
  'print_string',
  'print_char',
  'print_bytes',
  'print_int',
  'print_float',
  'print_endline',
  'print_newline',
  //TODO: 'stdout',
];

local unix_funcs = [
  'stdout',
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
        ['Ocolor_format.printf',
	 'Ocolor_format.std_formatter',
	 'Ocolor_format.raw_std_formatter'] +
        // ANSITerminal
        ['ANSITerminal.print_string', 'ANSITerminal.printf'] +
        //TODO 'UConsole.$F'
        //TODO 'UCommon.pr' 'UCommon.pr2', ...
	[]
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
        Do not output directly to the console. Use the safer CapConsole.print() function,
	or make your function return a string, or use Logs.
      |||,
    },
  ],

}

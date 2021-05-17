(* [parsing_stats l json_output paths] recursively explores [paths] to
 * find files in given language [l]. Then it parses those files using
 * [Parse_target.parse_program] (which internally uses either a
 * treesitter or pfff parser) and output parsing statistics on stdout.
 * The output can be in JSON format if [json_output] is true.
 *
 * This function is called by the -parsing_stat command-line action.
 * Here is an example of use:
 *   $ semgrep-core -lang ocaml -parsing_stats tests/ocaml/ -json
 *   {"total":111,"bad":0,"percent_correct":100.0}
 *)
val parsing_stats : Lang.t -> bool (* json *) -> Common.path list -> unit

(* TODO: parsing regressions as in pfff (unfinished) *)
val parsing_regressions : Lang.t -> Common.path list -> unit

(* Similar to [parsing_stats], but uses only tree-sitter parsers,
 * and stop the parsing at the tree-sitter CST level (it does not
 * try to convert this CST in the generic AST).
 *)
val test_parse_tree_sitter : Lang.t -> Common.filename list -> unit

(* Dump the tree-sitter CST of the given file (it automatically detects
 * the language and parser to use based on the filename extension). *)
val dump_tree_sitter_cst : Common.filename -> unit

(* Dump the generic AST of the given file (it automatically detects
 * the language to use based on the filename extension) but only use
 * a pfff parser.
 *)
val dump_ast_pfff : Common.filename -> unit

(* For each file, parse the file using a pfff parser and
 * parse the file using a tree-sitter parser and output the differences
 * in the generic ASTs produced (internally using the Unix diff program
 * on the dumped ASTs).
 *)
val diff_pfff_tree_sitter : Common.filename list -> unit

(* [test_parse_rules paths] recursively explores [paths] to
 * find YAML files containing rules and check if they
 * parse correctly using Parse_rule.parse.
 *)
val test_parse_rules : Common.path list -> unit

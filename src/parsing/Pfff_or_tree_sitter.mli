type 'ast parser =
  | Pfff of (Common.filename -> 'ast * Parsing_stat.t)
  | TreeSitter of (Common.filename -> 'ast Tree_sitter_run.Parsing_result.t)

(* TODO: factorize with previous type *)
type 'ast pattern_parser =
  | PfffPat of (string -> 'ast)
  | TreeSitterPat of (string -> 'ast Tree_sitter_run.Parsing_result.t)

(* usage:
    run file [
        TreeSitter (Parse_typescript_tree_sitter.parse);
        Pfff (throw_tokens Parse_js.parse);
     ] Js_to_generic.program
*)
val run :
  Common.filename ->
  'ast parser list ->
  ('ast -> AST_generic.program) ->
  Parsing_result2.t

(* usage:
    let js_ast =
      str |> run_pattern ~print_errors [
         PfffPat Parse_js.any_of_string;
         TreeSitterPat Parse_typescript_tree_sitter.parse_pattern;
         ]
      in
      Js_to_generic.any js_ast
*)
val run_pattern :
  print_errors:bool -> 'ast pattern_parser list -> string -> 'ast

(* helpers used both in Parse_target.ml and Parse_target2.ml *)

val exn_of_loc : Tok.location -> Exception.t

val loc_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Tok.location

(* used by Parse_jsonnet *)
val error_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Exception.t

val throw_tokens :
  (Common.filename -> ('ast, 'toks) Parsing_result.t) ->
  Common.filename ->
  'ast * Parsing_stat.t

val run_external_parser :
  Common.filename ->
  (Common.filename -> AST_generic.program Tree_sitter_run.Parsing_result.t) ->
  Parsing_result2.t

(* helpers used both in Parse_pattern.ml and Parse_pattern2.ml *)

val extract_pattern_from_tree_sitter_result :
  'any Tree_sitter_run.Parsing_result.t -> bool (* print_errors *) -> 'any

val dump_and_print_errors :
  ('a -> unit) -> 'a Tree_sitter_run.Parsing_result.t -> unit

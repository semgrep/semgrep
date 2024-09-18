type 'ast parser =
  | Pfff of (Fpath.t -> 'ast * Parsing_stat.t)
  | TreeSitter of (Fpath.t -> ('ast, unit) Tree_sitter_run.Parsing_result.t)

(* TODO: factorize with previous type *)
type 'ast pattern_parser =
  | PfffPat of (string -> 'ast)
  | TreeSitterPat of (string -> ('ast, unit) Tree_sitter_run.Parsing_result.t)

(* usage:
    run file [
        TreeSitter (Parse_typescript_tree_sitter.parse);
        Pfff (throw_tokens Parse_js.parse);
     ] Js_to_generic.program
*)
val run :
  Fpath.t ->
  'ast parser list ->
  ('ast -> AST_generic.program) ->
  Parsing_result2.t

(* usage:
    let js_ast =
      str |> run_pattern [
         PfffPat Parse_js.any_of_string;
         TreeSitterPat Parse_typescript_tree_sitter.parse_pattern;
         ]
      in
      Js_to_generic.any js_ast
*)
val run_pattern : 'ast pattern_parser list -> string -> 'ast

(* helpers used both in Parse_target.ml and Parse_target2.ml *)

val exn_of_loc : Tok.location -> Exception.t

(* used by Parse_jsonnet *)
val error_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Exception.t

val throw_tokens :
  (Fpath.t -> ('ast, 'toks) Parsing_result.t) ->
  Fpath.t ->
  'ast * Parsing_stat.t

val run_external_parser :
  Fpath.t ->
  (Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t) ->
  Parsing_result2.t

(* helpers used both in Parse_pattern.ml and Parse_pattern2.ml *)

val extract_pattern_from_tree_sitter_result :
  ('any, unit) Tree_sitter_run.Parsing_result.t -> 'any

val dump_and_print_errors :
  ('a -> unit) -> ('a, 'extra) Tree_sitter_run.Parsing_result.t -> unit

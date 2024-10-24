(*****************************************************************************)
(* Types *)
(*****************************************************************************)

module Err = Tree_sitter_run.Tree_sitter_error

type error = Tree_sitter_error of Err.t

type t = {
  ast : AST_generic.program;
  errors : error list;
  skipped_tokens : Tok.location list;
  inserted_tokens : Tok.location list;
  tolerated_errors : error list;
  stat : Parsing_stat.t;
}

let ok ast stat tolerated_errors =
  {
    ast;
    errors = [];
    skipped_tokens = [];
    inserted_tokens = [];
    tolerated_errors = List_.map (fun x -> Tree_sitter_error x) tolerated_errors;
    stat;
  }

let loc_of_tree_sitter_error (err : Err.t) =
  let start = err.start_pos in
  {
    Tok.str = err.substring;
    pos =
      Pos.make (Fpath.v err.file.name) ~line:(start.row + 1) (* fake *)
        ~column:start.column 0;
  }

(*
   Return error locations.
   Left: skipped tokens (ERROR nodes)
   Right: inserted empty tokens (MISSING nodes)
*)
let locs_of_tree_sitter_errors errs =
  let skipped = List.filter (fun (err : Err.t) -> err.kind = Error_node) errs in
  let inserted =
    List.filter (fun (err : Err.t) -> err.kind = Missing_node) errs
  in
  ( List_.map loc_of_tree_sitter_error skipped,
    List_.map loc_of_tree_sitter_error inserted )

let partial ast stat tree_sitter_errors =
  let skipped_tokens, inserted_tokens =
    locs_of_tree_sitter_errors tree_sitter_errors
  in
  let errors = List_.map (fun x -> Tree_sitter_error x) tree_sitter_errors in
  { ast; errors; skipped_tokens; inserted_tokens; tolerated_errors = []; stat }

let has_error x = x.errors <> []
let format_error ?style (Tree_sitter_error x) = Err.to_string ?style x

let format_errors ?style errors =
  errors |> List_.map (format_error ?style) |> String.concat ""

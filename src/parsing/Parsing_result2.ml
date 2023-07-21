(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type error = Tree_sitter_error of Tree_sitter_run.Tree_sitter_error.t

type t = {
  ast : AST_generic.program;
  errors : error list;
  skipped_tokens : Tok.location list;
  inserted_tokens : Tok.location list;
  stat : Parsing_stat.t;
}

let ok ast stat =
  { ast; errors = []; skipped_tokens = []; inserted_tokens = []; stat }

let loc_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  {
    Tok.str = err.substring;
    pos =
      {
        charpos = 0;
        (* fake *)
        line = start.row + 1;
        column = start.column;
        file = err.file.name;
      };
  }

(*
   Return error locations.
   Left: skipped tokens (ERROR nodes)
   Right: inserted empty tokens (MISSING nodes)
*)
let locs_of_tree_sitter_errors errs =
  errs
  |> Common.map loc_of_tree_sitter_error
  |> List.partition (fun (tok : Tok.location) ->
         (* Assume all empty tokens are missing nodes inserted by tree-sitter.
            TODO: expose the error kind explicitly in Tree_sitter_error.t *)
         tok.str <> "")

let partial ast stat tree_sitter_errors =
  let skipped_tokens, inserted_tokens =
    locs_of_tree_sitter_errors tree_sitter_errors
  in
  let errors = Common.map (fun x -> Tree_sitter_error x) tree_sitter_errors in
  { ast; errors; skipped_tokens; inserted_tokens; stat }

let has_error x = x.errors <> []

let format_error ?style (Tree_sitter_error x) =
  Tree_sitter_run.Tree_sitter_error.to_string ?style x

let format_errors ?style x =
  x.errors |> Common.map (format_error ?style) |> String.concat ""

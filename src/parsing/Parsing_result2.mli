(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* There's room for errors from other sources but we keep the original
   tree-sitter error because it contains a detailed error message.
   Alternatively, we could extract that error message but let's not throw
   it away. *)
type error = Tree_sitter_error of Tree_sitter_run.Tree_sitter_error.t

(* Mostly 'type t = (AST_generic, Parse_info.token_location) Parsing_result.t',
 * but the tokens part is actually used to represent skipped_tokens
 * instead of all the tokens (as intented in Parsing_result.t), hence
 * the new type.
 * TODO? merge with Parsing_result.t? add a skipped_tokens there too?
 *)
type t = {
  ast : AST_generic.program;
  (* Partial-parsing errors tree-sitter was able to recover from: *)
  errors : error list;
  (* Tokens that were ignored by the tree-sitter parser, corresponding to
     ERROR nodes. *)
  skipped_tokens : Tok.location list;
  (* Empty tokens that were inserted by the tree-sitter parser,
     corresponding to MISSING nodes. *)
  inserted_tokens : Tok.location list;
  (* Partial-parsing errors that are not reported. We need them when
     we want to be strict i.e. in parsing tests. *)
  tolerated_errors : error list;
  stat : Parsing_stat.t;
}

(*****************************************************************************)
(* Constructors *)
(*****************************************************************************)

val loc_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Tok.location

val ok :
  AST_generic.program ->
  Parsing_stat.t ->
  (* tolerated errors (missing tokens inserted by tree-sitter) *)
  Tree_sitter_run.Tree_sitter_error.t list ->
  t

val partial :
  AST_generic.program ->
  Parsing_stat.t ->
  Tree_sitter_run.Tree_sitter_error.t list ->
  t

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val has_error : t -> bool

(* Convert errors into a human-readable format. *)
val format_errors : ?style:Tree_sitter_run.Snippet.style -> error list -> string

(*
   Type holding parsing stats and optionally AST stats.
*)

type ast_stat = { total_node_count : int; untranslated_node_count : int }

type t = {
  filename : string;
  total_line_count : int;
  mutable error_line_count : int;
  mutable have_timeout : bool;
  (* used only for cpp for now, to help diagnose problematic macros,
   * see print_recurring_problematic_tokens below.
   *)
  mutable commentized : int;
  mutable problematic_lines : (string list * int) list;
  (* AST stats obtained by inspecting the resulting AST, if any. *)
  ast_stat : ast_stat option;
}

val default_stat : string (* filename *) -> t
val bad_stat : string (* filename *) -> t
val correct_stat : string (* filename *) -> t

(*
   Print file name and number of lines and error lines in compact format
   suitable for logging.
*)
val summary_of_stat : t -> string
val string_of_stats : ?verbose:bool -> t list -> string
val recurring_problematic_tokens : t list -> string
val aggregate_stats : t list -> int * int (* total * bad *)

val regression_information :
  ext:string -> Fpath.t list -> Common2.score -> string

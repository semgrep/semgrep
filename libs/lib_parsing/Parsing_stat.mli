(*
   Type holding parsing stats and optionally AST stats.
*)

type ast_stat = { total_node_count : int; untranslated_node_count : int }

type t = {
  filename : Common.filename;
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

val default_stat : Common.filename -> t
val bad_stat : Common.filename -> t
val correct_stat : Common.filename -> t

(*
   Print file name and number of lines and error lines in compact format
   suitable for logging.
*)
val summary_of_stat : t -> string
val print_parsing_stat_list : ?verbose:bool -> t list -> unit
val print_recurring_problematic_tokens : t list -> unit
val aggregate_stats : t list -> int * int (* total * bad *)

val print_regression_information :
  ext:string -> Fpath.t list -> Common2.score -> unit

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
}

(* alias, deprecated *)
type parsing_stat = t

val default_stat : Common.filename -> parsing_stat
val bad_stat : Common.filename -> parsing_stat
val correct_stat : Common.filename -> parsing_stat

(*
   Print file name and number of lines and error lines in compact format
   suitable for logging.
*)
val summary_of_stat : parsing_stat -> string
val print_parsing_stat_list : ?verbose:bool -> parsing_stat list -> unit
val print_recurring_problematic_tokens : parsing_stat list -> unit
val aggregate_stats : parsing_stat list -> int * int (* total * bad *)

val print_regression_information :
  ext:string -> Common2.path list -> Common2.score -> unit

(* this can be raised when timeout_threshold is set *)
exception File_timeout of Rule_ID.t list

type timeout_config = {
  timeout : float;
  threshold : int;
  caps : < Cap.time_limit >;
  clock : float Eio.Time.clock_ty Eio.Std.r option;
}

(* Matches many rules against one target. This function is called from
 * Test_engine.ml, Test_subcommand.ml, and of course Core_scan.ml
 * (and also Match_extract_mode.ml now).
 *
 * Return matches, errors, and match time.
 *
 * This will run the search-mode and taint-mode rules.
 * !This can also raise File_timeout!
 *)
val check :
  matches_hook:(Core_match.t list -> Core_match.t list) ->
  timeout:timeout_config option ->
  Match_env.xconfig ->
  Rule.rules ->
  Xtarget.t ->
  Core_result.matches_single_file

(* for osemgrep interactive *)
val is_relevant_rule_for_xtarget :
  Rule.t -> Match_env.xconfig -> Xtarget.t -> bool

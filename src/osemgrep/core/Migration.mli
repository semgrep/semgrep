(* Anything that can help deprecate old features and help users migrate
 * to the new way to do things.
 *)

(* may raise Error.Exit if we detect the presence of a .semgrep.yml *)
val abort_if_use_of_legacy_dot_semgrep_yml : unit -> unit

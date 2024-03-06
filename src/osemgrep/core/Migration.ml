(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Anything that can help deprecate old features and teach users how to
 * migrate to the new way to do things.
 *)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

(* warn people if they still rely on the deprecated .semgrep.yml
 * or .semgrep/ rules folder (except if it's the usual ~/.semgrep).
 *)
let abort_if_use_of_legacy_dot_semgrep_yml () =
  if
    Sys.file_exists ".semgrep.yml"
    || Sys.file_exists ".semgrep"
       && not (Sys.file_exists ".semgrep/settings.yml")
  then (
    flush stdout;
    Logs.err (fun m ->
        m
          "The implicit use of .semgrep.yml (or .semgrep/) has been deprecated \
           in Semgrep 1.38.0.\n\
           Please use an explicit --config .semgrep.yml (or --config .semgrep/)");
    Error.exit_code_exn (Exit_code.fatal ~__LOC__))

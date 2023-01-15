(*
   Shared parameters, options, and help messages for the semgrep CLI.
*)
open Cmdliner

let help_page_bottom =
  [
    `S Manpage.s_authors;
    `P "r2c <support@r2c.dev>";
    `S Manpage.s_bugs;
    `P
      "If you encounter an issue, please report it at\n\
      \      https://github.com/returntocorp/semgrep/issues";
  ]

(* Small wrapper around Cmdliner.Cmd.eval_value.
 * Note that I didn't put this helper function in Cmdliner_helpers.ml because
 * it's using Exit_code.ml and Error.ml which are semgrep-specific.
 *)
let eval_value ~argv cmd =
  (* the ~catch:false is to let non-cmdliner exn (e.g., Error.Semgrep_error)
   * to bubble up; those exns will then be caught in CLI.safe_run.
   *)
  match Cmd.eval_value ~catch:false ~argv cmd with
  (* alt: could define a new Exit_code for those kinds of errors *)
  | Error (`Term | `Parse) -> Error.exit Exit_code.fatal
  (* this should never happen, because of the ~catch:false above *)
  | Error `Exn -> assert false
  | Ok ok -> (
      match ok with
      | `Ok config -> config
      | `Version
      | `Help ->
          Error.exit Exit_code.ok)

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

(* Wrapper that catches exceptions and turns them into an exit code. *)
let safe_run run conf : Exit_code.t =
  try
    Printexc.record_backtrace true;
    run conf
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n%!" msg;
      Exit_code.fatal
  | e ->
      let trace = Printexc.get_backtrace () in
      Printf.eprintf "Error: exception %s\n%s%!" (Printexc.to_string e) trace;
      Exit_code.fatal

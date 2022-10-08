(*
   Shared parameters, options, and help messages for the semgrep CLI.
*)

open Printf
open Cmdliner

(* Turn "a" into "-a" and "abc" into "--abc" *)
let add_option_dashes option_names =
  Common.map
    (fun s ->
      assert (s <> "");
      if String.length s = 1 then "-" ^ s else "--" ^ s)
    option_names

(* Define a flag that can be negated e.g. --foo and --no-foo.
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
   See https://github.com/dbuenzli/cmdliner/issues/164
*)
let negatable_flag ?(default = false) ~neg_options ~doc options =
  let neg_doc =
    let options_str = add_option_dashes options |> String.concat "/" in
    sprintf "negates %s" options_str
  in
  let enable = (true, Arg.info options ~doc) in
  let disable = (false, Arg.info neg_options ~doc:neg_doc) in
  Arg.value (Arg.vflag default [ enable; disable ])

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

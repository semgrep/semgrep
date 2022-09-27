(*
   'semgrep scan' subcommand
*)

(* Provide 'Term', 'Arg', and 'Manpage' modules. *)
open Cmdliner

type conf = { bar : bool }

(* All the business logic after command-line parsing. *)
let run _conf =
  print_endline "hello";
  0

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let bar_term =
  let info = Arg.info [ "bar" ] ~doc:"Enable the bar!" in
  Arg.value (Arg.flag info)

(*** Subcommand 'scan' ***)

let cmdline_term run =
  let combine bar = run { bar } in
  Term.(const combine $ bar_term)

let doc = "run semgrep rules on files"

let man =
  [
    `S Manpage.s_description;
    `P
      "Searches TARGET paths for matches to rules or patterns. Defaults to \
       searching entire current working directory.";
    `P "To get started quickly, run";
    `Pre "semgrep --config auto .";
    `P
      "This will automatically fetch rules for your project from the Semgrep \
       Registry. NOTE: Using `--config auto` will log in to the Semgrep \
       Registry with your project URL.";
    `P "For more information about Semgrep, go to https://semgrep.dev.";
    `P
      "NOTE: By default, Semgrep will report pseudonymous usage metrics to its \
       server if you pull your configuration from the Semgrep registy. To \
       learn more about how and why these metrics are collected, please see \
       https://semgrep.dev/docs/metrics. To modify this behavior, see the \
       --metrics option below.";
  ]
  @ CLI_common.help_page_bottom

let main argv =
  let info = Cmd.info "semgrep scan" ~doc ~man in
  CLI_common.safe_run run |> cmdline_term |> Cmd.v info |> Cmd.eval' ~argv

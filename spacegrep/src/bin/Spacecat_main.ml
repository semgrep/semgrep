open Printf
open Cmdliner
open Spacegrep

type config = {
  debug: bool;
}

let debug_term =
  let info =
    Arg.info ["debug"]
      ~doc:"Print the tree in a richer, unambiguous format suitable for
            debugging."
  in
  Arg.value (Arg.flag info)

let cmdline_term =
  let combine debug =
    { debug }
  in
  Term.(const combine
        $ debug_term
       )

let doc =
  "use a universal parser to interpret a program as a tree"

let man = [
  `S Manpage.s_description;
  `P "Read an arbitrary program from stdin and print the parse tree
      to stdout.";
  `S Manpage.s_authors;
  `P "Martin Jambon <martin@returntocorp.com>";
  `S Manpage.s_bugs;
  `P "Check out bug reports at \
      https://github.com/returntocorp/spacegrep/issues.";
]

let parse_command_line () =
  let info =
    Term.info
      ~doc
      ~man
      "spacecat"
  in
  match Term.eval (cmdline_term, info) with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok config -> config

let run config =
  let print =
    if config.debug then
      Print.Debug.to_stdout
    else
      Print.to_stdout
  in
  Parse.of_stdin ()
  |> print

let main () =
  Printexc.record_backtrace true;
  let config = parse_command_line () in
  run config

let () = main ()

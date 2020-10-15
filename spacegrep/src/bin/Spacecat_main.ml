(*
   Entrypoint for the 'spacecat' command.
*)

open Printf
open Cmdliner
open Spacegrep

type config = {
  is_pattern: bool;
  debug: bool;
}

let is_pattern_term =
  let info =
    Arg.info ["pattern"; "p"]
      ~doc:"Interpret the input as a pattern rather than a document."
  in
  Arg.value (Arg.flag info)

let debug_term =
  let info =
    Arg.info ["debug"]
      ~doc:"Print the tree in a richer, unambiguous format suitable for
            debugging."
  in
  Arg.value (Arg.flag info)

let cmdline_term =
  let combine is_pattern debug =
    { is_pattern; debug }
  in
  Term.(const combine
        $ is_pattern_term
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
  `S Manpage.s_see_also;
  `P "spacegrep(1)"
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
  let src = Src_file.of_stdin () in
  let pat =
    if config.is_pattern then
      Parse_pattern.of_src ~is_doc:false src
    else
      Parse_pattern.of_src ~is_doc:true src
      |> Pattern_AST.as_doc
  in
  let print =
    if config.debug then
      Print.Debug.to_stdout
    else
      Print.to_stdout
  in
  print pat

let main () =
  Printexc.record_backtrace true;
  let config = parse_command_line () in
  run config

let () = main ()

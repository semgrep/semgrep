(*
   Command-line options.
*)

open Printf
open Cmdliner

type format = Map | Visit | Compare
type t = { input_file : Fpath.t; format : format; fun_prefix : string }

let o_input_file =
  let info =
    Arg.info [] ~doc:"Input file. Must be a valid OCaml source file (.ml)."
  in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let o_format =
  let info = Arg.info [ "f"; "format" ] ~doc:"Output format." in
  let format =
    Arg.enum [ ("map", Map); ("visit", Visit); ("compare", Compare) ]
  in
  Arg.value (Arg.opt format Map info)

let o_fun_prefix =
  let info =
    Arg.info [ "fun-prefix" ]
      ~doc:
        "Prefix for the generated functions. This overrides the default \
         determined by the output format."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let cmdline_term run =
  let combine input_file format fun_prefix =
    let fun_prefix =
      match fun_prefix with
      | Some s -> s
      | None -> (
          match format with
          | Map -> "map_"
          | Visit -> "v_"
          | Compare -> "cmp_")
    in
    let conf = { input_file = Fpath.v input_file; format; fun_prefix } in
    run conf
  in
  Term.(const combine $ o_input_file $ o_format $ o_fun_prefix)

let doc = "derive repetitive source code from OCaml type definitions"

let man =
  [ (*
  `S Manpage.s_description;
  `P "Multi-line, general description goes here.";
  `P "This is another paragraph.";

  (* 'ARGUMENTS' and 'OPTIONS' sections are inserted here by cmdliner. *)

  `S Manpage.s_examples; (* standard 'EXAMPLES' section *)
  `P "Here is some code:";
  `Pre "let four = 2 + 2";

  `S Manpage.s_authors;
  `P "Your Name Here <yourname@example.com>";

  `S Manpage.s_bugs;
  `P "Contribute documentation improvements at
      https://github.com/mjambon/cmdliner-cheatsheet";

  `S Manpage.s_see_also;
  `P "Cmdliner project https://erratique.ch/software/cmdliner/doc/Cmdliner"
*) ]

let parse_command_line_and_run run =
  let info = Cmd.info ~doc ~man "otarzan" in
  Cmd.v info (cmdline_term run) |> Cmd.eval |> exit

let safe_run run conf =
  try run conf with
  | Failure msg ->
      eprintf "Error: %s\n%!" msg;
      exit 1
  | e ->
      let trace = Printexc.get_backtrace () in
      eprintf "Error: exception %s\n%s%!" (Printexc.to_string e) trace

let main run =
  Printexc.record_backtrace true;
  parse_command_line_and_run (safe_run run)

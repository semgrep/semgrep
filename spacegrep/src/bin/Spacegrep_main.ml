(*
   Entrypoint for the 'spacegrep' command.
*)

open Printf
open Cmdliner
open Spacegrep

type config = {
  debug: bool;
  pattern: string option;
  pattern_files: string list;
  doc_files: string list;
}

(*
   Run all the patterns on all the documents.
*)
let run_all patterns docs =
  List.iter (fun get_doc_src ->
    let doc_src = get_doc_src () in
    let doc = Parse_doc.of_src doc_src in
    List.iter (fun pat ->
      let matches = Match.search pat doc in
      Match.print doc_src matches
    ) patterns;
  ) docs

let run config =
  let patterns =
    (match config.pattern with
     | None -> []
     | Some pat_str -> [Src_file.of_string pat_str]
    ) @ List.map Src_file.of_file config.pattern_files
    |> List.map Parse_pattern.of_src
  in
  let docs =
    match config.doc_files with
    | [] -> [Src_file.of_stdin]
    | files -> List.map (fun file -> (fun () -> Src_file.of_file file)) files
  in
  if config.debug then
    Match.debug := true;
  run_all patterns docs

let debug_term =
  let info =
    Arg.info ["debug"]
      ~doc:"Print debugging information during matching."
  in
  Arg.value (Arg.flag info)

let pattern_term =
  let info =
    Arg.info []
      ~docv:"PATTERN"
      ~doc:"$(docv) is a pattern. Any text is a valid pattern. The special
            constructs '...' and uppercase metavariables such as '$X'
            are supported. '...' will match any input sequence lazily,
            possibly extending until the end of the current block or into
            sub-blocks.
            Metavariables will capture words. If the same metavariable occurs
            in multiple places in the pattern, it must capture the same
            word everywhere.
            Indentation in the pattern is significant and is useful to
            specify how far '...' extends. Any nesting in the pattern
            must match the nesting in the document.
            However, a flat pattern may still match a nested document.
            Nesting in the document is determined primarily by indentation
            but also by matching standard ascii braces that occur on the same
            line ('()', '[]', {}').
            Use the companion command 'spacecat' to see how a document
            or a pattern is interpreted by 'spacegrep'.
            "
  in
  Arg.value (Arg.pos 0 Arg.(some string) None info)

let pattern_file_term =
  let info =
    Arg.info ["patfile"; "p"]
      ~docv:"FILE"
      ~doc:"Read a pattern from file $(docv)."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let doc_file_term =
  let info =
    Arg.info ["docfile"; "d"]
      ~docv:"FILE"
      ~doc:"Read a document from file $(docv). This disables document input
            from stdin."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let cmdline_term =
  let combine debug pattern pattern_files doc_files =
    { debug; pattern; pattern_files; doc_files }
  in
  Term.(const combine
        $ debug_term
        $ pattern_term
        $ pattern_file_term
        $ doc_file_term
       )

let doc =
  "match a pattern against any program"

let man = [
  `S Manpage.s_description;
  `P "Match a pattern against an arbitrary program read from stdin.";
  `S Manpage.s_authors;
  `P "Martin Jambon <martin@returntocorp.com>";
  `S Manpage.s_bugs;
  `P "Check out bug reports at \
      https://github.com/returntocorp/spacegrep/issues.";
  `S Manpage.s_see_also;
  `P "spacecat(1)"
]

let parse_command_line () =
  let info =
    Term.info
      ~doc
      ~man
      "spacegrep"
  in
  match Term.eval (cmdline_term, info) with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok config -> config

let main () =
  Printexc.record_backtrace true;
  let config = parse_command_line () in
  run config

let () = main ()

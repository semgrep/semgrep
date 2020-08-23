(*
   Entrypoint for the 'spacegrep' command.
*)

open Printf
open Cmdliner
open Spacegrep

type when_use_color =
  | Auto
  | Always
  | Never

type config = {
  color: when_use_color;
  debug: bool;
  pattern: string option;
  pattern_files: string list;
  doc_files: string list;
}

let detect_highlight when_use_color oc =
  match when_use_color with
  | Always -> true
  | Never -> false
  | Auto -> Unix.isatty (Unix.descr_of_out_channel oc)

(*
   Run all the patterns on all the documents.
*)
let run_all ~debug ~highlight patterns docs =
  List.iter (fun get_doc_src ->
    let doc_src = get_doc_src () in
    let doc_type = File_type.guess doc_src in
    match doc_type with
    | Gibberish ->
        if debug then
          printf "ignore gibberish file: %s\n%!"
            (Src_file.source_string doc_src)
    | Text ->
        if debug then
          printf "read document: %s\n%!"
            (Src_file.source_string doc_src);
        let doc = Parse_doc.of_src doc_src in
        List.iter (fun (pat_src, pat) ->
          if debug then
            printf "match document from %s against pattern from %s\n%!"
              (Src_file.source_string doc_src)
              (Src_file.source_string pat_src);
          let matches = Match.search pat doc in
          Match.print ~highlight doc_src matches
        ) patterns;
  ) docs

let run config =
  let patterns =
    let pattern_files = Find_files.list config.pattern_files in
    (match config.pattern with
     | None -> []
     | Some pat_str -> [Src_file.of_string pat_str]
    ) @ List.map Src_file.of_file pattern_files
    |> List.map (fun pat_src -> (pat_src, Parse_pattern.of_src pat_src))
  in
  let docs =
    match config.doc_files with
    | [] -> [fun () -> Src_file.of_stdin ()]
    | roots ->
        let files = Find_files.list roots in
        List.map (fun file -> (fun () -> Src_file.of_file file)) files
  in
  let debug = config.debug in
  if debug then
    Match.debug := true;
  let highlight = detect_highlight config.color stdout in
  run_all ~debug ~highlight patterns docs

let color_conv =
  let parser when_use_color =
    match when_use_color with
    | "auto" -> Ok Auto
    | "always" -> Ok Always
    | "never" -> Ok Never
    | s -> Error (`Msg ("Invalid argument for --color option: " ^ s))
  in
  let printer fmt when_use_color =
    let s =
      match when_use_color with
      | Auto -> "auto"
      | Always -> "always"
      | Never -> "never"
    in
    Format.pp_print_string fmt s
  in
  Cmdliner.Arg.conv
    (parser, printer)

let color_term =
  let info =
    Arg.info ["color"]
      ~doc:"Whether to highlight matching text. Valid values are 'auto'
            (default), 'always', and 'never'."
  in
  Arg.value (Arg.opt color_conv Auto info)

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
            are supported. Each '...' allows skipping non-matching input in the
            same block or in sub-blocks, spanning at most 10 lines.
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
      ~doc:"Read a pattern from file or directory $(docv)."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let doc_file_term =
  let info =
    Arg.info ["docfile"; "d"]
      ~docv:"FILE"
      ~doc:"Read documents from file or directory $(docv).
            This disables document input from stdin."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let cmdline_term =
  let combine color debug pattern pattern_files doc_files =
    { color; debug; pattern; pattern_files; doc_files }
  in
  Term.(const combine
        $ color_term
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

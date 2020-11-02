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

type output_format =
  | Text
  | Semgrep

type config = {
  color: when_use_color;
  output_format: output_format;
  debug: bool;
  force: bool;
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
let run_all ~debug ~force ~output_format ~highlight patterns docs =
  let matches =
    List.map (fun get_doc_src ->
      let doc_src = get_doc_src () in
      let doc_type = File_type.classify doc_src in
      let matches =
        match doc_type, force with
        | (Minified | Binary), false ->
            eprintf "ignoring gibberish file: %s\n%!"
              (Src_file.source_string doc_src);
            []
        | _ ->
            if debug then
              printf "read document: %s\n%!"
                (Src_file.source_string doc_src);
            let doc = Parse_doc.of_src doc_src in
            List.mapi (fun pat_id (pat_src, pat) ->
              if debug then
                printf "match document from %s against pattern from %s\n%!"
                  (Src_file.source_string doc_src)
                  (Src_file.source_string pat_src);
              (pat_id, Match.search doc_src pat doc)
            ) patterns
      in
      (doc_src, matches)
    ) docs
  in
  match output_format with
  | Text ->
      Match.print_nested_results ~highlight matches
  | Semgrep ->
      Semgrep.print_semgrep_json matches

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
  run_all
    ~debug
    ~force:config.force
    ~output_format:config.output_format
    ~highlight
    patterns docs

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

let output_format_conv =
  let parser s =
    match s with
    | "text" -> Ok Text
    | "semgrep" -> Ok Semgrep
    | s -> Error (`Msg ("Invalid output format: " ^ s))
  in
  let printer fmt output_format =
    let s =
      match output_format with
      | Text -> "text"
      | Semgrep -> "semgrep"
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

let output_format_term =
  let info =
    Arg.info ["output-format"]
      ~doc:"Specifies how to print the matches. The default format, 'text',
            is an unspecified human-readable format. The other available
            format is 'semgrep', which produce json output for internal
            consumption by the semgrep front-end."
  in
  Arg.value (Arg.opt output_format_conv Text info)

let debug_term =
  let info =
    Arg.info ["debug"]
      ~doc:"Print debugging information during matching."
  in
  Arg.value (Arg.flag info)

let force_term =
  let info =
    Arg.info ["f"; "force"]
      ~doc:"Don't skip files that don't look human-readable."
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
  let combine color output_format debug force pattern pattern_files doc_files =
    { color; output_format; debug; force; pattern; pattern_files; doc_files }
  in
  Term.(const combine
        $ color_term
        $ output_format_term
        $ debug_term
        $ force_term
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
      https://github.com/returntocorp/semgrep/issues.";
  `S Manpage.s_see_also;
  `P "semgrep, spacecat"
]

let info name =
  Term.info
    ~doc
    ~man
    name

let parse_command_line name =
  match Term.eval (cmdline_term, info name) with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok config -> config

(*
   Entry point for calling the command 'spacegrep' directly.
*)
let main () =
  let config = parse_command_line "spacegrep" in
  run config

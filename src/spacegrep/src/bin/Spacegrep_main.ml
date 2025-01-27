(*
   Entrypoint for the 'spacegrep' command.
*)

(* for cmdliner >= 1.1.0 *)
[@@@alert "-deprecated"]

open Printf
open Cmdliner
open Spacegrep

let timeout_exit_code = 3

type when_use_color = Auto | Always | Never

(* There used to be a Semgrep output_format when spacegrep was called
 * from the Semgrep Python wrapper, but it's not the case anymore
 *)
type output_format = Text

type config = {
  case_insensitive : bool;
  ellipsis_max_span : int;
  color : when_use_color;
  output_format : output_format;
  debug : bool;
  force : bool;
  pattern : string option;
  pattern_files : string list;
  doc_files : string list;
  time : bool;
  timeout : int option;
  warn : bool;
  no_skip_search : bool;
  comment_style : Comment.style;
}

(* Those 2 types below are copy-pasted from Semgrep_core_response.atd
 * but duplicated here to avoid some dependencies between spacegrep and
 * semgrep-core.
 *)
type skip_reason = Minified | Binary
type skipped_target = { path : string; reason : skip_reason; details : string }

type matches = {
  matches :
    (Src_file.t * (int * Match.match_ list * float) list * float * float) list;
  skipped : skipped_target list;
  num_analyzed : int;
  num_files : int;
  num_matches : int;
  num_matching_files : int;
}

let detect_highlight when_use_color oc =
  match when_use_color with
  | Always -> true
  | Never -> false
  | Auto -> Unix.isatty (Unix.descr_of_out_channel oc)

let parse_pattern comment_style src =
  let src = Comment.remove_comments_from_src comment_style src in
  Parse_pattern.of_src src

let parse_doc comment_style src =
  let src = Comment.remove_comments_from_src comment_style src in
  Parse_doc.of_src src

(*
   Run all the patterns on all the documents.
*)
let run_all ~search_param ~debug ~force ~warn ~comment_style patterns docs :
    matches =
  let num_files = ref 0 in
  let num_analyzed = ref 0 in
  let num_matching_files = ref 0 in
  let num_matches = ref 0 in
  let skipped = ref [] in
  let matches =
    docs
    |> List_.filter_map
         (fun (get_doc_src : ?max_len:int -> unit -> Src_file.t) ->
           let matches, run_time =
             Match.timef (fun () ->
                 (*
                 We inspect the first 4096 bytes to guess whether the
                 file type. This saves time on large files, by reading
                 typically just one block from the file system.
              *)
                 let peek_length = 4096 in
                 let partial_doc_src = get_doc_src ~max_len:peek_length () in
                 let doc_type = File_type.classify partial_doc_src in
                 incr num_files;
                 let path = Src_file.source_string partial_doc_src in
                 match doc_type with
                 | Minified when not force ->
                     if warn then eprintf "ignoring minified file: %s\n%!" path;
                     skipped :=
                       {
                         path;
                         reason = Minified;
                         details =
                           "not a source file: target file appears to be \
                            minified";
                       }
                       :: !skipped;
                     None
                 | Binary when not force ->
                     if warn then eprintf "ignoring gibberish file: %s\n%!" path;
                     skipped :=
                       {
                         path;
                         reason = Binary;
                         details = "target looks like a binary file";
                       }
                       :: !skipped;
                     None
                 | Minified
                 | Binary
                 | Short
                 | Text -> (
                     incr num_analyzed;
                     let doc_src =
                       if Src_file.length partial_doc_src < peek_length then
                         (* it's actually complete, no need to re-input
                            the file *)
                         partial_doc_src
                       else get_doc_src ()
                     in
                     if debug then
                       printf "parse document: %s\n%!"
                         (Src_file.source_string doc_src);
                     let doc, parse_time =
                       Match.timef (fun () -> parse_doc comment_style doc_src)
                     in
                     let matches_in_file =
                       List_.mapi
                         (fun pat_id (pat_src, pat) ->
                           if debug then
                             printf
                               "match document from %s against pattern from %s\n\
                                %!"
                               (Src_file.source_string doc_src)
                               (Src_file.source_string pat_src);
                           let matches_for_pat, match_time =
                             Match.timed_search search_param doc_src pat doc
                           in
                           num_matches :=
                             !num_matches + List.length matches_for_pat;
                           (pat_id, matches_for_pat, match_time))
                         patterns
                     in
                     match matches_in_file with
                     | [] -> None
                     | _ ->
                         incr num_matching_files;
                         Some (doc_src, matches_in_file, parse_time)))
           in
           match matches with
           | None -> None
           | Some (doc_src, matches_in_file, parse_time) ->
               Some (doc_src, matches_in_file, parse_time, run_time))
  in
  {
    matches;
    skipped = List.rev !skipped;
    num_analyzed = !num_analyzed;
    num_files = !num_files;
    num_matches = !num_matches;
    num_matching_files = !num_matching_files;
  }

(*
   Exit the process after the specified timeout (in seconds).
*)
let exit_after ?stderr_msg ~duration exit_code =
  (*
     Warning: the signal handler executes asynchronously, much like a thread.
     Don't use printf or other functions that are not thread-safe.
  *)
  let write_msg =
    match stderr_msg with
    | None -> fun () -> ()
    | Some msg ->
        let msg = Bytes.of_string msg in
        fun () -> ignore (Unix.write Unix.stderr msg 0 (Bytes.length msg))
  in
  let handler _signal =
    write_msg ();
    exit exit_code
  in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle handler);
  ignore (Unix.alarm duration)

let apply_timeout config =
  match config.timeout with
  | None -> ()
  | Some duration ->
      exit_after ~stderr_msg:"\ntimeout\n" ~duration timeout_exit_code

let run config =
  apply_timeout config;
  let patterns_or_errors =
    let pattern_files = Find_files.list config.pattern_files in
    (match config.pattern with
    | None -> []
    | Some pat_str -> [ Src_file.of_string pat_str ])
    @ List_.map Src_file.of_file pattern_files
    |> List_.map (fun pat_src ->
           (pat_src, parse_pattern config.comment_style pat_src))
  in
  let patterns, errors =
    let rev_patterns, rev_errors =
      List.fold_left
        (fun (rev_patterns, rev_errors) (src, parse_result) ->
          match parse_result with
          | Ok pat -> ((src, pat) :: rev_patterns, rev_errors)
          | Error err -> (rev_patterns, (src, err) :: rev_errors))
        ([], []) patterns_or_errors
    in
    (List.rev rev_patterns, List.rev rev_errors)
  in
  let docs =
    match config.doc_files with
    | [] ->
        [
          (fun ?max_len () ->
            ignore max_len;
            Src_file.of_stdin ());
        ]
    | roots ->
        let files = Find_files.list roots in
        List_.map (fun file ?max_len () -> Src_file.of_file ?max_len file) files
  in
  let debug = config.debug in
  if debug then Match.debug := true;
  let highlight = detect_highlight config.color stdout in
  let {
    matches;
    skipped = _;
    num_analyzed;
    num_files;
    num_matches;
    num_matching_files;
  } =
    let search_param =
      Match.create_search_param ~no_skip_search:config.no_skip_search
        ~case_sensitive:(not config.case_insensitive)
        ~ellipsis_max_span:config.ellipsis_max_span ()
    in
    run_all ~search_param ~debug ~force:config.force ~warn:config.warn
      ~comment_style:config.comment_style patterns docs
  in
  (match config.output_format with
  | Text ->
      Print_match.print_nested_results ~with_time:config.time ~highlight matches
        errors);
  if debug then (
    printf "\nanalyzed %i files out of %i\n" num_analyzed num_files;
    printf "found %i matches in %i files\n" num_matches num_matching_files)

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
  Cmdliner.Arg.conv (parser, printer)

let output_format_conv =
  let parser s =
    match s with
    | "text" -> Ok Text
    | s -> Error (`Msg ("Invalid output format: " ^ s))
  in
  let printer fmt output_format =
    let s =
      match output_format with
      | Text -> "text"
    in
    Format.pp_print_string fmt s
  in
  Cmdliner.Arg.conv (parser, printer)

let case_insensitive_term =
  let info =
    Arg.info
      [ "case-insensitive"; "i" ]
      ~doc:
        "Match ascii letters in a case-insensitive fashion. For example, the \
         pattern 'Hello' will match both 'HellO' and 'hello'. However, \
         backreferences must still match exactly e.g. the pattern '\\$A + \
         \\$A' will match 'foo + foo' but not 'foo + Foo'."
  in
  Arg.value (Arg.flag info)

let ellipsis_max_span_term =
  let default = Match.default_search_param.ellipsis_max_span in
  let info =
    Arg.info
      [ "ellipsis-max-span"; "e" ]
      ~doc:
        (sprintf
           "Maximum number of newlines a single ellipsis pattern ('...') can \
            match. The default is %i newlines. The value 0 will force the \
            matched items to be all on the same line."
           default)
  in
  Arg.value (Arg.opt Arg.int default info)

let color_term =
  let info =
    Arg.info [ "color" ]
      ~doc:
        "Whether to highlight matching text. Valid values are 'auto' \
         (default), 'always', and 'never'."
  in
  Arg.value (Arg.opt color_conv Auto info)

let output_format_term =
  let info =
    Arg.info [ "output-format" ]
      ~doc:
        "Specifies how to print the matches. The default format, 'text', is an \
         unspecified human-readable format. The other format is 'semgrep', \
         which produces json output for consumption by the semgrep front-end. \
         Use a program like 'jq' or 'ydump' for pretty-printing this json \
         output."
  in
  Arg.value (Arg.opt output_format_conv Text info)

let debug_term =
  let info =
    Arg.info [ "debug" ] ~doc:"Print debugging information during matching."
  in
  Arg.value (Arg.flag info)

let force_term =
  let info =
    Arg.info [ "f"; "force" ]
      ~doc:"Don't skip files that don't look human-readable."
  in
  Arg.value (Arg.flag info)

let pattern_term =
  let info =
    Arg.info [] ~docv:"PATTERN"
      ~doc:
        "$(docv) is a pattern. Any text is a valid pattern. The special \
         constructs '...' and uppercase metavariables such as '$X' are \
         supported. Each '...' allows skipping non-matching input in the same \
         block or in sub-blocks, spanning at most 10 lines. Metavariables will \
         capture words. If the same metavariable occurs in multiple places in \
         the pattern, it must capture the same word everywhere. Indentation in \
         the pattern is significant and is useful to specify how far '...' \
         extends. Any nesting in the pattern must match the nesting in the \
         document. However, a flat pattern may still match a nested document. \
         Nesting in the document is determined primarily by indentation but \
         also by matching standard ascii braces that occur on the same line \
         ('()', '[]', {}'). Use the companion command 'spacecat' to see how a \
         document or a pattern is interpreted by 'spacegrep'."
  in
  Arg.value (Arg.pos 0 Arg.(some string) None info)

let pattern_file_term =
  let info =
    Arg.info [ "patfile"; "p" ] ~docv:"FILE"
      ~doc:"Read a pattern from file or root directory $(docv)."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let anon_doc_file_term =
  let info =
    Arg.info [] ~docv:"FILE"
      ~doc:
        "Read documents from file or directory $(docv). This disables document \
         input from stdin. Same as using '-d' or '--docfile'."
  in
  Arg.value (Arg.pos 1 Arg.(some string) None info)

let doc_file_term =
  let info =
    Arg.info [ "docfile"; "d" ] ~docv:"FILE"
      ~doc:
        "Read documents from file or root directory $(docv). This disables \
         document input from stdin. This option can be used multiple times to \
         specify multiple files or scanning roots."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let time_term =
  let info =
    Arg.info [ "time" ]
      ~doc:
        "Include parsing and matching times in the output.\n\
        \            This is an extra json field if the output is json\n\
        \            ('semgrep' format)."
  in
  Arg.value (Arg.flag info)

let timeout_term =
  let info =
    Arg.info [ "timeout" ] ~docv:"SECONDS"
      ~doc:
        (sprintf
           "Exit with code %i if the task is not finished after $(docv) \
            seconds."
           timeout_exit_code)
  in
  Arg.value (Arg.opt Arg.(some int) None info)

let warn_term =
  let info =
    Arg.info [ "warn"; "w" ]
      ~doc:
        "Print warnings about files that can't be processed such as binary \
         files or minified files."
  in
  Arg.value (Arg.flag info)

let no_skip_search_term =
  let info =
    Arg.info [ "no-skip-search" ]
      ~doc:
        "Disable the optimization that performs a first inspection of the \
         parsed pattern and parsed target and determines in some cases that \
         there can't be a match. Disabling this optimization forces the normal \
         search function to be called no matter what."
  in
  Arg.value (Arg.flag info)

let cmdline_term =
  let combine case_insensitive ellipsis_max_span color output_format debug force
      pattern pattern_files anon_doc_file doc_files time timeout warn
      no_skip_search comment_style eol_comment_start multiline_comment_start
      multiline_comment_end =
    let doc_files =
      match anon_doc_file with
      | None -> doc_files
      | Some x -> x :: doc_files
    in
    let comment_style =
      Comment.CLI.merge_comment_options ~comment_style ~eol_comment_start
        ~multiline_comment_start ~multiline_comment_end
    in
    {
      case_insensitive;
      ellipsis_max_span;
      color;
      output_format;
      debug;
      force;
      pattern;
      pattern_files;
      doc_files;
      time;
      timeout;
      warn;
      no_skip_search;
      comment_style;
    }
  in
  Term.(
    const combine $ case_insensitive_term $ ellipsis_max_span_term $ color_term
    $ output_format_term $ debug_term $ force_term $ pattern_term
    $ pattern_file_term $ anon_doc_file_term $ doc_file_term $ time_term
    $ timeout_term $ warn_term $ no_skip_search_term
    $ Comment.CLI.comment_style_term $ Comment.CLI.eol_comment_start_term
    $ Comment.CLI.multiline_comment_start_term
    $ Comment.CLI.multiline_comment_end_term)

let doc = "match a pattern against any program"

let man =
  [
    `S Manpage.s_description;
    `P "Match a pattern against an arbitrary program read from stdin.";
    `S Manpage.s_authors;
    `P "Martin Jambon <martin@semgrep.com>";
    `S Manpage.s_bugs;
    `P "Check out bug reports at https://github.com/semgrep/semgrep/issues.";
    `S Manpage.s_see_also;
    `P "semgrep, spacecat";
  ]

let info name = Term.info ~doc ~man name

let parse_command_line name =
  match Term.eval (cmdline_term, info name) with
  | `Error _ -> exit 1
  | `Version
  | `Help ->
      exit 0
  | `Ok config -> config

(*
   Entry point for calling the command 'spacegrep' directly.
*)
let main () =
  (*
     Make the GC work less aggressively than the default similar to
     what's done for semgrep-core.
     This wastes memory but makes everything 2-3x faster.
     See https://md.ekstrandom.net/blog/2010/06/ocaml-memory-tuning

     TODO: clean benchmarks
  *)
  Gc.set
    {
      (Gc.get ()) with
      Gc.space_overhead = 300;
      Gc.minor_heap_size = 4_000_000;
      Gc.major_heap_increment = 8_000_000;
    };

  let config = parse_command_line "spacegrep" in
  run config

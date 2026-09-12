(*
   Entrypoint for a standalone parser-dumper, to be called by generated
   parsers.
*)

(* for cmdliner >= 1.1.0 *)
[@@@alert "-deprecated"]

open Printf
open Cmdliner
open Tree_sitter_bindings

type input_kind =
  | Source_file
  | Json_file of string

type config = {
  source_file: string;
  input_kind: input_kind;
  output_json: bool;
  txt_stat: string option;
  json_error_log: string option;
}

(*
   Different classes of errors, useful for parsing stats.
*)
module Exit = struct
  let any_error = 1
  let bad_command_line = 10
  let external_parsing_error = 11
  let internal_parsing_error = 12
end

let source_file_term =
  let info =
    Arg.info []
      ~docv:"SRC_FILE"
      ~doc:"$(docv) specifies the path to the source file to be parsed."
  in
  Arg.required (Arg.pos 0 Arg.(some string) None info)

let input_json_term =
  let info =
    Arg.info []
      ~docv:"JSON_INPUT_FILE"
      ~doc:"$(docv) specifies a pre-parsed CST in the JSON format
            produced by tree-sitter. This input will be used directly
            instead of parsing the source file, which must still be
            specified on the command line for error reporting."
  in
  Arg.value (Arg.pos 1 Arg.(some string) None info)

let output_json_term =
  let info =
    Arg.info ["output-json"]
      ~doc:"Output a complete json representation of the parse tree obtained
            from the tree-sitter parser. This can be very slow on large
            data due to pretty-printing. This is intended for debugging
            small test cases."
  in
  Arg.value (Arg.flag info)

let txt_stat_term =
  let info =
    Arg.info ["txt-stat"]
      ~docv:"FILE"
      ~doc:"$(docv) specifies a file to which the number of total
            lines (A), the number of unparsable lines (B), and the
            number of errors (C) is written in the format 'A B C'."
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let json_error_log_term =
  let info =
    Arg.info ["json-error-log"; "e"]
      ~docv:"FILE"
      ~doc:"$(docv) specifies a file to which parsing errors should be
            appended. The format consists of one json object per line,
            and is otherwise left unspecified for now."
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let cmdline_term =
  let combine source_file input_json output_json txt_stat json_error_log =
    let input_kind =
      match input_json with
      | None -> Source_file
      | Some json_file -> Json_file json_file
    in
    { source_file; input_kind; output_json; txt_stat; json_error_log }
  in
  Term.(const combine
        $ source_file_term
        $ input_json_term
        $ output_json_term
        $ txt_stat_term
        $ json_error_log_term
       )

let doc ~lang =
  sprintf "parse a %s file with tree-sitter and ocaml-tree-sitter" lang

let man ~lang = [
  `S Manpage.s_description;
  `P (sprintf "\
Parse a %s file and dump the resulting parse tree (CST) in a
human-readable format for inspection purposes. This is a test program
meant to evaluate the quality of tree-sitter parsers and the recovery
of the full CST by ocaml-tree-sitter."
        lang);
  `S Manpage.s_bugs;
  `P "Check out bug reports at
      https://github.com/semgrep/ocaml-tree-sitter-core/issues.";
]

let parse_command_line ~lang =
  let info =
    Cmd.info
      ~doc:(doc ~lang)
      ~man:(man ~lang)
      ("parse-" ^ lang)
  in
  match Cmd.eval_value (Cmd.v info cmdline_term) with
  | Ok (`Ok config) -> config
  | Ok (`Version | `Help) -> exit 0
  | Error _ -> exit Exit.bad_command_line

let safe_run f =
  Printexc.record_backtrace true;
  try f ()
  with e ->
    let trace = Printexc.get_backtrace () in
    flush stdout;
    let msg, exit_code, show_trace =
      match e with
      | Tree_sitter_error.Error err ->
          let msg = Tree_sitter_error.to_string ~style:Auto err in
          let exit_code =
            match err.kind with
            | Error_node | Missing_node -> Exit.external_parsing_error
            | Internal -> Exit.internal_parsing_error
          in
          msg, exit_code, false
      | Failure msg ->
          msg, Exit.any_error, true
      | e ->
          let msg = sprintf "exception %s" (Printexc.to_string e) in
          msg, Exit.any_error, true
    in
    (if show_trace then
       eprintf "Error: %s\n%s\n"
         msg
         trace
     else
       eprintf "Error: %s\n" msg
    );
    eprintf "\nexit %i\n" exit_code;
    flush stderr;
    exit exit_code

let print_error err =
  eprintf "Error: %s\n"
    (Tree_sitter_error.to_string ~style:Auto err)

let print_errors errors =
  List.iter print_error errors

(*
   Obtain tree-sitter's CST either by parsing the source code
   or loading it from a json file.
*)
let load_input_tree ~parse_source_file conf =
  let src_file = conf.source_file in
  match conf.input_kind with
  | Source_file ->
      parse_source_file src_file
  | Json_file json_file ->
      Tree_sitter_parsing.load_json_file ~src_file ~json_file

let parse_and_dump
    ~parse_source_file
    ~parse_input_tree
    ~dump_tree
    ~dump_extras
    conf =
  let input_tree = load_input_tree ~parse_source_file conf in
  if conf.output_json then (
    printf "Complete CST obtained from the tree-sitter parser:\n%!";
    print_endline (Tree_sitter_output.to_json ~pretty:true input_tree.root)
  );
  printf "CST obtained from the tree-sitter parser:\n%!";
  Tree_sitter_parsing.print input_tree;
  let res : _ Parsing_result.t = parse_input_tree input_tree in
  let some_success =
    match res.program with
    | Some matched_tree ->
        printf "---\n";
        printf "Recovered typed CST:\n%!";
        dump_tree matched_tree;
        printf "Extras:\n%!";
        dump_extras res.extras;
        true
    | None ->
        eprintf "Error: \
                 failed to recover rich CST from original tree-sitter CST\n%!";
        false
  in
  let errors = res.errors in
  let stat = res.stat in
  (match conf.txt_stat with
   | Some out_file -> Parsing_result.export_stat ~out_file stat
   | None -> ()
  );
  (match conf.json_error_log with
   | Some err_log -> Tree_sitter_error.log_json_errors err_log errors
   | None -> ()
  );
  let lines = stat.total_line_count in
  let err_lines = stat.error_line_count in
  let success_ratio =
    if lines > 0 then float (lines - err_lines) /. float lines
    else 1.
  in
  print_errors errors;
  printf "\
total lines: %i
error lines: %i
error count: %i
success: %.2f%%
"
    stat.total_line_count
    stat.error_line_count
    stat.error_count
    (100. *. success_ratio);

  let success = some_success && errors = [] in
  let exit_code =
    if success then 0
    else Exit.external_parsing_error
  in
  exit_code

let run ~lang ~parse_source_file ~parse_input_tree ~dump_tree ~dump_extras =
  let conf = parse_command_line ~lang in
  safe_run (fun () ->
    let suggested_exit_code =
      parse_and_dump
        ~parse_source_file
        ~parse_input_tree
        ~dump_tree
        ~dump_extras
        conf
    in
    exit suggested_exit_code
  )

(*
   Application's entrypoint.
*)

(* for cmdliner >= 1.1.0 *)
[@@@alert "-deprecated"]

open Printf
open Cmdliner
open Tree_sitter_gen

type gen_conf = {
  lang : string;
  grammar : string;
  out_dir : string option;
}

type simplify_conf = {
  grammar: string;
  output_path: string;
}

type to_js_conf = {
  input_path: string option;
  output_path: string option;
  sort_choices: bool;
  sort_rules: bool;
  strip: bool;
}

type cmd_conf =
  | Gen of gen_conf
  | Simplify of simplify_conf
  | To_JS of to_js_conf

let safe_run f =
  try f () with
  | Failure msg ->
      eprintf "\
    Error: %s
    Try --help.
    %!" msg;
      exit 1
  | e ->
      let trace = Printexc.get_backtrace () in
      eprintf "\
    Error: exception %s
    %s
    Try --help.
    %!"
        (Printexc.to_string e)
        trace;
      exit 1

let gen (conf : gen_conf) =
  let tree_sitter_grammar =
    match
      Yojson.Safe.from_file conf.grammar
      |> Tree_sitter_grammar.grammar_of_yojson
    with
    | Ok g -> g
    | Error e ->
        failwith (Printf.sprintf "Failed to parse %s: %s" conf.grammar e)
  in
  let grammar = CST_grammar_conv.of_tree_sitter tree_sitter_grammar in
  Codegen.ocaml ?out_dir:conf.out_dir ~lang:conf.lang grammar

let simplify (conf : simplify_conf) =
  Simplify_grammar.run conf.grammar conf.output_path

let to_js (conf : to_js_conf) =
  To_JS.run
    ~sort_choices:conf.sort_choices
    ~sort_rules:conf.sort_rules
    ~strip:conf.strip
    conf.input_path conf.output_path

let run conf =
  safe_run (fun () ->
    match conf with
    | Gen conf -> gen conf
    | Simplify conf -> simplify conf
    | To_JS conf -> to_js conf
  )

(**************************************************************************)
(* Command-line interface *)
(**************************************************************************)

let lang_term =
  let info =
    Arg.info []
      ~docv:"LANG"
      ~doc:"$(docv) is the name of the language described by the grammar.
            If specified, this name will appear in the generated file names.
            It must be a valid OCaml lowercase identifier."
  in
  Arg.required (Arg.pos 0 Arg.(some string) None info)

let grammar_term =
  let info = Arg.info []
      ~docv:"GRAMMAR_JSON"
      ~doc:"$(docv) is a file containing a tree-sitter grammar in json format.
            Its name is commonly 'grammar.json'. Try to not confuse it with
            'grammar.js' from which it is typically derived."
  in
  Arg.required (Arg.pos 1 Arg.(some file) None info)

let out_dir_term =
  let info =
    Arg.info ["out-dir"; "d"]
      ~docv:"DIR"
      ~doc:"$(docv) specifies where to put the output files. The default is
            the current directory."
  in
  Arg.value (Arg.opt Arg.(some string) None info)


let simplify_cmd =
  let grammar_term =
    let info = Arg.info []
        ~docv:"GRAMMAR_JSON"
        ~doc:"$(docv) is a file containing a tree-sitter grammar in json format.
              Its name is commonly 'grammar.json'. Try to not confuse it with
              'grammar.js' from which it is typically derived."
    in
    Arg.required (Arg.pos 0 Arg.(some file) None info) in

  let output_file_term =
    let info = Arg.info []
        ~docv:"OUTPUT_FILE"
        ~doc:"$(docv) is a file containing the new tree-sitter grammar in json format.
          the main difference is that this file will expand all alias since ocaml-tree-sitter doest not support it."
    in
    Arg.required (Arg.pos 1 Arg.(some string) None info) in

  let doc =
    "simplify grammar.json for ocaml-tree-sitter" in

  let man = [
    `S Manpage.s_description;
    `P "simplify-grammar removes aliases and unhides hidden rules,
        for easier support by ocaml-tree-sitter.";
    `S Manpage.s_bugs;
    `P "Check out bug reports at
        https://github.com/semgrep/ocaml-tree-sitter-core/issues.";
  ] in
  let info = Cmd.info ~doc ~man "simplify" in
  let config grammar output_path =
    Simplify { grammar; output_path }
  in
  let cmdline_term = Term.(
    const config
    $ grammar_term
    $ output_file_term) in
  Cmd.v info cmdline_term

let to_js_cmd =
  let input_path_term =
    let info = Arg.info []
        ~docv:"INPUT_FILE"
        ~doc:"$(docv) is a file containing a tree-sitter grammar in json format.
              Its name is commonly 'grammar.json'."
    in
    Arg.value (Arg.pos 0 Arg.(some file) None info) in

  let output_path_term =
    let info = Arg.info []
        ~docv:"OUTPUT_FILE"
        ~doc:"$(docv) is a file containing the recovered tree-sitter grammar in
              JavaScript format."
    in
    Arg.value (Arg.pos 1 Arg.(some string) None info) in

  let sort_choices_term : bool Term.t =
    let info = Arg.info ["sort-choices"]
        ~doc:"Sort the elements of the 'choice()' constructs. \
              This may not preserve the parsing behavior perfectly."
    in
    Arg.value (Arg.flag info) in

  let sort_rules_term : bool Term.t =
    let info = Arg.info ["sort-rules"]
        ~doc:"Sort the rule definitions alphabetically. The first rule \
              remains first because it is the grammar's entry point."
    in
    Arg.value (Arg.flag info) in

  let strip_term : bool Term.t =
    let info = Arg.info ["strip"]
        ~doc:"Remove elements that don't affect the generated OCaml types \
              such as precedences."
    in
    Arg.value (Arg.flag info) in

  let normalize_term : bool Term.t =
    let info = Arg.info ["normalize"]
        ~doc:"Shorthand for all the options that rearrange the grammar \
              so as to make them easier to compare while ignoring the parts \
              that don't affect the generated types in file 'CST.ml'. \
              It currently combines --sort-choices, --sort-rules, \
              and --strip."
    in
    Arg.value (Arg.flag info) in

  let doc =
    "recover a tree-sitter grammar.js from grammar.json" in

  let man = [
    `S Manpage.s_description;
    `P "simplify-grammar removes aliases and unhides hidden rules,
        for easier support by ocaml-tree-sitter.";
    `S Manpage.s_bugs;
    `P "Check out bug reports at
        https://github.com/semgrep/ocaml-tree-sitter-core/issues.";
  ] in
  let info = Cmd.info ~doc ~man "to-js" in
  let config input_path output_path sort_choices sort_rules strip normalize =
    let sort_choices, sort_rules, strip =
      normalize || sort_choices,
      normalize || sort_rules,
      normalize || strip
    in
    To_JS { input_path; output_path; sort_choices; sort_rules; strip }
  in
  let cmdline_term = Term.(
    const config
    $ input_path_term
    $ output_path_term
    $ sort_choices_term
    $ sort_rules_term
    $ strip_term
    $ normalize_term) in
  Cmd.v info cmdline_term

let gen_cmd =
  let config lang grammar out_dir =
    Gen { lang; grammar; out_dir }
  in

  let cmdline_term =
    Term.(
      const config
      $ lang_term
      $ grammar_term
      $ out_dir_term
    ) in

  let doc = "derive ocaml code to interpret tree-sitter parsing output" in
  let man = [
    `S Manpage.s_description;
    `P "ocaml-tree-sitter takes a tree-sitter grammar and generates OCaml
      for reading tree-sitter parsing output. The input grammar is in
      json format, commonly under the file name 'grammar.json'.
      tree-sitter itself is used separately to generate the actual C parser
      from the json grammar. The generated OCaml code is meant to be used
      to process the output of such a parser.";
    `S Manpage.s_bugs;
    `P "Check out bug reports at
      https://github.com/semgrep/ocaml-tree-sitter-core/issues.";
  ] in
  let version = "0.0.0" in
  let info = Cmd.info ~version ~doc ~man "gen" in

  Cmd.v info cmdline_term

let root_info =
  let man = [
    `S Manpage.s_description;
    `P "Generate OCaml parsers based on tree-sitter grammars";
    `P "For a general project setup you will run `ocaml-tree-sitter-gen-c` and `ocaml-tree-sitter-gen-ocaml`scripts
    but there is `gen` and `simplify` subcommands. Checkout their help page";
    `S Manpage.s_bugs;
    `P "Check out bug reports at
      https://github.com/semgrep/ocaml-tree-sitter-core/issues.";
  ] in
  let doc = "Generate OCaml parsers based on tree-sitter grammars" in
  Cmd.info ~man ~doc "ocaml-tree-sitter"

let subcommands = [gen_cmd; simplify_cmd; to_js_cmd]

let parse_command_line () : cmd_conf =
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  match Cmd.eval_value (Cmd.group ~default root_info subcommands) with
  | Ok (`Ok conf) -> conf
  | Ok (`Version | `Help) -> exit 0
  | Error _ -> exit 1

let main () =
  Printexc.record_backtrace true;
  let conf = parse_command_line () in
  run conf

let () = main ()

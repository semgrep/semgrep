(*s: pfff/cli/Main.ml *)
(*
 * Please imagine a long and boring GNU-style copyright notice
 * appearing just here.
 *)
open Common
module J = JSON

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A "driver" for the different parsers in pfff.
 *
 * Also useful to dump the CST or AST of a language (-dump_xxx).
 *
 * related:
 *  - https://astexplorer.net/, supports many languages, many parsers
*)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this
 * program also depends on external files?
*)

(*s: constant [[Main.verbose]] *)
let verbose = ref false
(*e: constant [[Main.verbose]] *)

let log_config_file = ref "log_config.json"

(*s: constant [[Main.lang]] *)
let lang = ref "c"
(*e: constant [[Main.lang]] *)

(*s: constant [[Main.action]] *)
(* action mode *)
let action = ref ""
(*e: constant [[Main.action]] *)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*s: function [[Main.main_action]] *)
let main_action _xs =
  raise Todo
(*e: function [[Main.main_action]] *)

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(*s: function [[Main.test_json_pretty_printer]] *)
let test_json_pretty_printer file =
  let json = J.load_json file in
  let s = J.string_of_json json in
  pr s
(*e: function [[Main.test_json_pretty_printer]] *)

(* ---------------------------------------------------------------------- *)
(*s: function [[Main.pfff_extra_actions]] *)
let pfff_extra_actions () = [
  "-dump_json", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  (*s: [[Main.pfff_extra_actions]] other cases *)
  "-json_pp", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  (*x: [[Main.pfff_extra_actions]] other cases *)
  "-layer_stat", " <file>",
  Common.mk_action_1_arg Test_program_lang.layer_stat;
  (*e: [[Main.pfff_extra_actions]] other cases *)
]
(*e: function [[Main.pfff_extra_actions]] *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*s: function [[Main.all_actions]] *)
let all_actions () =
  pfff_extra_actions() @
  (*s: [[Main.all_actions]] concatenated actions *)
  (* Test_parsing_generic.actions() @ *)
  (*x: [[Main.all_actions]] concatenated actions *)
  Test_parsing_ml.actions()@
  Test_analyze_ml.actions()@
  Test_parsing_skip.actions()@
  Test_parsing_scala.actions()@

  Test_parsing_php.actions()@
  Test_parsing_js.actions()@
  Test_parsing_json.actions()@
  Test_analyze_js.actions()@
  Test_parsing_python.actions()@
  Test_parsing_ruby.actions()@
  Test_analyze_ruby.actions()@

  Test_parsing_c.actions()@
  Test_parsing_cpp.actions()@
  Test_parsing_java.actions()@
  Test_parsing_go.actions()@

  Test_parsing_nw.actions()@
  Test_parsing_hs.actions()@
  Test_parsing_csharp.actions()@
  Test_parsing_rust.actions()@
  Test_parsing_erlang.actions()@

  Test_parsing_text.actions()@
  Test_parsing_html.actions()@
  (* TODO need dune file
      Test_parsing_css.actions()@
      Test_parsing_web.actions()@

      Test_parsing_sql.actions()@
  *)

  Test_parsing_lisp.actions()@

  (* beta *)


  (*  Test_analyze_generic.actions() @ *)
  (*
    Test_analyze_cpp.actions () ++
    Test_analyze_php.actions () ++
    Test_analyze_ml.actions () ++
    Test_analyze_c.actions() ++
  *)
  (*e: [[Main.all_actions]] concatenated actions *)
  []
(*e: function [[Main.all_actions]] *)

(*s: function [[Main.options]] *)
let options () = [
  "-verbose", Arg.Set verbose,
  " ";
  (*s: [[Main.options]] main cases *)
  "-lang", Arg.String (fun s ->
    lang := s;
    (*s: [[Main.options]] in [[-lang]] callback *)
    (* a big ugly *)
    (* Test_parsing_generic.lang := s; *)
    (*e: [[Main.options]] in [[-lang]] callback *)
  ), (spf " <str> choose language (default = %s)" !lang);
  (*x: [[Main.options]] main cases *)
  "-sgrep_mode", Arg.Set Flag_parsing.sgrep_mode,
  " enable sgrep mode parsing (to debug)";
  (*e: [[Main.options]] main cases *)
] @
  (*s: [[Main.options]] concatenated flags *)
  Flag_parsing.cmdline_flags_verbose () @
  Flag_parsing.cmdline_flags_debugging () @
  Meta_parse_info.cmdline_flags_precision () @

  Flag_parsing_cpp.cmdline_flags_debugging () @

  Flag_parsing_php.cmdline_flags_pp () @
  Flag_parsing_cpp.cmdline_flags_macrofile () @

  Common2.cmdline_flags_devel () @
  Common2.cmdline_flags_other () @
  (*e: [[Main.options]] concatenated flags *)
  (*s: [[Main.options]] concatenated actions *)
  Common.options_of_actions action (all_actions()) @
  (*e: [[Main.options]] concatenated actions *)
  [
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "pfff version: %s" Config_pfff.version);
      exit 0;
    ), "  guess what";
  ]
(*e: function [[Main.options]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Main.main]] *)
let main () =
  (*s: [[Main.main()]] tune the GC *)
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (*e: [[Main.main()]] tune the GC *)

  let usage_msg =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  if Sys.file_exists !log_config_file
  then begin
    Logging.load_config_file !log_config_file;
    logger#info "loaded %s" !log_config_file;
  end;

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with
     (*s: [[Main.main()]] match [[args]] actions *)
     (* --------------------------------------------------------- *)
     (* actions, useful to debug subpart *)
     (* --------------------------------------------------------- *)
     | xs when List.mem !action (Common.action_list (all_actions())) ->
         Common.do_action !action xs (all_actions())

     | _ when not (Common.null_string !action) ->
         failwith ("unrecognized action or wrong params: " ^ !action)
     (*e: [[Main.main()]] match [[args]] actions *)

     (* --------------------------------------------------------- *)
     (* main entry *)
     (* --------------------------------------------------------- *)
     | x::xs ->
         main_action (x::xs)

     (* --------------------------------------------------------- *)
     (* empty entry *)
     (* --------------------------------------------------------- *)
     | [] ->
         Common.usage usage_msg (options());
         failwith "too few arguments"
    )
  )
(*e: function [[Main.main]] *)

(*****************************************************************************)
(*s: toplevel [[Main._1]] *)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )
(*e: toplevel [[Main._1]] *)
(*e: pfff/cli/Main.ml *)

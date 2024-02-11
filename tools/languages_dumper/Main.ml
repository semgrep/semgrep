(*
 * Please imagine a long and boring GNU-style copyright notice
 * appearing just here.
 *)
open Common
module J = JSON

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
let main_action _xs = raise Todo
(*e: function [[Main.main_action]] *)

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(*s: function [[Main.test_json_pretty_printer]] *)
let test_json_pretty_printer file =
  let json = UChan.with_open_in (Fpath.v file) J.json_of_chan in
  let s = J.string_of_json json in
  UCommon.pr s
(*e: function [[Main.test_json_pretty_printer]] *)

(* ---------------------------------------------------------------------- *)
(*s: function [[Main.pfff_extra_actions]] *)
let pfff_extra_actions () =
  [
    ("-dump_json", " <file>", Arg_.mk_action_1_arg test_json_pretty_printer);
    (*s: [[Main.pfff_extra_actions]] other cases *)
    ("-json_pp", " <file>", Arg_.mk_action_1_arg test_json_pretty_printer)
    (*e: [[Main.pfff_extra_actions]] other cases *);
  ]
(*e: function [[Main.pfff_extra_actions]] *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*s: function [[Main.all_actions]] *)
let all_actions () = pfff_extra_actions ()

(*s: [[Main.all_actions]] concatenated actions *)
(* Test_parsing_generic.actions() @ *)
(*x: [[Main.all_actions]] concatenated actions *)
(*
  @ Test_parsing_ml.actions ()
  @ Test_parsing_scala.actions ()
  @ Test_parsing_php.actions ()
  @ Test_parsing_js.actions ()
  @ Test_parsing_json.actions ()
  @ Test_parsing_python.actions ()
  @ Test_parsing_c.actions ()
  @ Test_parsing_cpp.actions ()
  @ Test_parsing_java.actions ()
  @ Test_parsing_go.actions () @ []
*)
(*e: function [[Main.all_actions]] *)

(*s: function [[Main.options]] *)
let options () =
  [
    ("-verbose", Arg.Set verbose, " ");
    (*s: [[Main.options]] main cases *)
    ( "-lang",
      Arg.String
        (fun s ->
          lang := s
          (*s: [[Main.options]] in [[-lang]] callback *)
          (* a big ugly *)
          (* Test_parsing_generic.lang := s; *)
          (*e: [[Main.options]] in [[-lang]] callback *)),
      spf " <str> choose language (default = %s)" !lang );
    (*x: [[Main.options]] main cases *)
    ( "-sgrep_mode",
      Arg.Set Flag_parsing.sgrep_mode,
      " enable sgrep mode parsing (to debug)" )
    (*e: [[Main.options]] main cases *);
  ]
  (*s: [[Main.options]] concatenated flags *)
  @ Flag_parsing.cmdline_flags_verbose ()
  @ Flag_parsing.cmdline_flags_debugging ()
  @ Flag_parsing_cpp.cmdline_flags_debugging ()
  @ Flag_parsing_php.cmdline_flags_pp ()
  @ Flag_parsing_cpp.cmdline_flags_macrofile ()
  @ Common2.cmdline_flags_devel ()
  @ Common2.cmdline_flags_other ()
  (*e: [[Main.options]] concatenated flags *)
  (*s: [[Main.options]] concatenated actions *)
  @ Arg_.options_of_actions action (all_actions ())
  @ (*e: [[Main.options]] concatenated actions *)
  [
    ( "-version",
      Arg.Unit
        (fun () ->
          UCommon.pr2
            (spf "pfff version: %s" (*Config_pfff.version*) "DEPRECATED");
          exit 0),
      "  guess what" );
  ]
(*e: function [[Main.options]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Main.main]] *)
let main () =
  (*s: [[Main.main()]] tune the GC *)
  Gc.set { (Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024 };

  (*e: [[Main.main()]] tune the GC *)
  let usage_msg =
    "Usage: "
    ^ Filename.basename Sys.argv.(0)
    ^ " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Arg_.parse_options (options ()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () ->
      match args with
      (*s: [[Main.main()]] match [[args]] actions *)
      (* --------------------------------------------------------- *)
      (* actions, useful to debug subpart *)
      (* --------------------------------------------------------- *)
      | xs when List.mem !action (Arg_.action_list (all_actions ())) ->
          Arg_.do_action !action xs (all_actions ())
      | _ when not (String_.empty !action) ->
          failwith ("unrecognized action or wrong params: " ^ !action)
      (*e: [[Main.main()]] match [[args]] actions *)
      (* --------------------------------------------------------- *)
      (* main entry *)
      (* --------------------------------------------------------- *)
      | x :: xs -> main_action (x :: xs)
      (* --------------------------------------------------------- *)
      (* empty entry *)
      (* --------------------------------------------------------- *)
      | [] ->
          Arg_.usage usage_msg (options ());
          failwith "too few arguments")

(*****************************************************************************)

let _ = UCommon.main_boilerplate (fun () -> main ())

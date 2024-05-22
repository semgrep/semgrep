open Common
open Fpath_.Operators
module J = JSON
module E = Core_error

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Functions accessible from the CLI via -dump_xxx to help debug
 * Semgrep. See Core_CLI.actions() and Arg_helpers.ml for more info.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* used for the Dump AST in semgrep.live *)
let json_of_v (v : OCaml.v) =
  let rec aux v =
    match v with
    | OCaml.VUnit -> J.String "()"
    | OCaml.VBool v1 -> if v1 then J.String "true" else J.String "false"
    | OCaml.VFloat v1 -> J.Float v1 (* ppf "%f" v1 *)
    | OCaml.VChar v1 -> J.String (spf "'%c'" v1)
    | OCaml.VString v1 -> J.String v1
    | OCaml.VInt i -> J.Int (Int64.to_int i)
    | OCaml.VTuple xs -> J.Array (List_.map aux xs)
    | OCaml.VDict xs -> J.Object (List_.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ :: _ :: _ -> J.Object [ (s, J.Array (List_.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%Ld" s i64)
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (List_.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

(* mostly a copy paste of Test_analyze_generic.ml *)
let dump_il_all (caps : < Cap.stdout >) file =
  let ast = Parse_target.parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let xs = AST_to_IL.stmt lang (AST_generic.stmt1 ast) in
  xs |> List.iter (fun stmt -> CapConsole.print caps#stdout (IL.show_stmt stmt))
[@@action]

let dump_il (caps : < Cap.stdout >) file =
  let module G = AST_generic in
  let print s = CapConsole.print caps#stdout s in
  let ast = Parse_target.parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let report_func_def_with_name ent_opt fdef =
    let name =
      match ent_opt with
      | None -> "<lambda>"
      | Some { G.name = EN n; _ } -> G.show_name n
      | Some _ -> "<entity>"
    in
    print (spf "Function name: %s" name);
    let s =
      AST_generic.show_any
        (G.S (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody))
    in
    print s;
    print "==>";

    (* Creating a CFG and throwing it away here so the implicit return
     * analysis pass may be run in order to mark implicit return nodes.
     *)
    let _ = CFG_build.cfg_of_fdef lang fdef in

    (* This round, the IL stmts will show return nodes when
     * they were implicit before.
     *)
    let _, xs = AST_to_IL.function_definition lang fdef in
    let s = IL.show_any (IL.Ss xs) in
    print s
  in
  Visit_function_defs.visit report_func_def_with_name ast
[@@action]

let dump_v1_json caps ~(get_lang : unit -> Language.t option) file =
  let file = Core_scan.replace_named_pipe_by_regular_file caps file in
  let lang =
    match get_lang () with
    | Some x -> x
    | None -> (
        (* This guessing doesn't work with dockerfiles "foo.dockerfile"
           or "Dockerfile". Not sure why. *)
        match Lang.langs_of_filename file with
        | x :: _ -> x
        | [] -> failwith (spf "unsupported language for %s" !!file))
  in
  E.try_with_log_exn_and_reraise file (fun () ->
      let { Parsing_result2.ast; skipped_tokens; _ } =
        Parse_target.parse_and_resolve_name lang file
      in
      let v1 = AST_generic_to_v1.program ast in
      let s = Ast_generic_v1_j.string_of_program v1 in
      UCommon.pr s;
      if skipped_tokens <> [] then
        Logs.warn (fun m -> m "fail to fully parse %s" !!file))
[@@action]

let generate_ast_json file =
  match Lang.langs_of_filename file with
  | lang :: _ ->
      let ast = Parse_target.parse_and_resolve_name_warn_if_partial lang file in
      let v1 = AST_generic_to_v1.program ast in
      let s = Ast_generic_v1_j.string_of_program v1 in
      let file = !!file ^ ".ast.json" |> Fpath.v in
      UFile.write_file file s;
      Logs.info (fun m -> m "saved JSON output in %s" !!file)
  | [] -> failwith (spf "unsupported language for %s" !!file)
[@@action]

let dump_ext_of_lang (caps : < Cap.stdout >) () =
  let lang_to_exts =
    Lang.keys
    |> List_.map (fun lang_str ->
           match Lang.of_string_opt lang_str with
           | Some lang ->
               lang_str ^ "->" ^ String.concat ", " (Lang.ext_of_lang lang)
           | None -> "")
  in
  CapConsole.print caps#stdout
    (spf "Language to supported file extension mappings:\n %s"
       (String.concat "\n" lang_to_exts))
[@@action]

let dump_equivalences (caps : < Cap.stdout >) file =
  let xs = Parse_equivalences.parse file in
  CapConsole.print caps#stdout (Dumper.dump xs)
[@@action]

let dump_rule (caps : < Cap.tmp >) file =
  let file = Core_scan.replace_named_pipe_by_regular_file caps file in
  let rules = Parse_rule.parse file in
  rules |> List.iter (fun r -> UCommon.pr (Rule.show r))
[@@action]

let prefilter_of_rules file =
  let cache = Some (Hashtbl.create 101) in
  let rules = Parse_rule.parse file in
  let xs =
    rules
    |> List_.map (fun r ->
           let pre_opt = Analyze_rule.regexp_prefilter_of_rule ~cache r in
           let pre_atd_opt =
             Option.map Analyze_rule.prefilter_formula_of_prefilter pre_opt
           in
           let id = r.Rule.id |> fst in
           {
             Semgrep_prefilter_t.rule_id = Rule_ID.to_string id;
             filter = pre_atd_opt;
           })
  in
  let s = Semgrep_prefilter_j.string_of_prefilters xs in
  UCommon.pr s
[@@action]

(* This is called from 'pysemgrep ci' to get contributors from
 * 'git log'. This must print the JSON on stdout as it is
 * processed by core_runner.py
 *)
let dump_contributions () =
  Parse_contribution.get_contributions ()
  |> Semgrep_output_v1_j.string_of_contributions |> UCommon.pr
[@@action]

(*****************************************************************************)
(* Other actions *)
(*****************************************************************************)

(* [test_rules dirs] run the tests discovered under [dirs]
 * and print a summary.
 * This is what 'semgrep-core -test_rules' run.
 *)
let test_rules (caps : < Cap.stdout ; Cap.exit >) (paths : Fpath.t list) : unit
    =
  let total_mismatch = ref 0 in
  let fail_callback num_errors _msg =
    total_mismatch := !total_mismatch + num_errors
  in
  let tests = Test_engine.make_tests ~fail_callback paths in
  tests |> List.iter (fun (test : Testo.t) -> test.func ());
  CapConsole.print caps#stdout (spf "total mismatch: %d" !total_mismatch);
  if !total_mismatch > 0 then CapStdlib.exit caps#exit 1
[@@action]

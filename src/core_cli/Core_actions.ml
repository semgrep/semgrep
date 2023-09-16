open Common
open File.Operators
module J = JSON
module E = Core_error

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Functions accessible from the CLI via -dump_xxx to help debug
 * Semgrep. See Core_CLI.actions() and Arg_helpers.ml for more info.
 *)

(*****************************************************************************)
(* Dumpers *)
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
    | OCaml.VInt i -> J.Int i
    | OCaml.VTuple xs -> J.Array (Common.map aux xs)
    | OCaml.VDict xs -> J.Object (Common.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ :: _ :: _ -> J.Object [ (s, J.Array (Common.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%d" s (Int64.to_int i64))
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (Common.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

(* temporary *)
let dump_elixir_raw_ast file =
  let x = Parse_elixir_tree_sitter.parse file in
  match x.program with
  | Some x -> pr (AST_elixir.show_program x)
  | None -> failwith (spf "could not parse %s" file)

let dump_elixir_ast file =
  let x = Parse_elixir_tree_sitter.parse file in
  match x.program with
  | Some x ->
      let x = Elixir_to_elixir.map_program x in
      pr (AST_elixir.show_program x)
  | None -> failwith (spf "could not parse %s" file)

(* mostly a copy paste of Test_analyze_generic.ml *)
let dump_il_all file =
  let ast = Parse_target.parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let xs = AST_to_IL.stmt lang (AST_generic.stmt1 ast) in
  List.iter (fun stmt -> pr2 (IL.show_stmt stmt)) xs
  [@@action]

let dump_il file =
  let module G = AST_generic in
  let ast = Parse_target.parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let report_func_def_with_name ent_opt fdef =
    let name =
      match ent_opt with
      | None -> "<lambda>"
      | Some { G.name = EN n; _ } -> G.show_name n
      | Some _ -> "<entity>"
    in
    pr2 (spf "Function name: %s" name);
    let s =
      AST_generic.show_any
        (G.S (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody))
    in
    pr2 s;
    pr2 "==>";

    let _, xs = AST_to_IL.function_definition lang fdef in
    let s = IL.show_any (IL.Ss xs) in
    pr2 s
  in
  Visit_function_defs.visit report_func_def_with_name ast

let dump_v1_json file =
  let file = Core_scan.replace_named_pipe_by_regular_file file in
  match Lang.langs_of_filename file with
  | lang :: _ ->
      E.try_with_print_exn_and_reraise !!file (fun () ->
          let { Parsing_result2.ast; skipped_tokens; _ } =
            Parse_target.parse_and_resolve_name lang !!file
          in
          let v1 = AST_generic_to_v1.program ast in
          let s = Ast_generic_v1_j.string_of_program v1 in
          pr s;
          if skipped_tokens <> [] then
            pr2 (spf "WARNING: fail to fully parse %s" !!file))
  | [] -> failwith (spf "unsupported language for %s" !!file)

let generate_ast_json file =
  match Lang.langs_of_filename file with
  | lang :: _ ->
      let ast =
        Parse_target.parse_and_resolve_name_warn_if_partial lang !!file
      in
      let v1 = AST_generic_to_v1.program ast in
      let s = Ast_generic_v1_j.string_of_program v1 in
      let file = !!file ^ ".ast.json" |> Fpath.v in
      File.write_file file s;
      pr2 (spf "saved JSON output in %s" !!file)
  | [] -> failwith (spf "unsupported language for %s" !!file)

let generate_ast_binary lang file =
  let final =
    Parse_with_caching.ast_cached_value_of_file Version.version lang file
  in
  let file = Fpath.(file + Parse_with_caching.binary_suffix) in
  assert (Parse_with_caching.is_binary_ast_filename file);
  Common2.write_value final !!file;
  pr2 (spf "saved marshalled generic AST in %s" !!file)

let dump_ext_of_lang () =
  let lang_to_exts =
    Lang.keys
    |> Common.map (fun lang_str ->
           match Lang.of_string_opt lang_str with
           | Some lang ->
               lang_str ^ "->" ^ String.concat ", " (Lang.ext_of_lang lang)
           | None -> "")
  in
  pr2
    (spf "Language to supported file extension mappings:\n %s"
       (String.concat "\n" lang_to_exts))

let dump_equivalences file =
  let file = Core_scan.replace_named_pipe_by_regular_file file in
  let xs = Parse_equivalences.parse file in
  pr2_gen xs

let dump_rule file =
  let file = Core_scan.replace_named_pipe_by_regular_file file in
  let rules = Parse_rule.parse file in
  rules |> List.iter (fun r -> pr (Rule.show r))

let prefilter_of_rules file =
  let rules = Parse_rule.parse file in
  let xs =
    rules
    |> Common.map (fun r ->
           let pre_opt = Analyze_rule.regexp_prefilter_of_rule r in
           let pre_atd_opt =
             Option.map Analyze_rule.prefilter_formula_of_prefilter pre_opt
           in
           let id = r.Rule.id |> fst in
           {
             Semgrep_prefilter_t.rule_id = (id :> string);
             filter = pre_atd_opt;
           })
  in
  let s = Semgrep_prefilter_j.string_of_prefilters xs in
  pr s

(* This is called from 'pysemgrep ci' to get contributors from
 * 'git log'. This must print the JSON on stdout as it is
 * processed by core_runner.py
 *)
let dump_contributions () =
  Parse_contribution.get_contributions ()
  |> Semgrep_output_v1_j.string_of_contributions |> pr

(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Common
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
    | OCaml.VTuple xs -> J.Array (List.map aux xs)
    | OCaml.VDict xs -> J.Object (List.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ :: _ :: _ -> J.Object [ (s, J.Array (List.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%Ld" s i64)
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (List.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

(* used to be in Core_error.mli but better here as should be used
 * only in test code.
 * val try_with_log_exn_and_reraise : Fpath.t -> (unit -> 'a) -> 'a
 *)
let try_with_log_exn_and_reraise (file : Fpath.t) f =
  try f () with
  | Time_limit.Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      let err = E.exn_to_error ~file e in
      Logs.err (fun m -> m "%s" (E.string_of_error err));
      Exception.reraise e

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let dump_il file =
  let module G = AST_generic in
  let print s = UConsole.print s in
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

    Implicit_return.mark_implicit_return lang ast;

    (* This round, the IL stmts will show return nodes when
     * they were implicit before.
     *)
    let IL.{ fbody = xs; _ } = AST_to_IL.function_definition lang fdef in
    let s = IL.show_any (IL.Ss xs) in
    print s
  in
  Visit_function_defs.visit report_func_def_with_name ast
[@@action]

let pp_il file =
  let ast = Parse_target.parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  Implicit_return.mark_implicit_return lang ast;
  let xs = AST_to_IL.program lang ast in
  let output = Pretty_IL.program xs in
  UConsole.print output
[@@action]

let dump_exts_of_lang () =
  let lang_to_exts =
    Lang.keys
    |> List.map (fun lang_str ->
           match Lang.of_string_opt lang_str with
           | Some lang ->
               lang_str ^ "->" ^ String.concat ", " (Lang.exts_of_lang lang)
           | None -> "")
  in
  UConsole.print
    (spf "Language to supported file extension mappings:\n %s"
       (String.concat "\n" lang_to_exts))
[@@action]

let dump_rule (file : Fpath.t) : unit =
  let rules = Parse_rule.parse file in
  (* TODO: handle parse errors gracefully instead of silently ignoring *)
  rules |> Result.iter (List.iter (fun r -> UCommon.pr (Rule.show r)))
[@@action]

(*****************************************************************************)
(* Other non-dumpers actions *)
(*****************************************************************************)

let prefilter_of_rules ~interfile file =
  match Parse_rule.parse file with
  | Ok rules ->
      let xs =
        rules
        |> List.map (fun r ->
               let pre_opt = Prefiltering.File.of_rule ~interfile r in
               let pre_atd_opt =
                 Option.map Prefiltering.File.to_semgrep_formula pre_opt
               in
               let id = r.Rule.id |> fst in
               {
                 Prefiltering.Semgrep_prefilter_t.rule_id = Rule_ID.to_string id;
                 filter = pre_atd_opt;
               })
      in
      let s = Prefiltering.Semgrep_prefilter_j.string_of_prefilters xs in
      UCommon.pr s
  (* TODO: handle parse errors gracefully instead of silently ignoring *)
  | Error _ -> ()
[@@action]

module S = Sarif.Sarif_v_2_1_0_v

let sarif_sort (file : Fpath.t) =
  let str = UFile.read_file file in
  let (x : S.sarif_json_schema) =
    Sarif.Sarif_v_2_1_0_j.sarif_json_schema_of_string str
  in
  let x =
    {
      x with
      runs =
        x.runs
        |> List.map (fun (r : S.run) ->
               {
                 r with
                 invocations =
                   r.invocations
                   |> Option.map
                        (List.map (fun (i : S.invocation) ->
                             {
                               i with
                               tool_execution_notifications =
                                 i.tool_execution_notifications
                                 |> Option.map
                                      (List.sort
                                         (fun
                                           (a : S.notification)
                                           (b : S.notification)
                                         ->
                                           match
                                             (a.message.text, b.message.text)
                                           with
                                           | Some a1, Some b1 -> compare a1 b1
                                           | _else_ -> failwith "wrong format"));
                             }));
                 results =
                   r.results
                   |> Option.map
                        (List.sort (fun (a : S.result) (b : S.result) ->
                             match (a.fingerprints, b.fingerprints) with
                             | ( Some [ ("matchBasedId/v1", a1) ],
                                 Some [ ("matchBasedId/v1", b1) ] ) ->
                                 compare a1 b1
                             | _else_ -> failwith "wrong format"));
                 tool =
                   {
                     r.tool with
                     driver =
                       {
                         r.tool.driver with
                         rules =
                           r.tool.driver.rules
                           |> Option.map
                                (List.sort
                                   (fun
                                     (a : S.reporting_descriptor)
                                     (b : S.reporting_descriptor)
                                   -> compare a.id b.id));
                       };
                   };
               });
    }
  in
  let str = Sarif.Sarif_v_2_1_0_j.string_of_sarif_json_schema x in
  UCommon.pr str
[@@action]

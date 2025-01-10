(* Cooper Pierce
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module FT = File_type
open Rule

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Translating pattern formulae back into YAML.
 *
 * This is for the purpose of migration of old-style rule syntax, at the time of
 * this file being written. We want to be able to embed old-style rules into our
 * common formula type, and then get valid YAML back out of it.
 *
 * That's what this code does.
 *)

(*****************************************************************************)
(* Main translation logic *)
(*****************************************************************************)

let range_to_string (range : (Tok.location * Tok.location) option) =
  match range with
  | Some (start, end_) ->
      UFile.with_open_in start.pos.file (fun chan ->
          let extract_size = end_.pos.bytepos - start.pos.bytepos in
          seek_in chan start.pos.bytepos;
          really_input_string chan extract_size)
  | None -> failwith "invalid source/sink requires"

let translate_entropy_v2_options mode =
  ( "options",
    `O
      [
        ( "mode",
          `String
            (match mode with
            | Lax -> "lax"
            | Strict -> "strict"
            (* kb - how2optional node*)
            | Default -> "default") );
      ] )

let rec translate_metavar_cond cond : [> `O of (string * Yaml.value) list ] =
  match cond with
  | CondEval e -> `O [ ("comparison", `String (range_to_string e.e_range)) ]
  | CondType (mv, lang, strs, _) ->
      `O
        ([
           ("metavariable", `String mv);
           ("types", `A (List_.map (fun s -> `String s) strs));
         ]
        @
        match lang with
        | None -> []
        | Some x -> [ ("language", `String (Analyzer.to_string x)) ])
  | CondName { mvar; kind; modules } ->
      `O
        ([ ("metavariable", `String mvar) ]
        @ (match kind with
          | Some DjangoView -> [ ("kind", `String "django-view") ]
          | Some ExpressApp -> [ ("kind", `String "express-app") ]
          | Some ExpressController -> [ ("kind", `String "express-controller") ]
          | None -> [])
        @
        match modules with
        | Some [ name ] -> [ ("module", `String name) ]
        | Some names ->
            [ ("modules", `A (List_.map (fun n -> `String n) names)) ]
        | None -> [])
  | CondRegexp (mv, re_str, _) ->
      `O [ ("metavariable", `String mv); ("regex", `String re_str) ]
  | CondAnalysis (mv, analysis) ->
      let analyzer, options =
        match analysis with
        | CondEntropy -> ("entropy", [])
        | CondEntropyV2 options ->
            ("entropy_v2", [ translate_entropy_v2_options options ])
        | CondReDoS -> ("redos", [])
      in
      `O
        ([ ("metavariable", `String mv); ("analyzer", `String analyzer) ]
        @ options)
  | CondNestedFormula (mv, lang, f) ->
      let (`O fs) = translate_formula f in
      `O
        ([ ("metavariable", `String mv) ]
        @ fs
        @
        match lang with
        | None -> []
        | Some x -> [ ("language", `String (Analyzer.to_string x)) ])

and translate_taint_source
    {
      source_id = _;
      source_formula;
      source_exact;
      source_by_side_effect;
      label;
      source_control;
      source_requires;
    } : [> `O of (string * Yaml.value) list ] =
  let (`O source_f) = translate_formula source_formula in
  let label_obj =
    if label = Rule.default_source_label then []
    else [ ("label", `String label) ]
  in
  let requires_obj =
    match source_requires with
    | None -> []
    | Some { range; _ } -> [ ("requires", `String (range_to_string range)) ]
  in
  let exact_obj = if source_exact then [ ("exact", `Bool true) ] else [] in
  let side_effect_obj =
    match source_by_side_effect with
    | Yes -> [ ("by-side-effect", `Bool true) ]
    | Only -> [ ("by-side-effect", `String "only") ]
    | No -> []
  in
  let control_obj =
    if source_control then [ ("control", `Bool true) ] else []
  in
  `O
    (List_.flatten
       [
         source_f;
         exact_obj;
         control_obj;
         label_obj;
         requires_obj;
         side_effect_obj;
       ])

and translate_taint_sink
    {
      sink_id = _;
      sink_formula;
      sink_exact;
      sink_requires;
      sink_at_exit;
      sink_has_focus = _;
    } : [> `O of (string * Yaml.value) list ] =
  let (`O sink_f) = translate_formula sink_formula in
  let exact_obj = if sink_exact then [] else [ ("exact", `Bool false) ] in
  let at_exit_obj = if sink_at_exit then [ ("at-exit", `Bool true) ] else [] in
  let requires_obj =
    match sink_requires with
    | None -> []
    | Some { range; _ } -> [ ("requires", `String (range_to_string range)) ]
  in
  `O (List_.flatten [ sink_f; exact_obj; requires_obj; at_exit_obj ])

and translate_taint_sanitizer
    {
      sanitizer_id = _;
      sanitizer_formula;
      sanitizer_exact;
      sanitizer_by_side_effect;
      not_conflicting;
    } : [> `O of (string * Yaml.value) list ] =
  let (`O san_f) = translate_formula sanitizer_formula in
  let side_effect_obj =
    if sanitizer_by_side_effect then [ ("by-side-effect", `Bool true) ] else []
  in
  let exact_obj = if sanitizer_exact then [ ("exact", `Bool true) ] else [] in
  let not_conflicting_obj =
    if not_conflicting then [ ("not-conflicting", `Bool true) ] else []
  in
  `O (List_.flatten [ san_f; exact_obj; side_effect_obj; not_conflicting_obj ])

and translate_taint_propagator
    {
      propagator_id = _;
      propagator_formula;
      propagator_by_side_effect;
      from;
      to_;
      propagator_requires;
      propagator_replace_labels;
      propagator_label;
    } : [> `O of (string * Yaml.value) list ] =
  let (`O prop_f) = translate_formula propagator_formula in
  let side_effect_obj =
    if propagator_by_side_effect then []
    else [ ("by-side-effect", `Bool false) ]
  in
  let label_obj =
    match propagator_label with
    | None -> []
    | Some label -> [ ("label", `String label) ]
  in
  let requires_obj =
    match propagator_requires with
    | None -> []
    | Some { range; _ } -> [ ("requires", `String (range_to_string range)) ]
  in
  let replace_labels_obj =
    match propagator_replace_labels with
    | None -> []
    | Some strs ->
        [ ("replace-labels", `A (List_.map (fun s -> `String s) strs)) ]
  in
  let from_obj = [ ("from", `String (fst from)) ] in
  let to_obj = [ ("to", `String (fst to_)) ] in
  `O
    (List_.flatten
       [
         prop_f;
         side_effect_obj;
         from_obj;
         to_obj;
         label_obj;
         requires_obj;
         replace_labels_obj;
       ])

and translate_taint_spec
    ({ sources; sanitizers; sinks; propagators } : taint_spec) :
    [> `O of (string * Yaml.value) list ] =
  let sanitizers =
    match sanitizers with
    | None -> []
    | Some (_, sanitizers) ->
        [ ("sanitizers", `A (List_.map translate_taint_sanitizer sanitizers)) ]
  in
  let propagators =
    match List_.map translate_taint_propagator propagators with
    | [] -> []
    | other -> [ ("propagators", `A other) ]
  in
  `O
    (List_.flatten
       [
         [ ("sources", `A (List_.map translate_taint_source (snd sources))) ];
         sanitizers;
         propagators;
         [ ("sinks", `A (List_.map translate_taint_sink (snd sinks))) ];
       ])

and translate_formula f : [> `O of (string * Yaml.value) list ] =
  let rec aux { f; focus; conditions; fix; as_ } =
    let mk_focus_obj (_, mv_list) =
      match mv_list with
      | [] ->
          (* probably shouldn't happen... *)
          []
      | [ mv ] -> [ `O [ ("focus", `String mv) ] ]
      | mvs -> [ `O [ ("focus", `A (List_.map (fun x -> `String x) mvs)) ] ]
    in
    let where_obj =
      match (focus, conditions) with
      | [], [] -> []
      | _ ->
          [
            ( "where",
              `A
                (List_.map
                   (fun (_, cond) -> translate_metavar_cond cond)
                   conditions
                @ List.concat_map mk_focus_obj focus) );
          ]
    in
    let fix_obj =
      match fix with
      | None -> []
      | Some fix -> [ ("fix", `String fix) ]
    in
    let as_obj =
      match as_ with
      | None -> []
      | Some mvar -> [ ("as", `String mvar) ]
    in
    let (`O objs) = aux' f in
    `O (objs @ where_obj @ fix_obj @ as_obj)
  and aux' kind =
    match kind with
    | P { pat; pstr; _ } -> (
        match pat with
        | Sem (_, _)
        | Spacegrep _
        | Aliengrep _ ->
            `O [ ("pattern", `String (fst pstr)) ]
        | Regexp _ -> `O [ ("regex", `String (fst pstr)) ])
    | Inside (_, f) -> `O [ ("inside", (aux f :> Yaml.value)) ]
    | Anywhere (_, f) -> `O [ ("anywhere", (aux f :> Yaml.value)) ]
    | Not (_, f) -> `O [ ("not", (aux f :> Yaml.value)) ]
    | And (_, conjuncts) ->
        `O [ ("all", `A (List_.map aux conjuncts :> Yaml.value list)) ]
    | Or (_, fs) -> `O [ ("any", `A (List_.map aux fs :> Yaml.value list)) ]
  in
  aux f

let rec json_to_yaml json : Yaml.value =
  match json with
  | JSON.Object fields ->
      `O (List_.map (fun (name, value) -> (name, json_to_yaml value)) fields)
  | Array items -> `A (List_.map json_to_yaml items)
  | String s -> `String s
  | Int i -> `Float (float_of_int i)
  | Float f -> `Float f
  | Bool b -> `Bool b
  | Null -> `Null

(* This function goes and replace the pattern in the original rule's structure,
   so that we can amend the original YAML rule that we parsed.
*)
let replace_pattern rule_fields translated_formula : (string * Yaml.value) list
    =
  List.concat_map
    (fun (name, value) ->
      (* Remove all taint fields, except replace sources with translation *)
      (* Keep mode: taint for now though, since taint patterns aren't yet
         a thing.
      *)
      if
        List.mem name
          [ "pattern-sinks"; "pattern-sanitizers"; "pattern-propagators" ]
      then []
      else if
        (* Replace top level pattern with translation *)
        List.mem name
          [
            "pattern";
            "patterns";
            "pattern-either";
            "pattern-regex";
            "pattern-sources";
          ]
      then translated_formula
      else [ (name, value) ])
    rule_fields

let translate_files fparser xs =
  let formulas_by_file =
    xs
    |> List_.map (fun file ->
           Logs.info (fun m -> m "translate_files: processing %s" !!file);
           let formulas =
             match fparser file with
             | Ok rules ->
                 rules
                 |> List_.map (fun rule ->
                        match rule.mode with
                        | `Search formula
                        | `Extract { formula; _ } ->
                            [
                              ( "match",
                                (formula |> translate_formula :> Yaml.value) );
                            ]
                        | `Taint spec ->
                            [
                              ( "taint",
                                (translate_taint_spec spec :> Yaml.value) );
                            ]
                        | `SCA _ ->
                            failwith "sca rules not currently translated"
                        | `Steps _ ->
                            failwith "step rules not currently handled")
             | Error e ->
                 failwith ("parsing failure: " ^ Rule_error.string_of_error e)
           in
           (file, formulas))
  in
  List.iter
    (fun (file, formulas) ->
      let rules =
        match FT.file_type_of_file file with
        | FT.Config FT.Json ->
            UFile.read_file file |> JSON.json_of_string |> json_to_yaml
        | FT.Config FT.Yaml ->
            Yaml.of_string (UFile.read_file file) |> Result.get_ok
        | _ ->
            Logs.err (fun m ->
                m
                  "Wrong rule format, only JSON/YAML/JSONNET are valid. Trying \
                   to parse %s as YAML"
                  !!file);
            Yaml.of_string (UFile.read_file file) |> Result.get_ok
      in
      match rules with
      | `O [ ("rules", `A rules) ] ->
          let new_rules =
            List_.map2
              (fun rule new_formula ->
                match rule with
                | `O rule_fields -> `O (replace_pattern rule_fields new_formula)
                | _ -> failwith "wrong syntax")
              rules formulas
          in
          `O [ ("rules", `A new_rules) ]
          |> Yaml.to_string ~len:5242880 ~encoding:`Utf8 ~layout_style:`Block
               ~scalar_style:`Literal
          |> Result.get_ok |> UCommon.pr
      | _ -> failwith "wrong syntax")
    formulas_by_file

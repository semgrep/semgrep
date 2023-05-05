(* Cooper Pierce
 *
 * Copyright (C) 2019-2021 r2c
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
open File.Operators
module FT = File_type
open Rule

let logger = Logging.get_logger [ __MODULE__ ]

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

let rec expr_to_string expr =
  let { AST_generic.e_range; _ } = expr in
  match e_range with
  | Some (start, end_) ->
      Common.with_open_infile start.pos.file (fun chan ->
          let extract_size = end_.pos.charpos - start.pos.charpos in
          seek_in chan start.pos.charpos;
          really_input_string chan extract_size)
  | None -> failwith "invalid source/sink requires"

and translate_metavar_cond cond : [> `O of (string * Yaml.value) list ] =
  match cond with
  | CondEval e -> `O [ ("comparison", `String (expr_to_string e)) ]
  | CondRegexp (mv, re_str, _) ->
      `O [ ("metavariable", `String mv); ("regex", `String re_str) ]
  | CondAnalysis (mv, analysis) ->
      `O
        [
          ("metavariable", `String mv);
          ( "analyzer",
            `String
              (match analysis with
              | CondEntropy -> "entropy"
              | CondReDoS -> "redos") );
        ]
  | CondNestedFormula (mv, lang, f) ->
      let (`O fs) = translate_formula f in
      `O
        ([ ("metavariable", `String mv) ]
        @ fs
        @
        match lang with
        | None -> []
        | Some x -> [ ("language", `String (Xlang.to_string x)) ])

and translate_formula f : [> `O of (string * Yaml.value) list ] =
  match f with
  | P { pat; pstr; _ } -> (
      match pat with
      | Sem (_, _)
      | Spacegrep _
      | Aliengrep _ ->
          `O [ ("pattern", `String (fst pstr)) ]
      | Regexp _ -> `O [ ("regex", `String (fst pstr)) ])
  | Inside (_, f) -> `O [ ("inside", (translate_formula f :> Yaml.value)) ]
  | And (_, { conjuncts; focus; conditions; _ }) ->
      `O
        (("all", `A (Common.map translate_formula conjuncts :> Yaml.value list))
        ::
        (if focus =*= [] && conditions =*= [] then []
        else
          [
            ( "where",
              `A
                (Common.map
                   (fun (_, cond) -> translate_metavar_cond cond)
                   conditions
                @ Common.map
                    (fun (_, mv_list) ->
                      `O
                        [
                          ( "focus",
                            `A (Common.map (fun mvar -> `String mvar) mv_list)
                          );
                        ])
                    focus) );
          ]))
  | Or (_, fs) ->
      `O [ ("any", `A (Common.map translate_formula fs :> Yaml.value list)) ]
  | Not (_, f) -> `O [ ("not", (translate_formula f :> Yaml.value)) ]

let rec json_to_yaml json : Yaml.value =
  match json with
  | JSON.Object fields ->
      `O (Common.map (fun (name, value) -> (name, json_to_yaml value)) fields)
  | Array items -> `A (Common.map json_to_yaml items)
  | String s -> `String s
  | Int i -> `Float (float_of_int i)
  | Float f -> `Float f
  | Bool b -> `Bool b
  | Null -> `Null

let replace_pattern rule_fields translated_formula =
  List.concat_map
    (fun (name, value) ->
      (* Remove all taint fields, except replace sources with translation *)
      if name = "mode" && value =*= `String "taint" then []
      else if
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
      then [ ("match", translated_formula) ]
      else [ (name, value) ])
    rule_fields

let translate_files fparser xs =
  let formulas_by_file =
    xs
    |> Common.map (fun file ->
           logger#info "processing %s" !!file;
           let formulas =
             fparser file
             |> Common.map (fun rule ->
                    match rule.mode with
                    | `Search formula
                    | `Extract { formula; _ } ->
                        (formula |> translate_formula :> Yaml.value)
                    | _ ->
                        failwith
                          "Cannot translate taint mode rules at the moment")
           in
           (file, formulas))
  in
  List.iter
    (fun (file, formulas) ->
      let rules =
        match FT.file_type_of_file file with
        | FT.Config FT.Json ->
            File.read_file file |> JSON.json_of_string |> json_to_yaml
        | FT.Config FT.Yaml ->
            Yaml.of_string (File.read_file file) |> Result.get_ok
        | _ ->
            logger#error "wrong rule format, only JSON/YAML/JSONNET are valid";
            logger#info "trying to parse %s as YAML" !!file;
            Yaml.of_string (File.read_file file) |> Result.get_ok
      in
      match rules with
      | `O [ ("rules", `A rules) ] ->
          let new_rules =
            Common.map2
              (fun rule new_formula ->
                match rule with
                | `O rule_fields -> `O (replace_pattern rule_fields new_formula)
                | _ -> failwith "wrong syntax")
              rules formulas
          in
          `O [ ("rules", `A new_rules) ]
          |> Yaml.to_string ~len:5242880 ~encoding:`Utf8 ~layout_style:`Block
          |> Result.get_ok |> pr
      | _ -> failwith "wrong syntax")
    formulas_by_file

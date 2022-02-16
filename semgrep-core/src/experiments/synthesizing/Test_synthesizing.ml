(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module J = JSON
module In = Input_to_core_j

let expr_at_range s file =
  let r = Range.range_of_linecol_spec s file in
  pr2_gen r;
  let ast = Parse_target.parse_program file in
  (* just to see if it works with Naming on *)
  let lang = Lang.langs_of_filename file |> List.hd in
  Naming_AST.resolve lang ast;
  let e_opt = Range_to_AST.expr_at_range r ast in
  match e_opt with
  | Some e -> pr (AST_generic.show_expr e)
  | None -> failwith (spf "could not find an expr at range %s in %s" s file)
  [@@action]

let synthesize_patterns s file =
  let config = Config_semgrep.default_config in
  let options = Synthesizer.synthesize_patterns config s file in
  let json_opts = J.Object (List.map (fun (k, v) -> (k, J.String v)) options) in
  let s = J.string_of_json json_opts in
  pr s
  [@@action]

let generate_pattern_choices s =
  let config = Config_semgrep.default_config in
  let options = Synthesizer.print_pattern_from_targets config s in
  List.iter (fun s -> pr s) options
  [@@action]

let range_of_ast ast = Range.range_of_tokens (Visitor_AST.ii_of_any ast)

let locate_function_from_diff f =
  let str = Common.read_file f in
  let diff_files = In.diff_files_of_string str in
  let functions_from_file f =
    let file = f.In.filename in
    let function_from_range range =
      let r = Range.range_of_linecol_spec range file in
      let file_ast = Parse_target.parse_program file in
      let func = Range_to_AST.function_at_range r file_ast in
      match func with
      | None -> ()
      | Some func ->
          let func_r =
            let r2_opt = range_of_ast func in
            match r2_opt with
            (* NoTokenLocation issue for the expression, should fix! *)
            | None -> failwith "No range found"
            | Some r2 -> r2
          in
          let func_str = Range.content_at_range file func_r in
          pr2 func_str
    in
    List.iter function_from_range f.In.diffs
  in
  List.iter functions_from_file diff_files
  [@@action]

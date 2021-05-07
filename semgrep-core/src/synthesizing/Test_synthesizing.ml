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

let expr_at_range s file =
  let r = Range.range_of_linecol_spec s file in
  pr2_gen r;
  let ast = Parse_target.parse_program file in
  (* just to see if it works with Naming on *)
  let lang = Lang.langs_of_filename file |> List.hd in
  Naming_AST.resolve lang ast;
  let e_opt = Range_to_AST.expr_at_range r ast in
  (match e_opt with
   | Some e -> pr (AST_generic.show_expr e)
   | None -> failwith (spf "could not find an expr at range %s in %s" s file)
  )
[@@action]

let synthesize_patterns s file =
  let config = Config_semgrep.default_config in
  let options = Synthesizer.synthesize_patterns config s file in
  let json_opts =
    J.Object (List.map (fun (k, v) -> (k, J.String v)) options)
  in
  let s = J.string_of_json json_opts in
  pr s
[@@action]

let generate_pattern_choices s =
  let config = Config_semgrep.default_config in
  let options = Synthesizer.generate_pattern_choices config s in
  List.iter (fun s -> pr s) options
[@@action]

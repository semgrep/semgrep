(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
module J = JSON
module In = Input_to_core_j

let expr_at_range s file =
  let r = Range.range_of_linecol_spec s !!file in
  pr2_gen r;
  let ast = Parse_target.parse_program !!file in
  (* just to see if it works with Naming on *)
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let e_opt = Range_to_AST.expr_at_range r ast in
  match e_opt with
  | Some e -> pr (AST_generic.show_expr e)
  | None -> failwith (spf "could not find an expr at range %s in %s" s !!file)
[@@action]

let synthesize_patterns s file =
  let config = Rule_options.default_config in
  let options = Synthesizer.synthesize_patterns config s file in
  let json_opts =
    J.Object (Common.map (fun (k, v) -> (k, J.String v)) options)
  in
  let s = J.string_of_json json_opts in
  pr s
[@@action]

let generate_pattern_choices s =
  let config = Rule_options.default_config in
  let options = Synthesizer.print_pattern_from_targets config s in
  List.iter (fun s -> pr s) options
[@@action]

let locate_patched_functions f =
  let res = Synthesizer.locate_patched_functions f in
  pr res
[@@action]

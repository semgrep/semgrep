(*s: pfff/lang_GENERIC/parsing/Lib_AST.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
(*e: pad/r2c copyright *)

module V = Visitor_AST
module M = Map_AST

(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

(*s: function [[Lib_AST.extract_info_visitor]] *)
let extract_info_visitor recursor =
  let globals = ref [] in
  let hooks = { V.default_visitor with
                V.kinfo = (fun (_k, _) i -> Common.push i globals);
              } in
  begin
    let vout = V.mk_visitor hooks in
    recursor vout;
    List.rev !globals
  end
(*e: function [[Lib_AST.extract_info_visitor]] *)

(*s: function [[Lib_AST.ii_of_any]] *)
let ii_of_any any =
  extract_info_visitor (fun visitor -> visitor any)
(*e: function [[Lib_AST.ii_of_any]] *)

let range_of_tokens tokens =
  List.filter Parse_info.is_origintok tokens
  |> Parse_info.min_max_ii_by_pos

let range_of_any any =
  let leftmost_token, rightmost_token =
    ii_of_any any
    |> range_of_tokens
  in
  (Parse_info.token_location_of_info leftmost_token,
   Parse_info.token_location_of_info rightmost_token)

(*****************************************************************************)
(* Abstract position and constness for comparison *)
(*****************************************************************************)

(* update: you should now use AST_generic.equal_any which internally
 * does not care about position information.
*)

let abstract_for_comparison_visitor recursor =
  let hooks = { M.default_visitor with
                M.kinfo = (fun (_k, _) i ->
                  { i with Parse_info.token = Parse_info.Ab }
                );
                M.kidinfo = (fun (k, _) ii ->
                  k { ii with AST_generic.id_constness = ref None }
                )
              } in
  begin
    let vout = M.mk_visitor hooks in
    recursor vout;
  end

let abstract_for_comparison_any x =
  abstract_for_comparison_visitor (fun visitor -> visitor.M.vany x)
(*e: pfff/lang_GENERIC/parsing/Lib_AST.ml *)

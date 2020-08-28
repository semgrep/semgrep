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
    V.kinfo = (fun (_k, _) i -> Common.push i globals)
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

(*****************************************************************************)
(* Abstract position *)
(*****************************************************************************)
(*s: function [[Lib_AST.abstract_position_visitor]] *)
let abstract_position_visitor recursor = 
  let hooks = { M.default_visitor with
    M.kinfo = (fun (_k, _) i -> 
      { i with Parse_info.token = Parse_info.Ab }
    )
  } in
  begin
    let vout = M.mk_visitor hooks in
    recursor vout;
  end
(*e: function [[Lib_AST.abstract_position_visitor]] *)
(*s: function [[Lib_AST.abstract_position_info_any]] *)
let abstract_position_info_any x = 
  abstract_position_visitor (fun visitor -> visitor.M.vany x)
(*e: function [[Lib_AST.abstract_position_info_any]] *)
(*e: pfff/lang_GENERIC/parsing/Lib_AST.ml *)

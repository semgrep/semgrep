(* Yoann Padioleau
 *
 * Copyright (C) 2019, 2020 r2c
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
module G = AST_generic
module H = AST_generic_helpers
module PI = Parse_info

(*****************************************************************************)
(* Extract tokens *)
(*****************************************************************************)

class ['self] extract_info_visitor =
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    method! visit_tok globals tok = Common.push tok globals

    method! visit_expr globals x =
      match x.e with
      (* Ignore the tokens from the expression str is aliased to *)
      | Alias ((_str, t), _e) -> Common.push t globals
      | _ -> super#visit_expr globals x
  end

let ii_of_any any =
  let v = new extract_info_visitor in
  let globals = ref [] in
  v#visit_any globals any;
  List.rev !globals
  [@@profiling]

let info_of_any any =
  match ii_of_any any with
  | x :: _ -> x
  | [] -> assert false

(*e: function [[Lib_AST.ii_of_any]] *)

let first_info_of_any any =
  let xs = ii_of_any any in
  let xs = List.filter Parse_info.is_origintok xs in
  let min, _max = Parse_info.min_max_ii_by_pos xs in
  min

(*****************************************************************************)
(* Extract ranges *)
(*****************************************************************************)

class ['self] range_visitor =
  let smaller t1 t2 =
    if compare t1.PI.charpos t2.PI.charpos < 0 then t1 else t2
  in
  let larger t1 t2 =
    if compare t1.PI.charpos t2.PI.charpos > 0 then t1 else t2
  in
  let incorporate_tokens ranges (left, right) =
    match !ranges with
    | None -> ranges := Some (left, right)
    | Some (orig_left, orig_right) ->
        ranges := Some (smaller orig_left left, larger orig_right right)
  in
  let incorporate_token ranges tok =
    if PI.is_origintok tok then
      let tok_loc = PI.unsafe_token_location_of_info tok in
      incorporate_tokens ranges (tok_loc, tok_loc)
  in
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    method! visit_tok ranges tok = incorporate_token ranges tok

    method! visit_expr ranges expr =
      match expr.e_range with
      | None -> (
          let saved_ranges = !ranges in
          ranges := None;
          super#visit_expr ranges expr;
          expr.e_range <- !ranges;
          match saved_ranges with
          | None -> ()
          | Some r -> incorporate_tokens ranges r)
      | Some range -> incorporate_tokens ranges range

    method! visit_stmt ranges stmt =
      match stmt.s_range with
      | None -> (
          let saved_ranges = !ranges in
          ranges := None;
          super#visit_stmt ranges stmt;
          stmt.s_range <- !ranges;
          match saved_ranges with
          | None -> ()
          | Some r -> incorporate_tokens ranges r)
      | Some range -> incorporate_tokens ranges range
  end

let extract_ranges :
    AST_generic.any -> (PI.token_location * PI.token_location) option =
  let v = new range_visitor in
  let ranges = ref None in
  fun any ->
    v#visit_any ranges any;
    let res = !ranges in
    ranges := None;
    res

let range_of_tokens tokens =
  List.filter PI.is_origintok tokens |> PI.min_max_ii_by_pos
  [@@profiling]

let range_of_any_opt any =
  (* Even if the ranges are cached, calling `extract_ranges` to get them
   * is extremely expensive (due to `mk_visitor`). Testing taint-mode
   * open-redirect rule on Django, we spent ~16 seconds computing range
   * info (despite caching). If we bypass `extract_ranges` as we do here,
   * that time drops to just ~1.5 seconds! *)
  match any with
  | G.E e when Option.is_some e.e_range -> e.e_range
  | G.S s when Option.is_some s.s_range -> s.s_range
  | G.Tk tok -> (
      match Parse_info.token_location_of_info tok with
      | Ok tok_loc -> Some (tok_loc, tok_loc)
      | Error _ -> None)
  | G.Anys [] -> None
  | _ -> extract_ranges any
  [@@profiling]

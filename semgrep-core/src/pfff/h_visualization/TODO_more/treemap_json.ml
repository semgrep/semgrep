(*s: treemap_json.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
(*e: Facebook copyright *)
open Common

module J = Json_type

open Treemap
open Figures

module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Json -> Treemap *)
(*****************************************************************************)

(*s: function treemap_of_json *)
(* cf json_of_treemap_basic below. Just do reverse operation *)
let rec treemap_of_json j =
  match j with
  | J.Object [
    "kind", J.String "Node";
    "label", J.String s;
    "children", J.Array xs;
  ] ->
      let children = xs |> List.map treemap_of_json in

      let sizes = children |> List.map Treemap.size_of_treemap_node in
      let size = Common2.sum sizes in

      let rect = {
        label = s;
        color = Color.black;
        size = size;
      }
      in
      Common2.Node ((rect, s), children)

  | J.Object [
    "kind", J.String "Leaf";
    "size", J.Int size;
    "color", J.String scolor;
    "label", J.String lbl;
  ] ->
      let rect = {
        label = lbl;
        color = Color.color_of_string scolor;
        size = size;
      }
      in
      Common2.Leaf (rect, (lbl, size))

  | _ ->
      failwith "wrong format"
(*e: function treemap_of_json *)

(*****************************************************************************)
(* Treemap -> Json *)
(*****************************************************************************)

(*s: function json_of_color *)
let json_of_color c = J.String (Color.string_of_color c)
(*e: function json_of_color *)

(*s: function json_of_treemap *)
(* I was first using ocamltarzan to auto generate the json_of, but it
 * leds to verbosity, so I ended up manually coding it.
*)
let rec (json_of_treemap: ('a, 'b) Treemap.treemap -> J.json_type)
  = function
    | Common2.Node (((rect, _a), xs)) ->
        let { size = _v_sizeTODO; color = _v_colorTODO; label = v_label } = rect in

        let bnds = [] in

        let children =
          J.Array (List.map json_of_treemap xs)
        in
        let bnd = ("children", children) in
        let bnds = bnd :: bnds in

        let arg = J.String v_label in
        let bnd = ("label", arg) in
        let bnds = bnd :: bnds in

        let arg = J.String "Node" in
        let bnd = ("kind", arg) in
        let bnds = bnd :: bnds in

        J.Object bnds

    | Common2.Leaf (rect, _b) ->
        let { size = v_size; color = v_color; label = v_label } = rect in

        let bnds = [] in
        let arg = J.String v_label in
        let bnd = ("label", arg) in
        let bnds = bnd :: bnds in
        let arg = json_of_color v_color in
        let bnd = ("color", arg) in
        let bnds = bnd :: bnds in
        let arg = J.Int v_size in
        let bnd = ("size", arg) in
        let bnds = bnd :: bnds in

        let arg = J.String "Leaf" in
        let bnd = ("kind", arg) in
        let bnds = bnd :: bnds in
        J.Object bnds
(*e: function json_of_treemap *)


(*****************************************************************************)
(* Treemap rendering *)
(*****************************************************************************)

let rec vof_rectangle { p = v_p; q = v_q } =
  let bnds = [] in
  let arg = vof_point v_q in
  let bnd = ("q", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_point v_p in
  let bnd = ("p", arg) in let bnds = bnd :: bnds in OCaml.VDict bnds
and vof_point { x = v_x; y = v_y } =
  let bnds = [] in
  let arg = OCaml.vof_float v_y in
  let bnd = ("y", arg) in
  let bnds = bnd :: bnds in
  let arg = OCaml.vof_float v_x in
  let bnd = ("x", arg) in let bnds = bnd :: bnds in OCaml.VDict bnds

let rec vof_treemap_rendering v = OCaml.vof_list vof_treemap_rectangle v
and
  vof_treemap_rectangle {
    tr_rect = v_tr_rect;
    tr_color = v_tr_color;
    tr_label = v_tr_label;
    tr_depth = v_tr_depth;
    tr_is_node = v_tr_is_node;
  } =
  let bnds = [] in
  let arg = OCaml.vof_bool v_tr_is_node in
  let bnd = ("tr_is_node", arg) in
  let bnds = bnd :: bnds in
  let arg = OCaml.vof_int v_tr_depth in
  let bnd = ("tr_depth", arg) in
  let bnds = bnd :: bnds in
  let arg = OCaml.vof_string v_tr_label in
  let bnd = ("tr_label", arg) in
  let bnds = bnd :: bnds in
  let arg = OCaml.vof_int v_tr_color in
  let bnd = ("tr_color", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_rectangle v_tr_rect in
  let bnd = ("tr_rect", arg) in
  let bnds = bnd :: bnds in
  OCaml.VDict bnds


let json_of_treemap_rendering rendering =
  let v = vof_treemap_rendering rendering in
  OCaml.json_of_v v

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)
(*s: function test_json_of *)
let test_json_of dir =
  let maxc = 256 in
  let tree = tree_of_dirs_or_files
      ~file_hook:(fun file -> Common2.filesize file) [dir] in
  let treemap = treemap_of_tree
      ~size_of_leaf:(fun (_f, intleaf) -> intleaf)
      ~color_of_leaf:(fun (_f, _intleaf) ->
        Color.rgb (Random.int maxc) (Random.int maxc) (Random.int maxc)
      )
      ~label_of_dir:(fun dir -> Filename.basename dir)
      ~label_of_file:(fun (f, _intleaf) -> f)
      tree
  in
  let json =
    json_of_treemap
      (*
      (fun _ -> J.Null)
      (fun _ -> J.Null)
      *)
      treemap in
  let s = Json_out.string_of_json json in
  pr s
(*e: function test_json_of *)

(*s: function test_of_json *)
let test_of_json file =
  let json = Json_in.load_json file in
  let treemap = treemap_of_json json in

  let json2 = json_of_treemap treemap in
  let s = Json_out.string_of_json json2 in
  pr s
(*e: function test_of_json *)


(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  (*s: treemap_json actions *)
  "-test_json_of", "<dir>",
  Common.mk_action_1_arg test_json_of;
  "-test_of_json", "<file>",
  Common.mk_action_1_arg test_of_json;
  (*e: treemap_json actions *)

]

(*e: treemap_json.ml *)

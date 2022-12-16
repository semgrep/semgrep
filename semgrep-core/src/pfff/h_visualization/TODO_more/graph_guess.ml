(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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
open Common

module G = Graph

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Wrappers to use GUESS (http://guess.wikispot.org/Front_Page), to
 * generate data in its GDF format.
 *
 *
 * todo?
 * colorize(dir)
 * g.nodes.labelvisible=true
 *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(* see http://guess.wikispot.org/The_GUESS_.gdf_format *)

let to_gdf g ~str_of_node ~output =
  Common.with_open_outfile output (fun (pr_no_nl, _chan) ->
    let nodes = G.nodes g in
    let pr s = pr_no_nl (s ^ "\n") in

    let node_name_of_n n =
      let s = str_of_node n in
      let (d,b,e) = Common.dbe_of_filename_noext_ok s in

      match e with
      | "ml" ->
          let str = String.capitalize b in
          if str = "Math" then "Math_xxx"
          else str
      | "mli" -> b ^ "." ^ e
      | "NOEXT" ->
          (* can be directory like external/foo *)
          s ^ "/"
      | _ ->
          failwith (spf "PB: weird node: %s" s);
    in

    let dirs_of_n n =
      let s = str_of_node n in
      let (d,b,e) = Common.dbe_of_filename_noext_ok s in
      let xs = Common.split "/" d in
      match xs with
      | x::y::xs -> x, x ^ "/" ^ y, d
      | [x] -> x, x ^ "/_TOP_", d
      | [] -> "_TOP_", "_TOP_", d
    in

    (* check that no ambiguity? *)
    pr (spf "nodedef> name, dir1 varchar(200), dir2 varchar(200), dir varchar(200)");
    nodes |> List.iter (fun n ->
      let (dir1, dir2, dir) = dirs_of_n n in
      (* don't add extra space for attributes, otherwise no match when
       * use ==
      *)
      pr (spf "%s,%s,%s,%s" (node_name_of_n n) dir1 dir2 dir);
    );
    pr (spf "edgedef> node1,node2,directed");
    nodes |> List.iter (fun n1 ->
      let succ = G.succ n1 g in
      succ |> List.iter (fun n2 ->
        pr (spf "%s,%s,true" (node_name_of_n n1) (node_name_of_n n2));
      )
    );
    ()
  )

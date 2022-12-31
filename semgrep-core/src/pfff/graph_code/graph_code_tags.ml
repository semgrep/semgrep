(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
module G = Graph_code
module E = Entity_code

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Generating a set of TAGS from a graph_code.
 *
 * alternatives:
 *  - could start from the prolog facts (themselves generated from graph_code)
 *    to factorize some code, but for TAGS we are only interested
 *    in position and the only predicate that matters, at/3, does not
 *    actually contain enough information such as the byte offset in the file,
 *    so let's copy paste for now.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* quite similar to graph_code_prolog *)
let defs_of_graph_code ?(verbose = false) g =
  ignore verbose;

  (* we use the multi-values-to-same-key property of Hashtbl.add and
   * Hashtbl.find_all
   *)
  let hfile_to_tags = Hashtbl.create 101 in

  let hmemo_file_array = Hashtbl.create 101 in

  g
  |> G.iter_nodes (fun n ->
         let str, kind = n in
         try
           let nodeinfo = G.nodeinfo n g in
           let file = nodeinfo.G.pos.Parse_info.file in
           let line = nodeinfo.G.pos.Parse_info.line in
           let text =
             try
               let array =
                 Common.memoized hmemo_file_array file (fun () ->
                     Common2.cat_array file)
               in
               (* not sure why, but can't put an empty string for
                * tag_definition_text; Emacs is then getting really confused
                *)
               array.(line)
             with
             | Invalid_argument _out_of_bound ->
                 logger#error "PB accessing line %d of %s" line file;
                 ""
             | Sys_error _no_such_file ->
                 pr2_once (spf "PB accessing file %s" file);
                 ""
           in
           let tag =
             {
               Tags_file.tagname = str;
               line_number = nodeinfo.G.pos.Parse_info.line;
               byte_offset = nodeinfo.G.pos.Parse_info.charpos;
               kind;
               tag_definition_text = text;
             }
           in
           Hashtbl.add hfile_to_tags file tag;
           (* when add a tag for List.foo, also add foo.List *)
           let reversed_tagname =
             Common.split "\\." str |> List.rev |> Common.join "."
           in
           Hashtbl.add hfile_to_tags file
             { tag with Tags_file.tagname = reversed_tagname }
         with
         | Not_found -> (
             match kind with
             | E.Package
             | E.File
             | E.Dir
             | E.TopStmts
             | E.Module ->
                 ()
             | _ ->
                 if List.mem G.not_found (G.parents n g) then ()
                 else pr2 (spf "PB, nodeinfo not found for %s" str)));
  Common2.hkeys hfile_to_tags
  |> List.map (fun file ->
         ( file,
           Hashtbl.find_all hfile_to_tags file
           |> List.map (fun tag -> (tag.Tags_file.byte_offset, tag))
           |> Common.sort_by_key_lowfirst |> List.map snd ))

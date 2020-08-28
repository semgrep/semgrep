(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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

module Ast = Cst_php
module G = Graph_code
module Flag = Flag_analyze_php
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * In different structures such as the callgraph we need to refer to a
 * certain PHP entity, like a specific function. Usually those specific
 * pieces of code we want to refer to are functions or classes. So we need
 * a way to represent such place. We can use the location, which is the
 * fullid described below but it takes quite some space (a string, 2
 * ints). Enter the 'id' type.
 * 
 * update: we now also have "nested" ids which are ids about nested
 * entities like methods (of a class) or nested functions. Could have
 * defined an id type and subid type but this seems tedious. Better to
 * have only 'id' and a 'id_kind' and associated tables that stores subasts
 * and other info (see children_id and enclosing_id tables in
 * database_php.ml)
 * 
 * todo? One pb is that we lose some precision. Sometimes we would like
 * to enforce a certain kind of id. So all this scheme is good ? At the
 * same time a function can be called from many stuff, another func, a
 * toplevel stmt, a method, an expression in a variable declaration, etc
 * so maybe simpler to have indeed a single 'id'.
 * 
 * update: this file may be a little bit obsolete now that we use prolog
 * and the abstract interpreter for most of our needs. The id
 * in prolog is then simply the function name atom or the pair of atoms
 * with the class and method names (and atoms are represented efficiently
 * I hope in the Prolog engine).
 * 
 * todo? hmmm but maybe we could have a entity_finder2: 
 *  type entity_finder2 = { functions: ...; classes: ... }
 * which would be close to what julien is using in his code when he
 * needs some global analysis.
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* primary key (prefered) method *) 
type id = Id of int

(* also primary key, more readable, but less optimial spacewise *)
type filepos = {
  file: filename;
  line: int;
  column: int;
}
 (* with tarzan *)
type fullid = filepos

(* id_kind is now just an alias. Indeed it was copy-paste/redundant with
 * Datbase_code.ml.
 * 
 * todo? there are still some duplications:
 *  - highlight_code.ml
 *  - Ast_php.entity (but without the actual ast content)
 *  - view_widgets_source.ml ?
 *  - typing_c environment type
 *  - type_annotater_c.ml namedef type
 * 
 * note: StaticMethod could be considered a Function, because in PHP they
 * mostly use static methods because PHP (5.2) didn't have namespace and
 * so they abuse classes for modules. But right now we use Method.
 * 
 * Xhp declarations are considered Fields.
 *)
type id_kind = Entity_code.entity_kind

(* See comment in the .mli for more information about entity_finder.
 * 
 * update: I now return a list of entities instead of a single entity
 * and raising a Not_found or Multi_found. The rational is that
 * the caller knows better what to do when there are multiple
 * matching definitions for an entity. For instance for 
 * better error message having just Multi_found is not enough.
 * The caller needs for instance to show two witnesses.
 *)

type entity_finder = (id_kind * string) -> Cst_php.entity list

type method_identifier = (string * string)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_dbg s =
  if !Flag.verbose_entity_finder
  then pr2 s

(*****************************************************************************)
(* String_of *)
(*****************************************************************************)
let str_of_id (Id x) = 
  spf "id:%d" x

let str_of_fullid x = 
  spf "%s:%d:%d" x.file x.line x.column

let fullid_regexp = 
 "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)$"

let fullid_of_string s = 
  if s =~ fullid_regexp
  then
    let (filename, line, column) = matched3 s in
    { file = filename;
      line = s_to_i line;
      column = s_to_i column;
    }
  else failwith ("not a full_id:" ^ s)

let string_of_id_kind x = 
  Entity_code.string_of_entity_kind x

(*****************************************************************************)
(* Method identifier helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

let vof_filename = OCaml.vof_string

let vof_filepos { file = v_file; line = v_line; column = v_column } =
  let bnds = [] in
  let arg = OCaml.vof_int v_column in
  let bnd = ("column", arg) in
  let bnds = bnd :: bnds in
  let arg = OCaml.vof_int v_line in
  let bnd = ("line", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_filename v_file in
  let bnd = ("file", arg) in let bnds = bnd :: bnds in OCaml.VDict bnds

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (filepos_of_parse_info: Parse_info.token_location -> filepos) = fun pi -> 
  { 
    file = pi.Parse_info.file;
    line = pi.Parse_info.line;
    column = pi.Parse_info.column;
  }

(*****************************************************************************)
(* Entity finder based on graphcode *)
(*****************************************************************************)

(* 
 * TODO: use more cache? parsing cache too?
 *)
let hcache_entities = Hashtbl.create 101

(* Note that we will parse 2 times a file, once to analyze it, below,
 * in Check_all_php.check_file, and once because of the entity_finder
 * here. The -profile may actually return numbers for Parse_php.parse
 * more than 2 times the one for the checkers because we may
 * need to load via the entity_finder files outside the directory
 * passed as a parameter to scheck (e.g. flib/). We could cache
 * the parsed AST but it can stress the GC too much.
 *)
let _hdone = Hashtbl.create 101
let ast_php_entity_in_file ~check_dupes (s, kind) g file =
  (* pr2_dbg (Common.dump (s, kind, file)); *)

  (* sanity check, this should never happened *)
(*
  if Hashtbl.mem hdone file
  then failwith (spf "already processed file %s" file);
  Hashtbl.add hdone file true;
*)

  let ast2 = Parse_php.parse_program file in
  let entities =
    ast2 |> Common.map_filter (function
    | Ast.StmtList _ -> None
    | Ast.FuncDef def ->
        Some ((Ast.str_of_ident def.Ast.f_name, E.Function), Ast.FunctionE def)
    | Ast.TypeDef def ->
        Some ((Ast.str_of_ident def.Ast.t_name, E.Type), Ast.TypedefE def)
    | Ast.ClassDef def -> 
        Some ((Ast.str_of_ident def.Ast.c_name, E.Class), Ast.ClassE def)
    | Ast.ConstantDef def ->
        Some ((Ast.str_of_ident def.Ast.cst_name, E.Constant),Ast.ConstantE def)
    | Ast.NamespaceDef (tok, _, _) 
    | Ast.NamespaceBracketDef (tok, _, _) 
    | Ast.NamespaceUse (tok, _, _) ->
        raise (Ast.TodoNamespace tok)

    | Ast.NotParsedCorrectly _ | Ast.FinalDef _ -> None
    )
  in
  (* cache all those entities. todo: use marshalled form? for GC? *)
  entities |> List.iter (fun ((s, kind), def) -> 
    (* This is expensive so we do it only if one really wants
     * the check_dupe checks (like in our unit tests). Most of 
     * the time you don't need this check as building the codegraph
     * will already display such errors
     *)
    let asts =
     if check_dupes then
      if G.has_node (s, kind) g then
        let parent = G.parent (s, kind) g in
        match parent with
        | x when x =*= G.not_found ->
          pr2_dbg (spf "entity not found: %s" (G.string_of_node (s, kind)));
          []
        | x when x =*= G.dupe ->
          pr2_dbg (spf "entity dupe found: %s" (G.string_of_node (s, kind)));
          (* create fake multi entities *)
          [def; def]
        | _ ->
          [def]
      else begin
        pr2_dbg (spf "entity not in graph: %s" (G.string_of_node (s, kind)));
        []
      end
     else [def]
    in
    Hashtbl.replace hcache_entities (s, kind) asts

  );
  Hashtbl.find hcache_entities (s, kind)


let entity_finder_of_graph_code ?(check_dupes=false) g root =
  (fun (kind, s) ->
    (* pr2_gen (kind, s); *)
    Common.memoized hcache_entities (s, kind) (fun () ->
      match kind with
      | E.Function | E.Class | E.Constant ->
        (* todo: transpose in regular class as graph_code only stores that?*)
        if G.has_node (s, kind) g then
          let parent = G.parent (s, kind) g in
          (match parent with
          (* file_of_node() below would not work on undefined functions as
           * they are added on the fly in graph_code_php
           *)
          | x when x =*= G.not_found ->
            pr2_dbg (spf "entity not found: %s" (G.string_of_node (s, kind)));
            []
          | _ ->
            let file = G.file_of_node (s, kind) g in
            let path = Filename.concat root file in
            ast_php_entity_in_file ~check_dupes (s, kind) g path
          )
        else begin
          pr2_dbg (spf "entity not in graph: %s" (G.string_of_node (s, kind)));
          []
        end
      | _ ->
        pr2 (spf "entity not handled: %s" (G.string_of_node (s, kind)));
        []
    )
  )
  


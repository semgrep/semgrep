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

open Prolog_code
module G = Graph_code
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Generating prolog DB facts from a graph_code.
 *
 * For more information look at h_program-lang/prolog_code.pl
 * and its many predicates.
*)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* see hook_use_edge_for_prolog below, a bit complicated those
 * hooks of hooks ... it's just that the model of graph_code
 * does not allow to do certain things that we want in codequery
 * such as tracking the flow of values.
*)
let hook_facts = ref []

let build g =

  let res = ref [] in
  let add x = Common.push x res in

  add (Misc "% -*- prolog -*-");
  add (Misc ":- discontiguous kind/2, at/3");
  add (Misc ":- discontiguous type/2");
  add (Misc ":- discontiguous docall/2, use/3");
  add (Misc ":- discontiguous special/4");
  add (Misc ":- discontiguous extends/2, implements/2");

  (* defs *)
  g |> G.iter_nodes (fun n ->
    let (str, kind) = n in
    (match kind with
     | E.Function | E.Global | E.Constant | E.Type | E.Macro
     | E.Package | E.Module
     (* todo? field and constructor have a X.Y.type.fld so should
      * we generate for the entity a ([X;Y;type], fld) or ([X;Y], "type.fld")
     *)
     | E.Field | E.Constructor
     | E.Method | E.ClassConstant
     | E.Exception
       -> add (Kind (entity_of_str str, kind))
     (* todo: interface | trait *)
     | E.Class ->
         add (Kind (entity_of_str str, kind))

     (* less: hmm if only have a proto, e.g. in lib.h should add it no? *)
     | E.Prototype | E.GlobalExtern
       -> ()

     | E.File | E.Dir
       -> ()

     | (E.TopStmts|E.Other _|E.MultiDirs) ->
         pr2_gen n;
         raise Todo
    );

    (try
       (* todo: should avoid adding it twice when have both proto and func
        * defined
       *)
       let nodeinfo = G.nodeinfo n g in
       add (At (entity_of_str str,
                nodeinfo.G.pos.Parse_info.file,
                nodeinfo.G.pos.Parse_info.line));
       let t =
         match nodeinfo.G.typ with
         | None -> "unknown"
         | Some s -> s
       in
       add (Type (entity_of_str str, t))
     with Not_found -> ()
    );
  );

  (* uses *)

  (* we iter on the Use edges of the graph_code (see graph_code.ml), which
   * contains the inheritance tree, call graph, and data graph information.
  *)
  g |> G.iter_use_edges (fun n1 n2 ->
    match n1, n2 with
    (* less: at some point have to differentiate Extends and Implements
     * depending on the _kind, but for now let's simplify and convert
     * everything to a regular class inheritance
    *)
    | ((s1, E.Class), (s2, E.Class)) ->
        add (Extends (s1, s2))

    | ((s1, (E.Function|E.Method)), (s2, (E.Function|E.Prototype|E.Method)))->
        add (Call (entity_of_str s1, entity_of_str s2))

    | ((s1, (E.Function | E.Method | E.Global | E.Constant)),
       (s2, (E.Field | E.ClassConstant | E.Global) )) ->
        let info = G.edgeinfo_opt (n1, n2) G.Use g in
        (match info with
         | None ->
             add (UseData (entity_of_str s1, entity_of_str s2, None))
         | Some { G.read = false; G.write = false } ->
             failwith (spf "use access with neither read or write: %s -> %s"
                         s1 s2)
         | Some info ->
             if info.G.read
             then add (UseData (entity_of_str s1, entity_of_str s2, Some false));
             if info.G.write
             then add (UseData (entity_of_str s1, entity_of_str s2, Some true));
        )

    | _ -> ()
  );

  (* special uses *)
  !hook_facts |> List.iter add;

  List.rev !res

(* old: was for PHP.
    | E.Method _ ->
      let nodeinfo = G.nodeinfo n g in
      let props = nodeinfo.G.props in
      props +> List.iter (function
      | E.Privacy priv -> add (P.Privacy (P.entity_of_str str, priv));
      | _ -> ()
      );
    | E.Field ->
      let (xs, x) = P.entity_of_str str in
      if x =~ "\\$\\(.*\\)"
      then add (P.Kind ((xs, Common.matched1 x), kind))
      else failwith ("field does not contain $: " ^ x)

   (* uses *)
    | ((s1, E.Class _kind1), (s2, E.Class E.RegularClass)) ->
      add (P.Extends (s1, s2))
    | ((s1, E.Class _kind1), (s2, E.Class E.Trait)) ->
      add (P.Mixins (s1, s2))
    | ((s1, E.Class _kind1), (s2, E.Class E.Interface)) ->
      add (P.Implements (s1, s2))
*)



(* This is for codequery. In C the flow of values goes either
 * via assignments of via calls (where the argument is assigned in
 * the parameter)
*)
type context =
  | NoCtx
  | CallCtx of Graph_code.node
  | AssignCtx of Graph_code.node

let hook_use_edge_for_prolog ctx in_assign (src, dst) g _loc =
  let kind = snd dst in

  (match kind with
   | E.Global | E.Field ->
       let oldinfoopt = G.edgeinfo_opt (src, dst) G.Use g in
       let info =
         match oldinfoopt with
         | Some info -> info
         | None -> { G.read = false; G.write = false }
       in
       let newinfo =
         if in_assign
         then { info with G.write = true }
         else { info with G.read = true }
       in
       G.add_edgeinfo (src, dst) G.Use newinfo g
   | _ -> ()
  );
  let esrc = entity_of_str (fst src) in
  let edst = entity_of_str (fst dst) in
  (match ctx, kind with
   | NoCtx, _ -> ()
   | AssignCtx fld_node, E.Function ->
       let efld = entity_of_str (fst fld_node) in
       hook_facts |> Common.push (Special (esrc, efld, edst, "field"))
   | CallCtx func_node, E.Function ->
       let efunc = entity_of_str (fst func_node) in
       hook_facts |> Common.push (Special (esrc, efunc, edst, "function"))
   | _ -> ()
  )

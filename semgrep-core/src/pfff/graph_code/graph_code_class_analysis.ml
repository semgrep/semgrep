(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* parent -> children *)
type class_hierarchy = Graph_code.node Graphe.graph

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let class_method_of_string str =
  if str =~ "^\\(.*\\)\\.\\([^\\.]+\\)$"
  then Common.matched2 str
  else failwith (spf "not a method: %s" str)
(* alt: Graph_code.shortname_of_node *)

let string_of_class_method (c, m) =
  c ^ "." ^ m

(*****************************************************************************)
(* One off analysis *)
(*****************************************************************************)

(* Finding protected fields that could be private.
 * This was done for Oravec to possibliy optimize code as protected field
 * have an overhead in HPHP.
 *
 * It can be difficult to trace the use of a field in languages like
 * PHP because one can do $o->fld and you don't know the type of $o
 * and so its class. But for the protected_to_private analysis,
 * it means the field is protected and so it can be used only
 * via a $this->xxx expression, which is easy to statically
 * analyze.
*)
let protected_to_private_candidates g =
  g |> G.iter_nodes (fun node ->
    match node with
    | (_s, E.Field) ->

        let privacy =
          try G.privacy_of_node node g
          with Not_found ->
            pr2 (spf "No nodeinfo for %s" (G.string_of_node node));
            E.Private
        in
        (match privacy with
         | E.Private ->
             let users = G.pred node G.Use g in
             if null users
             then pr2 (spf "DEAD private field: %s" (G.string_of_node node))
         | E.Protected ->
             let parents = G.parents node g in
             if List.length parents > 1
             then begin pr2_gen node; pr2_gen parents end;
             let class_ = G.parent node g in
             if class_ =*= G.dupe
             then pr2 (spf "Redefined field: %s" (G.string_of_node node))
             else begin
               let classname = fst class_ in

               let users = G.pred node G.Use g in
               if null users
               then pr2 (spf "DEAD protected field: %s" (G.string_of_node node))
               else
               if users |> List.for_all (fun (s, _kind) ->
                 s =~ (spf "^%s\\." classname)
               )
               then pr2 (spf "Protected to private candidate: %s"
                           (G.string_of_node node))
               else ()
             end
         | _ -> ()
        )
    | _ -> ()
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* if B extends A then will have a node from A to B (children relation) *)
let class_hierarchy g =
  let dag = Graphe.create () in

  g |> G.iter_nodes (fun n1 ->
    (match snd n1 with
     | E.Class ->
         dag |> Graphe.add_vertex_if_not_present n1;

         (* explore if its extends/implements/uses another class/interf/trait *)
         let succ = g |> G.succ n1 G.Use in
         succ |> List.iter (fun n2 ->
           (match snd n2 with
            | E.Class ->
                dag |> Graphe.add_vertex_if_not_present n2;
                (* from parent to children *)
                dag |> Graphe.add_edge n2 n1
            | _ ->
                failwith (spf "class_hierarchy: impossible edge %s --> %s"
                            (G.string_of_node n1) (G.string_of_node n2))
           )
         )
     | _ -> ()
    )
  );
  dag


let toplevel_methods g dag =
  (* in a clean language we could just start from the Object class *)
  let start = Graphe.entry_nodes dag in

  let env = Set.empty in
  let htoplevels = Hashtbl.create 101 in

  let rec aux env n =

    let methods_here =
      G.children n g |> Common.map_filter (fun n2 ->
        match snd n2 with
        | E.Method ->
            let (_, method_str) = class_method_of_string (fst n2) in
            let privacy = G.privacy_of_node n2 g in
            Some (method_str, privacy, n2)
        | _ -> None
      )
    in
    methods_here |> List.iter (fun (s, priv, n2) ->
      if Set.mem s env
      then ()
      else
        (* We care only about toplevel public methods. Private or protected
         * methods can be used only via $this-> and so should be resolved.
         * Only calls like $o->foo() are unresolved and those methods
         * must be public methods.
        *)
      if priv = E.Public
      then Hashtbl.add htoplevels s n2
      else ()
    );
    let children_classes =
      Graphe.succ n dag in
    (* todo? what if public overriding a private? *)
    let env =
      methods_here |> List.fold_left (fun acc (s, _p, _) ->Set.add s acc) env in

    children_classes |> List.iter (aux env)
  in
  start |> List.iter (aux env);
  htoplevels


(* the inverse of lookup, go down in the children instead of up in the parent *)
let dispatched_methods2 g dag node =
  let (str, kind) = node in
  assert (kind =*= E.Method);

  let (c, m) = class_method_of_string str in

  let res = ref [] in
  let rec aux (current_class, class_kind) =
    let node = (string_of_class_method (current_class, m), kind) in
    (* todo? need get public and protected there too *)
    (if G.has_node node g
     then Common.push node res
    );
    let children = Graphe.succ (current_class, class_kind) dag in
    children |> List.iter aux
  in
  let node = (c, E.Class) in
  (if G.has_node node g
   then Graphe.succ node dag |> List.iter aux
   else failwith (spf "could not find class %s" c)
  );

  !res

let dispatched_methods a b c =
  Common.profile_code "GCCA.dispatched_methods" (fun () ->
    dispatched_methods2 a b c)

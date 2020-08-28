(* Julien Verlaguet
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

module SSet = Set.Make(String)
module SMap = Map.Make(String)

module Ast = Ast_php
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Makes the graph of dependencies in a program
 *
 * pad: this should use a visitor ... it's mostly boilerplate code.
 * I actually can't easily see where is done the specific stuff.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module Deps = struct
  open Ast_php

  let rec program acc stl =
    stmtl acc stl

  and stmtl acc stl =
    List.fold_left stmt acc stl

  and stmt acc = function
    | NamespaceDef (_, qu,_) | NamespaceUse (_, qu, _) ->
      raise (Cst_php.TodoNamespace (tok_of_name qu))
    (* adding names of entities *)
    | ClassDef c -> SSet.add (unwrap c.c_name) acc
    | FuncDef f -> SSet.add (unwrap f.f_name) acc
    | ConstantDef c -> SSet.add (unwrap c.cst_name) acc
    | TypeDef t -> SSet.add (unwrap t.t_name) acc

    (* boilerplate to recurse *)
    | Expr (e, _) | Throw (_, e) -> expr acc e
    | Block (_, stl,_) -> stmtl acc stl
    | If (_, e, st1, st2) -> stmtl (expr acc e) [st1; st2]
    | Do (_, stl, e)| While (_, e, stl) -> stmtl (expr acc e) stl
    | For (_, el1, el2, el3, stl) ->
        let acc = exprl acc el1 in
        let acc = exprl acc el2 in
        let acc = exprl acc el3 in
        stmtl acc stl
    | Switch (_, e, cl) -> casel (expr acc e) cl
    | Foreach (_, e1, _, e2, stl) ->
        let acc = expr acc e1 in
        let acc = expr acc e2 in
        stmtl acc stl
    | Return (_, e) | Break (_, e) | Continue (_, e) -> expr_opt acc e
    | Try (_, stl, cl, fl) ->
        let acc = stmtl acc stl in
        let acc = catchl acc cl in
        let acc = finallyl acc fl in
        acc
    | StaticVars (_, svl) -> List.fold_left static_var acc svl
    | Global (_, el) -> exprl acc el


  and static_var acc (_, e) = expr_opt acc e

  and casel acc l = List.fold_left case acc l
  and case acc = function
    | Case (_, e, stl) -> stmtl (expr acc e) stl
    | Default (_, stl) -> stmtl acc stl

  and catchl acc l = List.fold_left catch acc l
  and catch acc (_, _, _, stl) = stmtl acc stl

  and finallyl acc l = List.fold_left finally acc l
  and finally acc (_, stl) = stmtl acc stl

  and exprl acc l = List.fold_left expr acc l
  and expr_opt acc = function None -> acc | Some e -> expr acc e

  and expr acc = function
    | Id [(s, _)] -> SSet.add s acc
    | Id name ->       raise (Cst_php.TodoNamespace (tok_of_name name))
    | Var _ -> acc

    | Int _ | Double _ | String _ -> acc
    | Guil (_, el, _) -> encapsl acc el
    | IdSpecial _ -> acc
    | Array_get (e1, e2) -> expr (expr_opt acc e2) e1
    | Obj_get (e1, _, e2)
    | Binop (e1, _, e2)
    | Class_get (e1, _, e2)
    | InstanceOf (_, e1, e2)
    | Arrow(e1, _, e2)
    | AssignOp(e1, _, e2)
    | Assign (e1, _, e2) -> expr (expr acc e1) e2
    | Infix (_, e)
    | Postfix (_, e)
    | Cast (_, e)
    | Ref (_, e) | Unpack e
    | Unop (_, e) -> expr acc e
    | Call (e, (_, el, _)) -> exprl (expr acc e) el
    | Xhp x ->
        let acc = xml acc x in
        let name = Ast.unwrap x.xml_tag in
        SSet.add name acc
    | ConsArray ((_, avl, _)) -> array_valuel acc avl
    | Collection ([(n,_)], (_, mel, _)) ->
      let acc = SSet.add n acc in
      array_valuel acc mel
    | Collection (name, _mel) ->
      raise (Cst_php.TodoNamespace (tok_of_name name))
    | List (_, el, _) -> exprl acc el
    | New (_, e, el) -> exprl (expr acc e) el
    | CondExpr (e1, e2, e3) ->
        expr (expr (expr acc e1) e2) e3
    | Lambda fd -> func_def acc fd

  and array_valuel acc l = List.fold_left array_value acc l
  and vector_eltl acc l = List.fold_left vector_elt acc l
  and map_eltl acc l = List.fold_left map_elt acc l
  and array_value acc e = expr acc e

  and vector_elt acc e = expr acc e
  and map_elt acc (e1, e2) =
    let accp = expr acc e1 in
    expr accp e2
  and encapsl acc l = List.fold_left encaps acc l
  and encaps acc x = expr acc x

  and xhpl acc l = List.fold_left xhp acc l
  and xhp acc = function
    | XhpText _ -> acc
    | XhpExpr e -> expr acc e
    | XhpXml x -> xml acc x

  and xml acc x =
    let acc = xml_attrs acc x.xml_attrs in
    let acc = xhpl acc x.xml_body in
    acc

  and xml_attrs acc l = List.fold_left xml_attr acc l
  and xml_attr acc (_, x) = xhp_attr acc x

  and xhp_attr acc e = expr acc e

  and func_def acc f =
    let acc = parameterl acc f.f_params in
    let acc = hint_type acc f.f_return_type in
    let acc = stmtl acc f.f_body in
    acc

  and parameterl acc l = List.fold_left parameter acc l
  and parameter acc p =
    let acc = hint_type acc p.p_type in
    let acc = expr_opt acc p.p_default in
    acc

  and hint_type acc = function None -> acc | Some x -> hint_type_ acc x
  and hint_type_ acc = function
    (* not sure a type hints counts as a dependency *)
    | Hint [(s, _)] -> SSet.add s acc
    | Hint name -> raise (Cst_php.TodoNamespace (tok_of_name name))

    | HintArray _ -> acc
    | HintQuestion (_, t) -> hint_type_ acc t
    | HintTuple (_, t, _) -> List.fold_left (fun accp x -> SSet.union accp (hint_type_ accp x)) acc t
    | HintCallback (args, ret) ->
        let acc_u_ret = match ret with
                         | Some x -> hint_type_ acc x
                         | None -> acc
        in
        List.fold_left (fun accp x -> SSet.union accp (hint_type_ accp x)) acc_u_ret args
    | HintShape (_, (_, xs, _)) ->
      List.fold_left (fun accp (_s,x) ->
        SSet.union accp (hint_type_ accp x)) acc xs
    | HintTypeConst (x1, _, x2) ->
      List.fold_left (fun accp x ->
        SSet.union accp (hint_type_ accp x)) acc [x1; x2]
    | HintVariadic (_, x) -> hint_type acc x


  and class_def acc c =
    (* todo? implements? traits? *)
    let acc = extends acc c.c_extends in
    let acc = cconstants acc c.c_constants in
    let acc = cvariables acc c.c_variables in
    let acc = method_defl acc c.c_methods in
    acc

  and extends acc l =
    match l with
    | None -> acc
    | Some ht ->
      let (x, _) = name_of_class_name ht in
      SSet.add x acc

  and cconstants acc l = List.fold_left cconstant acc l
  and cconstant acc cst = expr_opt acc cst.cst_body
  and cvariables acc cvl = List.fold_left class_var acc cvl
  and method_defl acc l = List.fold_left method_def acc l

  and class_var acc cv =
    let acc = hint_type acc cv.cv_type in
    let acc = expr_opt acc cv.cv_value in
    acc

  and method_def acc m =
    let acc = parameterl acc m.f_params in
    let acc = hint_type acc m.f_return_type in
    let acc = stmtl acc m.f_body in
    acc
end

module Graph = struct

  type t = SSet.t SMap.t ref

  let empty () = ref SMap.empty

  let func_def acc fd =
    let x = Ast.unwrap fd.Ast.f_name in
    acc := SMap.add x (Deps.func_def SSet.empty fd) !acc

  let class_def acc cd =
    let x = Ast.unwrap cd.Ast.c_name in
    acc := SMap.add x (Deps.class_def SSet.empty cd) !acc


  let get_deps g x =
    let ss = try SMap.find x g with Not_found -> SSet.empty in
    SSet.fold (fun x acc -> x :: acc) ss []

  let invert g =
    let acc = ref SMap.empty in
    let add x y =
      let deps = try SMap.find x !acc with Not_found -> SSet.empty in
      let deps = SSet.add y deps in
      acc := SMap.add x deps !acc
    in
    SMap.iter (
    fun x deps ->
      SSet.iter (
      fun y -> add y x
     ) deps
   ) g;
    !acc

end

(*****************************************************************************)
(* Main entry point  *)
(*****************************************************************************)
module TopoSort = struct

  let verbose = ref false
  let o = output_string stdout

  type t = White | Gray | Black

  let get_color colors x =
    try SMap.find x colors with Not_found -> White

  let gray colors x = SMap.add x Gray colors
  let black colors x = SMap.add x Black colors

  let rec sort (g: SSet.t SMap.t ref) =
    let g = !g in
    let colors = SMap.empty in
    let _colors, acc =
      SMap.fold (
      fun x _ colors_acc ->
        edge g [] colors_acc x
     ) g (colors, [])
    in
    List.rev acc

  and edge graph stack (colors, acc) x =
    if not (SMap.mem x graph) then colors, acc else
    let c = get_color colors x in
    match c with
    | White ->
        let colors = gray colors x in
        let k = edge graph (x :: stack) in
        let l = Graph.get_deps graph x in
        let colors, acc = List.fold_left k (colors, acc) l in
        let colors = black colors x in
        let acc = x :: acc in
        colors, acc
    | Gray ->
        if !verbose
        then begin
          o "Cycle found:";
          print_cycle x stack;
          o "\n";
        end;
        let colors = black colors x in
        let acc = x :: acc in
        colors, acc
    | Black -> colors, acc

  and print_cycle x l =
    match l with
    | [] -> ()
    | y :: _ when x = y -> o x
    | y :: rl -> o y; o " "; print_cycle x rl
end

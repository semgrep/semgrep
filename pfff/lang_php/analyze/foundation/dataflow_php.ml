(*s: dataflow_php.ml *)
(*s: Facebook copyright *)
(* Iain Proctor, Yoann Padioleau, Jiao Li
 *
 * Copyright (C) 2009-2010 Facebook
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
open Cst_php

module F = Controlflow_php
module Ast = Cst_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * The goal of a dataflow analysis is to store information about each
 * variable at each program point that is each node in a CFG,
 * e.g. whether a variable is "live" at a program point.
 * As you may want different kind of information, the types below
 * are polymorphic. But each take as a key a variable name (dname, for
 * dollar name, the type of variables in Ast_php).
 *
 * less: could use a functor, so would not have all those 'a.
 * todo? do we need other kind of information than variable environment ?
 * Dataflow analysis talks only about variables ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The comparison function uses only the name string so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)

module VarMap = Map.Make(String)
module VarSet = Set.Make(String)

type nodei = Ograph_extended.nodei
module NodeiSet = Set.Make(Int)

(* The final dataflow result; a map from each program point to a map containing
 * information from each variables.
 *
 * opti: this used to be a 'NodeiMap.t' instead of an 'array' but 'nodei'
 * are always int and array gives a 6x speedup according to iain
 * so let's use array.
 *)
type 'a mapping = ('a inout) array

  and 'a inout = {
    in_env: 'a VarMap.t;
    out_env: 'a VarMap.t;
  }
   and 'a env = 'a VarMap.t

let empty_env () = VarMap.empty
let empty_inout () = {in_env = empty_env (); out_env = empty_env ()}

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

let eq_env eq e1 e2 =
  VarMap.equal eq e1 e2
let eq_inout eq io1 io2 =
  let eqe = eq_env eq in
  (eqe io1.in_env io2.in_env) && (eqe io1.out_env io2.out_env)

exception Break

let eq_mapping eq m1 m2 =
  try (
    for x = 0 to Array.length m1 do
      if not (eq_inout eq m1.(x) m2.(x)) then raise Break
    done;
    true
  )
  with Break -> false


(*****************************************************************************)
(* Env manipulation *)
(*****************************************************************************)

let (minus_env : NodeiSet.t env ->  NodeiSet.t env -> NodeiSet.t env) =
fun e1 e2 -> VarMap.fold (fun v s e' ->
  try
    let df = NodeiSet.diff (VarMap.find v e') s in
    if NodeiSet.is_empty df then VarMap.remove v e' else VarMap.add v df e'
  with Not_found -> e')
 e2 e1

let (add_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env) =
fun e1 e2 -> VarMap.fold (fun v s e' ->
    let s2 = try NodeiSet.union s (VarMap.find v e') with Not_found -> s in
      VarMap.add v s2 e')
  e2 e1

(*****************************************************************************)
(* Debugging support *)
(*****************************************************************************)

let csv_append s v =
  if String.length s == 0 then v else s ^ "," ^ v

let ns_to_str ns =
  "{" ^
  NodeiSet.fold (fun n s -> csv_append s (string_of_int n)) ns "" ^
  "}"

let (env_to_str: ('a -> string) -> 'a env -> string) =
fun val2str env ->
  VarMap.fold
    (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ")
    env ""

let (inout_to_str: ('a -> string) -> 'a inout -> string) =
fun val2str inout ->
  spf "IN= %15s  OUT = %15s"
    (env_to_str val2str inout.in_env)
    (env_to_str val2str inout.out_env)

let array_fold_left_idx f = let idx = ref 0 in
  Array.fold_left (fun v e -> let r = f v !idx e in incr idx; r)

let mapping_to_str (fl : F.flow) val2str mapping =
  array_fold_left_idx (fun s ni v -> s ^
    (spf "%2d <- %7s: %15s %s\n"
      ni
      ((fl#predecessors ni)#fold (fun s (ni, _) ->
      csv_append s (string_of_int ni)) "")
     (F.short_string_of_node (fl#nodes#find ni))
     (inout_to_str val2str v)
  )) "" mapping

(*****************************************************************************)
(* Main generic entry point *)
(*****************************************************************************)

(* The transition/transfer function. It is usually made from the
 * gens and kills.
 *
 * todo? having only a transfer function is enough ? do we need to pass
 * extra information to it ? maybe only the mapping is not enough. For
 * instance if in the code there is $x = &$g, a reference, then
 * we may want later to have access to this information. Maybe we
 * should pass an extra env argument ? Or maybe can encode this
 * sharing of reference in the 'a, so that when one update the
 * value associated to a var, its reference variable get also
 * the update.
 *)
type 'a transfn = 'a mapping -> nodei -> 'a inout

let rec fixpoint_worker eq mp trans flow succs work =
  if NodeiSet.is_empty work then mp else
  let ni = NodeiSet.choose work in
  let work' = NodeiSet.remove ni work in
  let old = mp.(ni) in
  let nu = trans mp ni in
  let work'' = if eq_inout eq old nu
               then work'
               else (mp.(ni) <- nu; NodeiSet.union work' (succs flow ni)) in
  fixpoint_worker eq mp trans flow succs work''


let forward_succs (f : F.flow) n = (f#successors n)#fold
  (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty
let backward_succs (f : F.flow) n = (f#predecessors n)#fold
  (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty

let (fixpoint:
    eq:('a -> 'a -> bool) ->
    init:'a mapping ->
    trans:'a transfn ->
    flow:F.flow ->
    forward: bool ->
   'a mapping) =
 fun ~eq ~init ~trans ~flow ~forward ->

  fixpoint_worker eq init trans flow
   (if forward then forward_succs else backward_succs)
   (flow#nodes#fold (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty)

(*****************************************************************************)
(* Node Visitors *)
(*****************************************************************************)

(* Node id -> var name -> is on the lhs? -> acc -> acc' *)
type 'a fold_fn = nodei -> string -> bool -> 'a -> 'a

type 'a fold_env = {fold_fn: string -> bool -> 'a -> 'a; fold_vars: VarSet.t}

(* expr_fold: 'a fold_env -> bool(left hand side) -> Ast.expr
 -> 'a -> 'a *)
let rec expr_fold fold_env lhs expr acc =
  (* Used for known left hand value, e.g. reference *)
  let reclvl = expr_fold fold_env true in
  let recl = expr_fold fold_env lhs in
  let recr = expr_fold fold_env false in
  let handle_lhs e acc =
    match e with
    | IdVar _
    | ArrayGet _
    | ObjGet _
    | ClassGet _
      -> reclvl e acc
    | _ -> raise Todo
  in
  match expr with
  | Id _ -> acc
  | IdVar (DName(name, _), _) ->
    fold_env.fold_fn name lhs acc
  | Assign(e, _, e1) ->
    handle_lhs e (recr e1 acc)
  | AssignOp(e, _, e1) ->
    handle_lhs e (recr e (recr e1 acc))
  | AssignList(_, list_assign, _, expr) ->
    let rec reclist _acc' list_assign' =
      let list_assign' = Ast.uncomma(Ast.unparen list_assign') in
      List.fold_left
        (fun acc' -> function
        | ListVar(e) -> handle_lhs e acc'
        | ListList(_, list_assign'') ->
          reclist acc' list_assign''
        | ListEmpty -> acc') acc list_assign'
    in
    reclist (recr expr acc) list_assign
  | This _ -> acc
  (* Special cases for function *)
  | Call (Id(XName([QI(Name(("sscanf", _)))])), args) ->
    let args = Ast.uncomma(Ast.unparen args) |> List.map
      (function
      | Arg e -> e
      | ArgRef _ | ArgUnpack _ -> raise Todo) in
    (match args with
    | x::y::vars ->
      let acc = recr x (recr y acc) in
      List.fold_left (fun acc _arg ->
        handle_lhs expr acc) acc vars
    (* Abuse Todo exception *)
    | _ -> raise Todo)
  (* Todo: false positive because passsing by reference *)
  | Call (e, args) ->
    let args = Ast.uncomma(Ast.unparen args) in
    recr e
    (List.fold_left
       (fun acc' -> function
       | Arg e1 | ArgUnpack (_, e1) -> recr e1 acc'
       | ArgRef (_, e1) -> handle_lhs e1 acc'
       )
       acc args)
  | ObjGet(e, _,e1) ->
    recl e (recr e1 acc)
  | ClassGet(e, _, _e1) -> recr e acc
  (* TODO: copy on write *)
  | ArrayGet(e, e1) ->
    recl e
      (match Ast.unbracket e1 with
      | Some e1 -> recr e1 acc
      | None -> acc)
  | HashGet(e, e1) ->
    recl e (recr (Ast.unbracket e1) acc)
  | BraceIdent(_, e, _) ->
    recl e acc
  (* Not sure *)
  | Deref(_, _e)->
    raise Todo
  | Sc(C _) -> acc
  | Sc(Guil(_, encaps_list, _))
  | Sc(HereDoc(_, encaps_list, _))
      ->
    encaps_list |> List.fold_left
      (fun acc' encaps ->
        match encaps with
        | EncapsString _ -> acc'
        | EncapsVar expr
        | EncapsCurly (_, expr, _)
        | EncapsDollarCurly (_, expr, _)
        | EncapsExpr (_, expr, _)
          -> recl expr acc') acc

  | Binary(e, _, e1) ->
    recl e (recl e1 acc)
  | Unary(_, e) -> recl e acc
  | Postfix(e, _)
  | Infix (_, e)
    -> handle_lhs e (recr e acc)
  | CondExpr(e, _, e1opt, _, e2) ->
    recl e (recl e2
              (match e1opt with
              | Some e1 -> recl e1 acc
              | None -> acc))
  (* TODO *)
  | ArrayLong _
  | ArrayShort _
  | Collection _
    -> raise Todo
  | New(_, e, None) -> recr e acc
  | New(_, e, Some(args)) ->
   let args = Ast.uncomma(Ast.unparen args) in
    (List.fold_left
       (fun acc' -> function
       | Arg e1 | ArgUnpack (_, e1) -> recr e1 acc'
       | ArgRef (_, e1) -> handle_lhs e1 acc')
       (recr e acc) args)
  | Clone _ -> raise Todo
  | AssignRef _ -> raise Todo
  | AssignNew(_e, _, _, _, _, None) -> raise Todo
  | AssignNew(_e, _, _, _, _, Some(_args)) ->
      raise Todo
  | Cast(_, e) -> recr e acc
  | CastUnset _ -> raise Todo
  | InstanceOf(e, _, e1) -> recr e (recr e1 acc)
  | Eval(_, _e) -> raise Todo
  | Lambda _ -> raise Todo
  | ShortLambda _ -> raise Todo
  | Exit(_, eopt) ->
    (match eopt with
    | Some(_, Some e, _) -> recr e acc
    | _ -> acc)
  | At _ -> raise Todo
  | Print(_, e) -> recr e acc
  | BackQuote _ -> raise Todo
  | Include _
  | Require _
  | IncludeOnce _
  | RequireOnce _
      -> raise Todo
  | Empty _
  | Isset _
      -> raise Todo
  (* TODO *)
  | XhpHtml _ -> raise Todo
  (* TODO: handle control flow about generator*)
  | Yield _
  | YieldBreak _
  | Await _
      -> raise Todo
  | SgrepExprDots _ -> raise Todo
  | ParenExpr(e) ->
    recl (Ast.unparen e) acc

let (node_fold: 'a fold_env -> F.node_kind -> 'a -> 'a) =
fun fold_env node acc -> match node with
  (* Nothing is needed if the node has no expr information*)
      | F.Enter
      | F.Exit
      | F.TrueNode
      | F.FalseNode
      | F.DoHeader
      | F.ForHeader
      | F.SwitchEnd
      | F.Case
      | F.Default
      | F.Return (None)
      | F.Break
      | F.Continue
      | F.TryHeader
      | F.CatchStart
      | F.Catch
      | F.TryEnd
      | F.Join
      | F.SimpleStmt (F.TodoSimpleStmt)
          -> acc
      (* expr *)
      | F.IfHeader expr
      | F.WhileHeader expr
      | F.DoWhileTail expr
      | F.SwitchHeader expr
      | F.Throw expr
      | F.SimpleStmt (F.ExprStmt (expr, _(*use_status*)))
      | F.Return (Some expr)
          -> expr_fold fold_env false expr acc
      | F.Parameter (DName(name, _)) ->
        fold_env.fold_fn name true acc
      | F.ForeachHeader (*var_list *) ->
        raise Todo
(*
        List.fold_left (fun acc' var ->
          match var with
          | (Some _, _) -> raise Todo
          | (None, IdVar(DName(s, _), _)) ->
            fold_env.fold_fn s true acc'
          | _ -> acc') acc var_list
*)

let (flow_fold: 'a fold_fn -> VarSet.t -> 'a -> F.flow -> 'a) =
 fun fold_fn vars acc flow -> flow#nodes#fold
  (fun acc' (ni, nd) -> node_fold {fold_fn = fold_fn ni; fold_vars = vars}
   nd.F.n acc') acc

let flow_fold_lv fold_fn vars acc flow = flow_fold
  (fun ndi dnm lhs acc' -> if lhs then fold_fn ndi dnm acc' else acc') vars acc
    flow
let flow_fold_rv fold_fn vars acc flow = flow_fold
  (fun ndi dnm lhs acc' -> if not lhs then fold_fn ndi dnm acc' else acc')
    vars acc flow

let new_node_array (f: F.flow) v =
  let arr = Array.make f#nb_nodes v in
  (* sanity checking *)
  let len = Array.length arr in
  f#nodes#tolist |> List.iter (fun (ni, _nod) ->
    if ni >= len
    then failwith "the CFG nodei is bigger than the number of nodes"
  );
  arr

(*****************************************************************************)
(* Example of analysis: reaching definitions *)
(*****************************************************************************)

(* For a reaching definitions analysis the dataflow result is
 * a map from each program point (as usual), to a map from each
 * variable (as usual), to a set of nodes that define this variable
 * that are visible at this program point.
 *
 * For instance on:
 *
 * 1: $a = 1;
 * 2: if(...) {
 * 3:   $a = 2;
 * 4: } else {
 * 5:   $a = 3;
 * 6: }
 * 7: echo $a;
 *
 * then at the program point (node index) 7, then for $a the nodei set
 * is {3, 5}, but not '1'.
 *)
type reaching_mapping = NodeiSet.t mapping

let add_def d nm ni =
  let v =
    try NodeiSet.add ni (VarMap.find nm d)
    with Not_found -> NodeiSet.singleton ni
  in
  VarMap.add nm v d

let (vars: F.flow -> VarSet.t) =
  flow_fold (fun _ va _ vs -> VarSet.add va vs) VarSet.empty VarSet.empty

let (reaching_defs: VarSet.t -> F.flow -> NodeiSet.t env) = fun vars ->
  flow_fold_lv (fun ni va acc -> add_def acc va ni) vars VarMap.empty

let up_map k f mp = mp.(k) <- f mp.(k); mp

let up_set_map k v mp = up_map k (VarSet.add v) mp

(* gen/kill *)
let (reaching_gens: VarSet.t -> F.flow -> VarSet.t array) = fun vars fl ->
  flow_fold_lv (fun ni v gs -> up_set_map ni v gs) vars
    (new_node_array fl VarSet.empty) fl

let (reaching_kills:
   NodeiSet.t env -> VarSet.t -> F.flow -> (NodeiSet.t env) array) =
 fun ds vars fl -> flow_fold_lv (fun ni va ks ->
    let d = NodeiSet.remove ni (VarMap.find va ds) in
    up_map ni (fun v -> VarMap.add va d v) ks) vars
      (new_node_array fl (empty_env())) fl

(*
 * This algorithm is taken from Modern Compiler Implementation in ML, Appel,
 * 1998, pp. 382.
 *
 * The transfer is setting in'[n] = U_{p in pred[n]} out[p] and
 * out'[n] = gen[n]U(in[n] - kill[n]) where gen[n] is {n} if there in a
 * definition at n and {} otherwise, and kill[n] is the set of all definitions
 * of the variable being defined at n except the one at n.
 *)
let (reaching_transfer:
   gen:VarSet.t array ->
   kill:(NodeiSet.t env) array ->
   flow:F.flow ->
   NodeiSet.t transfn) =
 fun ~gen ~kill ~flow ->
  fun mp ni ->

  let new_in = (flow#predecessors ni)#fold (fun s (nip, _) ->
      add_env s mp.(nip).out_env) VarMap.empty in
  let in_k = minus_env new_in kill.(ni) in
  let new_out = VarSet.fold (fun v e -> VarMap.add v
      (try NodeiSet.add ni (VarMap.find v e)
       with Not_found -> NodeiSet.singleton ni) e)
    gen.(ni) in_k in
  {in_env = new_in; out_env = new_out}

let (reaching_fixpoint: F.flow -> reaching_mapping) = fun flow ->
  let vars = vars flow in
  let defs = reaching_defs vars flow in
  let gen = reaching_gens vars flow in
  let kill = reaching_kills defs vars flow in

  fixpoint
    ~eq:NodeiSet.equal
    ~init:(new_node_array flow (empty_inout ()))
    ~trans:(reaching_transfer ~gen ~kill ~flow)
    ~forward:true
    ~flow

(*****************************************************************************)
(* Dataflow pretty printing *)
(*****************************************************************************)

let (display_dflow: F.flow -> 'a mapping -> ('a -> string) -> unit) =
 fun flow mapping string_of_val ->
   pr (mapping_to_str flow string_of_val mapping)


let display_reaching_dflow flow mp =
  let string_of_ni ni =
    let node = flow#nodes#assoc ni in
    match node.F.i with
    | None -> "Unknown location"
    | Some(info) ->
      let info = Parse_info.token_location_of_info info in
      spf "%s:%d:%d: "
        info.Parse_info.file info.Parse_info.line info.Parse_info.column
  in
  let arr = Array.make flow#nb_nodes true in
  (* Set the flag to false if the node has defined anything *)
  let flow_lv_fn = fun ni _name arr ->
    let node = flow#nodes#assoc ni in
    (match node.F.n with
    | F.SimpleStmt (F.ExprStmt _) ->
      arr.(ni) <- false
    | _ -> ());
    arr
  in
  let arr = flow_fold_lv flow_lv_fn VarSet.empty arr flow in

  (* Now flag the def if it is ever used on rhs *)
  (* Node id -> var name -> acc -> acc' *)
  let flow_rv_fn = fun ni name arr ->
    let in_env = mp.(ni).in_env in
    (try
       let ns = VarMap.find name in_env in
       NodeiSet.fold (fun n _ -> arr.(n) <- true) ns ()
     with
     | Not_found -> pr (spf "%s: Undefined variable" (string_of_ni ni)));
    arr
  in
  let arr = flow_fold_rv flow_rv_fn VarSet.empty arr flow in
  let i = ref 0 in
  List.iter (fun x ->
    if (not x)
    then pr (spf "%s: Dead Assignment" (string_of_ni !i));
    incr i
  ) (Array.to_list arr)

(*e: dataflow_php.ml *)

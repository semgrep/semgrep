(* A checker for detecting micro clones. Micro clones are small snippets of
 * repeated expressions or statements. They can often be removed in a
 * semantics-preserving way, (unless expressions have side-effects, for
 * example).
 *
 * Currently it supports detection of duplicate expressions inside conditional
 * statements. For example:
 *
 * {[ if ($a || $b || $c || $d || $e && $e && ($f && ($g || $z) && $h && $i))
 * {} ]}
 *
 * will generate the error
 *
 * {[ test.php:2:31: CHECK: Boolean operator && contains duplicate expression
 * $e. ]}
 *
 * There's lots of room for extensions. E.g., extend detection to duplicate
 * statements, conditional bodies, etc. Additionally, the simplify function
 * produces a simplified expression that can be extended to use Spatch for
 * patching.
 *)

module Ast = Cst_php
module Error = Error_php

(* A small module for boolean expressions. It maintains the invariant that a)
 * Boolean expression consists of a binary operator and two or more Atoms b)
 * Boolean expressions are flattened by construction. E.g., And(a, And(b, c))
 * is simplified to And(a, b, c).
 *
 * The purpose is to have a common construction and representation for boolean
 * expressions which can be simplified by rewrite rules (e.g., deduplication).
 * This is very much in the same vein as Z3's bool_rewriter.h.
 *)
module Boolean : sig
  (* In this module, support 6 kinds of boolean expressions in PHP *)
  type op = And  (* && *)
          | Or   (* || *)
          | LOr  (* or *)
          | LAnd (* and *)
          | AOr  (* | *)
          | AAnd (* & *)

  type 'a t = private
    | Atom of 'a
    | List of op * 'a t list

  val make : op -> 'a t -> 'a t -> 'a t

  module Lang : sig
    val (&&) : 'a t -> 'a t -> 'a t
    val (||) : 'a t -> 'a t -> 'a t
    val (+&&) : 'a t -> 'a t -> 'a t
    val (+||) : 'a t -> 'a t -> 'a t
    val (+&) : 'a t -> 'a t -> 'a t
    val (+|) : 'a t -> 'a t -> 'a t
    val v : 'a -> 'a t
  end
end = struct
  type op = And | Or | LOr | LAnd | AOr | AAnd
  type 'a t =
    | Atom of 'a
    | List of op * 'a t list

  let make op x y =
    match x,y with
    | List (op1,xs), List (op2,ys) when op1 = op && op2 = op ->
      List (op, xs@ys)
    | _,List (op2,ys) when op2 = op ->
      List (op,x::ys)
    | List (op1,xs),_ when op1 = op ->
      List (op,xs@[y])
    | x,y ->
      List (op,[x;y])

  module Lang = struct
    let (&&) op1 op2 = make And op1 op2
    let (||) op1 op2 = make Or op1 op2
    let (+&&) op1 op2 = make LAnd op1 op2
    let (+||) op1 op2 = make LOr op1 op2
    let (+&) op1 op2 = make AAnd op1 op2
    let (+|) op1 op2 = make AOr op1 op2
    let v x = Atom x
  end
end

open Boolean

type php_exp = Cst_php.expr * Cst_php.tok option

let op_to_string = function
  | And -> "And"
  | Or -> "Or"
  | LOr -> "LOr"
  | LAnd -> "LAnd"
  | AOr -> "AOr"
  | AAnd -> "AAnd"

let to_string exp =
  let open Printf in
  let (!) = Unparse_php.string_of_expr in
  let rec exp_to_string =
    function
    | Atom (Cst_php.IdVar (Cst_php.DName(v,_),_),_) -> sprintf "%s" v
    | Atom (x,_) -> !x
    | List (op,l) -> sprintf "%s(%s)" (op_to_string op) (list_to_string l)
  and
    list_to_string (l : php_exp Boolean.t list) : string =
    List.fold_left (fun (c,acc) x ->
        match c with
        | 0 -> (c+1),(exp_to_string x)
        | _ -> (c+1),(acc^", "^(exp_to_string x))) (0,"") l |> snd
  in
  exp_to_string exp

(* create a Boolean.t expression from an AST expression *)
let bool_exp_of_php_exp exp : php_exp Boolean.t =
  let open Ast in
  let rec aux exp parent_tok : php_exp Boolean.t =
    match exp with
    | Binary (lhs,(Logical OrBool,op_tok),rhs) ->
      Boolean.Lang.(aux lhs (Some op_tok) || aux rhs (Some op_tok))
    | Binary (lhs,(Logical AndBool,op_tok),rhs) ->
      Boolean.Lang.(aux lhs (Some op_tok) && aux rhs (Some op_tok))
    | Binary (lhs,(Arith And,op_tok),rhs) ->
      Boolean.Lang.(aux lhs (Some op_tok) +&& aux rhs (Some op_tok))
    | Binary (lhs,(Arith Or,op_tok),rhs) ->
      Boolean.Lang.(aux lhs (Some op_tok) +|| aux rhs (Some op_tok))
    | Binary (lhs,(Logical AndLog,op_tok),rhs) ->
      Boolean.Lang.(aux lhs (Some op_tok) +& aux rhs (Some op_tok))
    | Binary (lhs,(Logical OrLog,op_tok),rhs) ->
      Boolean.Lang.(aux lhs (Some op_tok) +| aux rhs (Some op_tok))
    | ParenExpr (_,exp,_) -> aux exp parent_tok
    | x -> Boolean.Lang.v (x, parent_tok)
  in aux exp None

(* Use the first Atom in the duplicate expression. *)
let emit_error expr =
  let res  =
    let rec aux = function
      | Atom x -> Some x
      | List (_,hd::_) -> aux hd
      | _ -> None
    in aux expr in
  match res with
  | Some (expr,Some op_tok) ->
    let err_tok = Parse_info.str_of_info op_tok in
    let err_expr = Unparse_php.string_of_expr expr in
    Error.fatal op_tok (Error.MicroCloneCondExp (err_tok,err_expr))
  | _ -> ()

(* Compare expressions syntactically *)
let compare exp1 exp2 =
  String.compare (to_string exp1) (to_string exp2)

let dedup l =
  List.fold_left (fun acc x ->
      if List.exists (fun y -> compare x y = 0) acc
      then (emit_error x; acc) else x::acc) [] l |> List.rev

(* Helper function to reconstruct Boolean.t *)
let boolean_of_list op (l : php_exp Boolean.t list) : php_exp Boolean.t =
  let rec aux l =
    match l with
    | [Atom x] -> Lang.v x
    | x::y::[] -> make op x y
    | [List (op,hd::tl)] -> make op hd (aux tl)
    | hd::tl -> make op hd (aux tl)
    | [] -> failwith "Error: Cannot construct a Boolean from an empty list."
  in aux l

(* Rewrite rule *)
let rule_dedup (exp : php_exp Boolean.t) : php_exp Boolean.t =
  match exp with
  | List (op,l) -> dedup l |> boolean_of_list op
  | Atom x -> Lang.v x

(* Bottom-up exression rewriter *)
let bur_map f (exp : php_exp Boolean.t) : php_exp Boolean.t =
  let rec aux exp =
    match exp with
    | List (op,l) ->
      let e = List.map aux l |> boolean_of_list op in
      f e
    | x -> f x in
  aux exp

(* verbose option can be used as a starting point for spatch *)
let simplify ?(verbose=false) exp =
  let open Printf in
  let exp' = bool_exp_of_php_exp exp in
  let exp'' = bur_map rule_dedup exp' in
  if verbose then
    let s' = to_string exp' in
    let s'' = to_string exp'' in
    if s' <> s'' then
      (printf "\n[+] Exp:\n\n\t%s\n" s';
       printf "\ncan be simplified:\n\n\t%s\n" s'')
    else
      printf "\n[+] Exp:\n\n\t%s\n" s'

let check ast =
  let open Ast in
  let visitor = Visitor_php.mk_visitor {
      Visitor_php.default_visitor with

      Visitor_php.kstmt = (fun (k,_) s ->
          match s with
          | If (_,(_,cond_exp,_),_,elseifs,_) ->
            let exps =
              cond_exp::(List.map (fun ((_,(_,exp,_),_)) -> exp) elseifs) in
            List.iter simplify exps;
            k s
          | _ -> k s)
    } in
  visitor (Ast.Program ast)

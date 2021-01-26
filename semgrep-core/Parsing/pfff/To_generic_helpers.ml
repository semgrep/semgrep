open AST_generic_
module G = AST_generic

(* This module is ugly, but it was written to allow to move AST_generic.ml
 * out of pfff/ and inside semgrep/. However there are many
 * language-specific ASTs that we using AST_generic.ml to factorize
 * the definitions of operators. To break the dependency we had
 * to duplicate that part of AST_generic in pfff/h_program-lang/AST_generic_.ml
 * (note that underscore at the end) and we need those boilerplate functions
 * below to convert them back to AST_generic.
 *
 * alt: use polymorphic variants (e.g., `Plus)
*)

let (conv_op: AST_generic_.operator -> AST_generic.operator) = function
  | Plus -> G.Plus
  | Minus -> G.Minus
  | Mult  -> G.Mult
  | Div  -> G.Div
  | Mod -> G.Mod
  | Pow -> G.Pow
  | FloorDiv  -> G.FloorDiv
  | MatMult -> G.MatMult
  | LSL -> G.LSL
  | LSR  -> G.LSR
  | ASR -> G.ASR
  | BitOr  -> G.BitOr
  | BitXor  -> G.BitXor
  | BitAnd  -> G.BitAnd
  | BitNot  -> G.BitNot
  | BitClear -> G.BitClear
  | And  -> G.And
  | Or  -> G.Or
  | Xor  -> G.Xor
  | Not -> G.Not
  | Eq -> G.Eq
  | NotEq -> G.NotEq
  | PhysEq -> G.PhysEq
  | NotPhysEq -> G.NotPhysEq
  | Lt  -> G.Lt
  | LtE  -> G.LtE
  | Gt  -> G.Gt
  | GtE -> G.GtE
  | Cmp -> G.Cmp
  | Concat -> G.Concat
  | Append -> G.Append
  | RegexpMatch -> G.RegexpMatch
  | NotMatch -> G.NotMatch
  | Range -> G.Range
  | RangeInclusive -> G.RangeInclusive
  | NotNullPostfix -> G.NotNullPostfix
  | Length -> G.Length
  | Elvis -> G.Elvis
  | Nullish -> G.Nullish
  | In -> G.In
  | NotIn -> G.NotIn
  | Is -> G.Is
  | NotIs -> G.NotIs


let (conv_incr: AST_generic_.incr_decr -> AST_generic.incr_decr) = function
  | Incr  -> G.Incr
  | Decr -> G.Decr

let (conv_prepost: AST_generic_.prefix_postfix -> AST_generic.prefix_postfix) =  function
  | Prefix  -> G.Prefix
  | Postfix -> G.Postfix


let (conv_incdec:
       (AST_generic_.incr_decr * AST_generic_.prefix_postfix) ->
     (AST_generic.incr_decr * AST_generic.prefix_postfix)) = fun (x, y) ->
  conv_incr x, conv_prepost y

let (conv_class_kind: AST_generic_.class_kind * Parse_info.t -> AST_generic.class_kind * Parse_info.t) = fun (c, t) ->
  (match c with
   | Class -> G.Class
   | Interface -> G.Interface
   | Trait -> G.Trait
  ), t

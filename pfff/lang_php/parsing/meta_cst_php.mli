open Cst_php

(*s: meta_ast_php.mli *)
val vof_program: program -> OCaml.v
val vof_toplevel: toplevel -> OCaml.v
val vof_expr: expr -> OCaml.v
val vof_any: any -> OCaml.v

(* used by pil.ml or ast_php_simple.ml *)

val vof_info: tok -> OCaml.v
val vof_tok: tok -> OCaml.v

val vof_dname: dname -> OCaml.v
val vof_name: name -> OCaml.v

val vof_binaryOp: binaryOp -> OCaml.v
val vof_unaryOp: unaryOp -> OCaml.v
val vof_assignOp: assignOp -> OCaml.v
val vof_castOp: castOp -> OCaml.v
val vof_fixOp: fixOp -> OCaml.v
val vof_ptype: ptype -> OCaml.v

val vof_hint_type: hint_type -> OCaml.v
val vof_constant: constant -> OCaml.v
val vof_class_name_reference: class_name_reference -> OCaml.v
val vof_modifier: modifier -> OCaml.v

(*e: meta_ast_php.mli *)

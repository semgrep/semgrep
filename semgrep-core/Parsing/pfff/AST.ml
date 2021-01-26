(*s: pfff/lang_GENERIC/parsing/AST.ml *)
(*
 * See pfff/lang_GENERIC_base/AST_generic.ml.
 *
 * I put AST_generic.ml under another dir because it is used by the other
 * lang_xxx/analyze/xxx_to_generic.ml files. We need to break the mutual
 * dependency between the lang_xxx/ and lang_GENERIC/ by moving some
 * basic files of lang_GENERIC/parsing/ under lang_GENERIC_base/.
 *
 * This file provides just convenient aliases so one can write in signature
 * files AST.program instead of the longer AST_generic.program.
 *)
(*s: type [[AST.xxx]] aliases *)
type program = AST_generic.program
type name = AST_generic.name
type ident = AST_generic.ident
type id_info = AST_generic.id_info
type expr = AST_generic.expr
type stmt = AST_generic.stmt
type any = AST_generic.any
(*e: type [[AST.xxx]] aliases *)
(*e: pfff/lang_GENERIC/parsing/AST.ml *)

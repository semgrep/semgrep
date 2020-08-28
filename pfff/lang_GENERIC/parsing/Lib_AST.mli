(*s: pfff/lang_GENERIC/parsing/Lib_AST.mli *)

(* Note that ii_of_any relies on Visitor_AST which itself
 * uses OCaml.v_ref_do_not_visit, so no need to worry about
 * tokens inside id_type or id_info.
 *)
(*s: signature [[Lib_AST.ii_of_any]] *)
val ii_of_any: AST.any -> Parse_info.t list
(*e: signature [[Lib_AST.ii_of_any]] *)

(*s: signature [[Lib_AST.abstract_position_info_any]] *)
val abstract_position_info_any: AST.any -> AST.any
(*e: signature [[Lib_AST.abstract_position_info_any]] *)
(*e: pfff/lang_GENERIC/parsing/Lib_AST.mli *)

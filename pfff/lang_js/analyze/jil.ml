

(* Javascript Intermediate Language (JIL).
 *
 * TODO This module serves multiple purposes:
 *  - ease certain analysis by reducing the complexity of Javascript
 *    to an even smaller language than ast_js.ml (already an improvement
 *    over cst_js.ml)
 *  - allow sgrep patterns to match even more code by reducing the multiple
 *    ways to do the same thing (e.g., o = {fld1: eval} vs o.fld1 = eval)
 *    to only one canonical representation.
 * 
 * TODO: Essence of Javascript paper.
 *
 * related:
 *  - CIL
 *  - Ruby Intermediate language
 *  - LLVM IR
 *  - ...
 *)


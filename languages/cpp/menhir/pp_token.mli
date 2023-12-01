(* Expanding or extracting macros, at the token level *)

(* the either is to differentialte macro-variables from macro-functions *)
type define_body = (unit, string list) Either.t * Parser_cpp.token list

(* TODO
   (* corresponds to what is in the yacfe configuration file (e.g. standard.h) *)
   type define_def = string * define_param * define_body
   and define_param =
   | NoParam
   | Params of string list
   and define_body =
   | DefineBody of Parser_c.token list
   | DefineHint of parsinghack_hint

      (* strongly corresponds to the TMacroXxx in the grammar and lexer and the
       * MacroXxx in the ast.
       *)
      and parsinghack_hint =
        | HintIterator
        | HintDeclarator
        | HintMacroString
        | HintMacroStatement
        | HintAttribute
        | HintMacroIdentBuilder
*)

(* extracting define_def, e.g. from a standard.h; assume have called
 * fix_tokens_define before to have the TDefEol *)
val extract_macros : Parser_cpp.token list -> (string, define_body) Assoc.t

(* TODO
   val string_of_define_def: define_def -> string
*)

(* used internally *)
(* This function work by side effect and may generate new tokens
 * in the new_tokens_before field of the token_extended in the
 * paren_grouped list. So don't forget to recall
 * Token_views_c.rebuild_tokens_extented after this call, as well
 * as probably insert_virtual_positions as new tokens
 * are generated.
 *
 * note: it does not do some fixpoint, so the generated code may also
 * contain some macros names.
 *)

val apply_macro_defs :
  (*
  msg_apply_known_macro:(string -> unit) ->
  msg_apply_known_macro_hint:(string -> unit) ->
  ?evaluate_concatop:bool ->
  ?inplace_when_single:bool ->
*)
  (string, define_body (* define_def *)) Hashtbl.t ->
  Token_views_cpp.paren_grouped list ->
  unit

(*s: semgrep/matching/matching_generic.mli *)

(*s: type [[Matching_generic.tin]] *)
(* incoming environment *)
type tin = Metavars_generic.metavars_binding
(* list of possible outcoming matching environments *)
(*e: type [[Matching_generic.tin]] *)
(*s: type [[Matching_generic.tout]] *)
(* list of possible outcoming matching environments *)
type tout = tin list
(*e: type [[Matching_generic.tout]] *)

(*s: type [[Matching_generic.matcher]] *)
(* currently 'a and 'b are usually the same type as we use the
 * same language for the host language and pattern language 
 *)
type ('a, 'b) matcher = 'a -> 'b -> tin -> tout
(*e: type [[Matching_generic.matcher]] *)

(*s: signature [[Matching_generic.TODOOPERATOR]] *)
(* monadic combinators *)
val ( >>= ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout
(*e: signature [[Matching_generic.TODOOPERATOR]] *)
(*s: signature [[Matching_generic.TODOOPERATOR (semgrep/matching/matching_generic.mli)]] *)
val ( >||> ) : (tin -> tout) -> (tin -> tout) -> tin -> tout
(*e: signature [[Matching_generic.TODOOPERATOR (semgrep/matching/matching_generic.mli)]] *)
(*s: signature [[Matching_generic.TODOOPERATOR (semgrep/matching/matching_generic.mli)2]] *)
val ( >!> ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout
(*e: signature [[Matching_generic.TODOOPERATOR (semgrep/matching/matching_generic.mli)2]] *)

(*s: signature [[Matching_generic.return]] *)
val return : unit -> tin -> tout
(*e: signature [[Matching_generic.return]] *)
(*s: signature [[Matching_generic.fail]] *)
val fail : unit -> tin -> tout
(*e: signature [[Matching_generic.fail]] *)

(*s: signature [[Matching_generic.empty_environment]] *)
val empty_environment : unit -> 'a list
(*e: signature [[Matching_generic.empty_environment]] *)

(*s: signature [[Matching_generic.envf]] *)
val envf : (string Ast_generic.wrap, Ast_generic.any) matcher
(*e: signature [[Matching_generic.envf]] *)

(*s: signature [[Matching_generic.check_and_add_metavar_binding]] *)
val check_and_add_metavar_binding :
  string * Ast_generic.any ->
  tin -> tin option
(*e: signature [[Matching_generic.check_and_add_metavar_binding]] *)

(*s: signature [[Matching_generic.has_ellipsis_stmts]] *)
(* helpers *)
val has_ellipsis_stmts : Ast_generic.stmt list -> bool
(*e: signature [[Matching_generic.has_ellipsis_stmts]] *)
(*s: signature [[Matching_generic.all_elem_and_rest_of_list]] *)
val all_elem_and_rest_of_list : 'a list -> ('a * 'a list) list
(*e: signature [[Matching_generic.all_elem_and_rest_of_list]] *)
(*s: signature [[Matching_generic.is_regexp_string]] *)
val is_regexp_string: string -> bool
(*e: signature [[Matching_generic.is_regexp_string]] *)
(*s: signature [[Matching_generic.regexp_of_regexp_string]] *)
val regexp_of_regexp_string: string -> Str.regexp
(*e: signature [[Matching_generic.regexp_of_regexp_string]] *)

(* internal:
val str_of_any : Ast_generic.any -> string
val equal_ast_binded_code : Ast_generic.any -> Ast_generic.any -> bool
*)

(*s: signature [[Matching_generic.m_option]] *)
(* generic matchers *)

val m_option : ('a, 'b) matcher -> ('a option, 'b option) matcher
(*e: signature [[Matching_generic.m_option]] *)
(*s: signature [[Matching_generic.m_option_ellipsis_ok]] *)
val m_option_ellipsis_ok :
  (Ast_generic.xml_attr_value -> 'a -> tin -> tout) ->
  Ast_generic.xml_attr_value option -> 'a option -> tin -> tout
(*e: signature [[Matching_generic.m_option_ellipsis_ok]] *)
(*s: signature [[Matching_generic.m_option_none_can_match_some]] *)
val m_option_none_can_match_some :
  ('a -> 'b -> tin -> tout) -> 'a option -> 'b option -> tin -> tout
(*e: signature [[Matching_generic.m_option_none_can_match_some]] *)

(*s: signature [[Matching_generic.m_ref]] *)
val m_ref : ('a, 'b) matcher -> ('a ref, 'b ref) matcher
(*e: signature [[Matching_generic.m_ref]] *)

(*s: signature [[Matching_generic.m_list]] *)
val m_list : ('a -> 'b -> tin -> tout) -> 'a list -> 'b list -> tin -> tout
(*e: signature [[Matching_generic.m_list]] *)
(*s: signature [[Matching_generic.m_list_prefix]] *)
val m_list_prefix :
  ('a -> 'b -> tin -> tout) -> 'a list -> 'b list -> tin -> tout
(*e: signature [[Matching_generic.m_list_prefix]] *)
(*s: signature [[Matching_generic.m_list_with_dots]] *)
val m_list_with_dots :
  ('a -> 'b -> tin -> tout) ->
  ('a -> bool) -> bool -> 'a list -> 'b list -> tin -> tout
(*e: signature [[Matching_generic.m_list_with_dots]] *)

(*s: signature [[Matching_generic.m_bool]] *)
val m_bool : 'a -> 'a -> tin -> tout
(*e: signature [[Matching_generic.m_bool]] *)
(*s: signature [[Matching_generic.m_int]] *)
val m_int : int -> int -> tin -> tout
(*e: signature [[Matching_generic.m_int]] *)
(*s: signature [[Matching_generic.m_string]] *)
val m_string : string -> string -> tin -> tout
(*e: signature [[Matching_generic.m_string]] *)
(*s: signature [[Matching_generic.string_is_prefix]] *)
val string_is_prefix : string -> string -> bool
(*e: signature [[Matching_generic.string_is_prefix]] *)
(*s: signature [[Matching_generic.m_string_prefix]] *)
val m_string_prefix : string -> string -> tin -> tout
(*e: signature [[Matching_generic.m_string_prefix]] *)

(*s: signature [[Matching_generic.m_info]] *)
val m_info : 'a -> 'b -> tin -> tout
(*e: signature [[Matching_generic.m_info]] *)
(*s: signature [[Matching_generic.m_tok]] *)
val m_tok : 'a -> 'b -> tin -> tout
(*e: signature [[Matching_generic.m_tok]] *)
(*s: signature [[Matching_generic.m_wrap]] *)
val m_wrap : ('a -> 'b -> tin -> tout) -> 'a * 'c -> 'b * 'd -> tin -> tout
(*e: signature [[Matching_generic.m_wrap]] *)
(*s: signature [[Matching_generic.m_bracket]] *)
val m_bracket :
  ('a -> 'b -> tin -> tout) -> 'c * 'a * 'd -> 'e * 'b * 'f -> tin -> tout
(*e: signature [[Matching_generic.m_bracket]] *)

(*s: signature [[Matching_generic.m_other_xxx]] *)
val m_other_xxx : 'a -> 'a -> tin -> tout
(*e: signature [[Matching_generic.m_other_xxx]] *)
(*e: semgrep/matching/matching_generic.mli *)

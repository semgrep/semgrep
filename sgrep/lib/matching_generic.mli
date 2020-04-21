
(* incoming environment *)
type tin = Metavars_generic.metavars_binding
(* list of possible outcoming matching environments *)
type tout = tin list

(* currently 'a and 'b are usually the same type as we use the
 * same language for the host language and pattern language 
 *)
type ('a, 'b) matcher = 'a -> 'b -> tin -> tout

(* monadic combinators *)
val ( >>= ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout
val ( >||> ) : (tin -> tout) -> (tin -> tout) -> tin -> tout
val ( >!> ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout

val return : unit -> tin -> tout
val fail : unit -> tin -> tout

val empty_environment : unit -> 'a list

val envf : (string Ast_generic.wrap, Ast_generic.any) matcher

val check_and_add_metavar_binding :
  string * Ast_generic.any ->
  tin -> tin option

(* helpers *)
val has_ellipsis_stmts : Ast_generic.stmt list -> bool
val all_elem_and_rest_of_list : 'a list -> ('a * 'a list) list

(* internal:
val str_of_any : Ast_generic.any -> string
val equal_ast_binded_code : Ast_generic.any -> Ast_generic.any -> bool
*)

(* generic matchers *)

val m_option : ('a, 'b) matcher -> ('a option, 'b option) matcher
val m_option_ellipsis_ok :
  (Ast_generic.xml_attr_value -> 'a -> tin -> tout) ->
  Ast_generic.xml_attr_value option -> 'a option -> tin -> tout
val m_option_none_can_match_some :
  ('a -> 'b -> tin -> tout) -> 'a option -> 'b option -> tin -> tout

val m_ref : ('a, 'b) matcher -> ('a ref, 'b ref) matcher

val m_list : ('a -> 'b -> tin -> tout) -> 'a list -> 'b list -> tin -> tout
val m_list_prefix :
  ('a -> 'b -> tin -> tout) -> 'a list -> 'b list -> tin -> tout
val m_list_with_dots :
  ('a -> 'b -> tin -> tout) ->
  ('a -> bool) -> bool -> 'a list -> 'b list -> tin -> tout

val m_bool : 'a -> 'a -> tin -> tout
val m_int : int -> int -> tin -> tout
val m_string : string -> string -> tin -> tout
val string_is_prefix : string -> string -> bool
val m_string_prefix : string -> string -> tin -> tout

val m_info : 'a -> 'b -> tin -> tout
val m_tok : 'a -> 'b -> tin -> tout
val m_wrap : ('a -> 'b -> tin -> tout) -> 'a * 'c -> 'b * 'd -> tin -> tout
val m_bracket :
  ('a -> 'b -> tin -> tout) -> 'c * 'a * 'd -> 'e * 'b * 'f -> tin -> tout

val m_other_xxx : 'a -> 'a -> tin -> tout

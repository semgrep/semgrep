(*s: semgrep/matching/Matching_generic.mli *)

(*s: type [[Matching_generic.tin]] *)
(* tin is for 'type in' and tout for 'type out' *)
(* incoming environment *)
type tin = {
  mv : Metavariable_capture.t;
  stmts_match_span : Stmts_match_span.t;
  cache : tout Caching.Cache.t option;
  (* TODO: this does not have to be in tout; maybe split tin in 2? *)
  config : Config_semgrep.t;
}

(*e: type [[Matching_generic.tin]] *)
(*s: type [[Matching_generic.tout]] *)
(* list of possible outcoming matching environments *)
and tout = tin list

(*e: type [[Matching_generic.tout]] *)

(*s: type [[Matching_generic.matcher]] *)
(* A matcher is something taking an element A and an element B
 * (for this module A will be the AST of the pattern and B
 * the AST of the program we want to match over), then some environment
 * information tin, and it will return something (tout) that will
 * represent a match between element A and B.
 *)
(* currently A and B are usually the same type as we use the
 * same language for the host language and pattern language
 *)
type 'a matcher = 'a -> 'a -> tin -> tout

(*e: type [[Matching_generic.matcher]] *)

type 'a comb_result = tin -> ('a * tout) list
(** See [comb_matcher] *)

type 'a comb_matcher = 'a -> 'a list -> 'a list comb_result
(** A "combinatorial" matcher takes an element A, a list of elements Bs,
 * and an input environment, and returns a list of pairs (Bs', tout),
 * where tout represents a match between A and a sub-lit of Bs, and
 * Bs' is a sub-list of Bs with the elements that were not matched.
 *
 * Used for Associative-Commutative (AC) matching! *)

(* monadic combinators *)
(*s: signature [[Matching_generic.monadic_bind]] *)
val ( >>= ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout

(*e: signature [[Matching_generic.monadic_bind]] *)
(*s: signature [[Matching_generic.TODOOPERATOR2]] *)
val ( >||> ) : (tin -> tout) -> (tin -> tout) -> tin -> tout

(*e: signature [[Matching_generic.TODOOPERATOR2]] *)
(*s: signature [[Matching_generic.TODOOPERATOR3]] *)
val ( >!> ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout

(*e: signature [[Matching_generic.TODOOPERATOR3]] *)

(*s: signature [[Matching_generic.return]] *)
val return : unit -> tin -> tout

(*e: signature [[Matching_generic.return]] *)
(*s: signature [[Matching_generic.fail]] *)
val fail : unit -> tin -> tout

(*e: signature [[Matching_generic.fail]] *)

val or_list : 'a matcher -> 'a -> 'a list -> tin -> tout

(* shortcut for >>=, since OCaml 4.08 you can define those "extended-let" *)
val ( let* ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout

(*s: signature [[Matching_generic.empty_environment]] *)
val empty_environment : tout Caching.Cache.t option -> Config_semgrep.t -> tin

(*e: signature [[Matching_generic.empty_environment]] *)

val add_mv_capture : Metavariable.mvar -> Metavariable.mvalue -> tin -> tin

val get_mv_capture : Metavariable.mvar -> tin -> Metavariable.mvalue option

(* Update the matching list of statements by providing a new matching
   statement. *)
val extend_stmts_match_span : AST_generic.stmt -> tin -> tin

(*s: signature [[Matching_generic.envf]] *)
val envf :
  Metavariable.mvar AST_generic.wrap -> Metavariable.mvalue -> tin -> tout

(*e: signature [[Matching_generic.envf]] *)

val if_config :
  (Config_semgrep.t -> bool) ->
  then_:(tin -> tout) ->
  else_:(tin -> tout) ->
  tin ->
  tout

(*s: signature [[Matching_generic.check_and_add_metavar_binding]] *)
val check_and_add_metavar_binding :
  Metavariable.mvar * Metavariable.mvalue -> tin -> tin option

(*e: signature [[Matching_generic.check_and_add_metavar_binding]] *)

(* helpers *)
(*s: signature [[Matching_generic.has_ellipsis_stmts]] *)
val has_ellipsis_stmts : AST_generic.stmt list -> bool

(*e: signature [[Matching_generic.has_ellipsis_stmts]] *)
val inits_and_rest_of_list_empty_ok : 'a list -> ('a list * 'a list) list

(*s: signature [[Matching_generic.all_elem_and_rest_of_list]] *)
val all_elem_and_rest_of_list : 'a list -> ('a * 'a list Lazy.t) list

(*e: signature [[Matching_generic.all_elem_and_rest_of_list]] *)

(* [all_splits xs] returns all possible pairs [(ls, rs)] such that [ls@rs]
  * contains the same elements as [xs].
  *
  * e.g.
  *     all_splits [1; 2] = [ ([1;2], []); ([2], [1]); ([1], [2]); ([], [1;2]) ] *)
val all_splits : 'a list -> ('a list * 'a list) list

val lazy_rest_of_list : 'a Lazy.t -> 'a

(*s: signature [[Matching_generic.is_regexp_string]] *)
(*e: signature [[Matching_generic.is_regexp_string]] *)
type regexp = Re.re

(*s: signature [[Matching_generic.regexp_of_regexp_string]] *)
val regexp_matcher_of_regexp_string : string -> string -> bool

(*e: signature [[Matching_generic.regexp_of_regexp_string]] *)

val equal_ast_binded_code :
  Config_semgrep.t -> Metavariable.mvalue -> Metavariable.mvalue -> bool

(* generic matchers *)
(*s: signature [[Matching_generic.m_option]] *)
val m_option : 'a matcher -> 'a option matcher

(*e: signature [[Matching_generic.m_option]] *)
(*s: signature [[Matching_generic.m_option_ellipsis_ok]] *)
val m_option_ellipsis_ok :
  AST_generic.expr matcher -> AST_generic.expr option matcher

(*e: signature [[Matching_generic.m_option_ellipsis_ok]] *)
(*s: signature [[Matching_generic.m_option_none_can_match_some]] *)
val m_option_none_can_match_some : 'a matcher -> 'a option matcher

(*e: signature [[Matching_generic.m_option_none_can_match_some]] *)

(*s: signature [[Matching_generic.m_ref]] *)
val m_ref : 'a matcher -> 'a ref matcher

(*e: signature [[Matching_generic.m_ref]] *)

(*s: signature [[Matching_generic.m_list]] *)
val m_list : 'a matcher -> 'a list matcher

(*e: signature [[Matching_generic.m_list]] *)
(*s: signature [[Matching_generic.m_list_prefix]] *)
val m_list_prefix : 'a matcher -> 'a list matcher

(*e: signature [[Matching_generic.m_list_prefix]] *)
(*s: signature [[Matching_generic.m_list_with_dots]] *)
val m_list_with_dots : 'a matcher -> ('a -> bool) -> bool -> 'a list matcher

(*e: signature [[Matching_generic.m_list_with_dots]] *)
val m_list_in_any_order : less_is_ok:bool -> 'a matcher -> 'a list matcher

val m_comb_unit : 'a -> 'a comb_result
(** Unit operation for the comb_result monad. *)

val m_comb_bind : 'a comb_result -> ('a -> 'b comb_result) -> 'b comb_result
(** Bind operation for the comb_result monad. *)

val m_comb_fold :
  'a comb_matcher -> 'a list -> 'a list comb_result -> 'a list comb_result
(** [m_comb_fold m_comb xs comb_result] folds [xs] by sequentially matching
 * each [x] against each partial result in [comb_result].
 *
 * That is, m_comb_fold m_comb [x1; ...; xn] comb_result is:
 *
 *    m_comb_bind ((m_comb_bind comb_result (m_comb x1)) ...)) (m_comb xn)
 *)

val m_comb_flatten : 'a comb_result -> tin -> tout
(** [m_comb_flatten comb_result] takes each (xs, tout) pair in [comb_result],
 * drops the [xs] and concatenates all the [tout]. *)

val m_comb_1to1 : 'a matcher -> 'a comb_matcher
(** [m_comb_1to1 m] returns a combinatorial matcher that, given an element [x]
 * an a list of elements [ys], will try matching `x` against each `y` in `ys`.
 * Each succesful match returns `(ys \ y, m x y)`. This is essentially the
 * combinatorial version of `or_list`. *)

val m_comb_1toN : ('a -> 'a list -> tin -> tout) -> 'a comb_matcher
(** [m_comb_1toN m_1toN] returns a combinatorial matcher that, given an element
 * [x] and a list of elements [ys], will try matching `x` against each possible
 * sub-list [ys'] in [ys]. Each succesful match returns
 * [(ys \ ys', m_1toN x ys')]. *)

(* use = *)
val m_eq : 'a matcher

(*s: signature [[Matching_generic.m_bool]] *)
val m_bool : bool matcher

(*e: signature [[Matching_generic.m_bool]] *)
(*s: signature [[Matching_generic.m_int]] *)
val m_int : int matcher

(*e: signature [[Matching_generic.m_int]] *)
(*s: signature [[Matching_generic.m_string]] *)
val m_string : string matcher

(*e: signature [[Matching_generic.m_string]] *)
(*s: signature [[Matching_generic.string_is_prefix]] *)
val string_is_prefix : string -> string -> bool

(*e: signature [[Matching_generic.string_is_prefix]] *)
(*s: signature [[Matching_generic.m_string_prefix]] *)
val m_string_prefix : string matcher

(*e: signature [[Matching_generic.m_string_prefix]] *)
val m_string_ellipsis_or_metavar_or_default :
  ?m_string_for_default:string matcher -> string AST_generic.wrap matcher

val m_ellipsis_or_metavar_or_string : string AST_generic.wrap matcher

(*s: signature [[Matching_generic.m_info]] *)
val m_info : Parse_info.t matcher

(*e: signature [[Matching_generic.m_info]] *)
(*s: signature [[Matching_generic.m_tok]] *)
val m_tok : Parse_info.t matcher

(*e: signature [[Matching_generic.m_tok]] *)
(*s: signature [[Matching_generic.m_wrap]] *)
val m_wrap : 'a matcher -> 'a AST_generic.wrap matcher

(*e: signature [[Matching_generic.m_wrap]] *)
(*s: signature [[Matching_generic.m_bracket]] *)
val m_bracket : 'a matcher -> 'a AST_generic.bracket matcher

(*e: signature [[Matching_generic.m_bracket]] *)
val m_tuple3 : 'a matcher -> 'b matcher -> 'c matcher -> ('a * 'b * 'c) matcher

(*s: signature [[Matching_generic.m_other_xxx]] *)
val m_other_xxx : 'a matcher

(*e: signature [[Matching_generic.m_other_xxx]] *)
(*e: semgrep/matching/Matching_generic.mli *)

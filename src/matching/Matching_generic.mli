(* In what follows, tin stands for 'type in' and tout for 'type out' *)

(* incoming environment *)
type tin = {
  mv : Metavariable.bindings;
  stmts_matched : AST_generic.stmt list;
  (* TODO: this does not have to be in tout; maybe split tin in 2? *)
  lang : Lang.t;
  config : Rule_options.t;
  deref_sym_vals : int;
      (** Counts the number of times that we "follow" symbollically propagated
    * values. This is bound to prevent potential infinite loops. *)
  wildcard_imports : AST_generic.dotted_ident list;
      (** Stores the "wildcard imports" that import everything from a given
          module, but only the ones that occur at the top level of the program
          being matched. This might change the matching behavior of qualified
          name patterns, for instance.
          These look like "from A import *" in Python.
        *)
}

(* list of possible outcoming matching environments *)
and tout = tin list

(* A matcher is something taking an element A and an element B
 * (for this module A will be the AST of the pattern and B
 * the AST of the program we want to match over), then some environment
 * information tin, and it will return something (tout) that will
 * represent a match between element A and B.
 *)
(* currently A and B are the same type because we use the
 * same language for the host language and pattern language
 *)
type 'a matcher = 'a -> 'a -> tin -> tout
type ('a, 'b) general_matcher = 'a -> 'b -> tin -> tout

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
val ( >>= ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout
val ( >||> ) : (tin -> tout) -> (tin -> tout) -> tin -> tout
val ( >!> ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout
val return : unit -> tin -> tout
val fail : unit -> tin -> tout
val or_list : 'a matcher -> 'a -> 'a list -> tin -> tout

(* Shortcut for >>=. Since OCaml 4.08, you can define those "extended-let" *)
val ( let* ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout

val environment_of_program :
  Lang.t -> Rule_options.t -> AST_generic.program -> tin

val environment_of_any : Lang.t -> Rule_options.t -> AST_generic.any -> tin

(* This is mostly helpful for Generic_vs_generic, because we want to disable
   wildcard imports in certain cases (resolved name matching).
*)
val wipe_wildcard_imports : (tin -> tout) -> tin -> tout

(* to handle LocalImportAll *)
val with_additional_wildcard_import :
  AST_generic.dotted_ident -> (tin -> tout) -> tin -> tout

val add_mv_capture : Metavariable.mvar -> Metavariable.mvalue -> tin -> tin

(* Update the matching list of statements by providing a new matching
   statement. *)
val extend_stmts_matched : AST_generic.stmt -> tin -> tin

val envf :
  Metavariable.mvar AST_generic.wrap -> Metavariable.mvalue -> tin -> tout

val if_config :
  (Rule_options.t -> bool) ->
  then_:(tin -> tout) ->
  else_:(tin -> tout) ->
  tin ->
  tout

val with_lang : (Lang.t -> tin -> 'a) -> tin -> 'a

val check_and_add_metavar_binding :
  Metavariable.mvar * Metavariable.mvalue -> tin -> tin option

(* helpers *)
val inits_and_rest_of_list_empty_ok : 'a list -> ('a list * 'a list) list
val all_elem_and_rest_of_list : 'a list -> ('a * 'a list Lazy.t) list

(* [all_splits xs] returns all possible pairs [(ls, rs)] such that [ls@rs]
   * contains the same elements as [xs].
   *
   * e.g.
   *     all_splits [1; 2] = [ ([1;2], []); ([2], [1]); ([1], [2]); ([], [1;2]) ] *)
val all_splits : 'a list -> ('a list * 'a list) list
val lazy_rest_of_list : 'a Lazy.t -> 'a
val regexp_matcher_of_regexp_string : string -> string -> bool

val equal_ast_bound_code :
  Rule_options.t -> Metavariable.mvalue -> Metavariable.mvalue -> bool

(* generic matchers *)
val m_option : 'a matcher -> 'a option matcher

val m_option_ellipsis_ok :
  AST_generic.expr matcher -> AST_generic.expr option matcher

val m_option_none_can_match_some : 'a matcher -> 'a option matcher
val m_list : ('a, 'b) general_matcher -> ('a list, 'b list) general_matcher
val m_list_prefix : 'a matcher -> 'a list matcher

(* checks if a is a subsequence (not sublist or subset) of b.
 * e.g. a = [1; 3] and b = [1; 2; 3] does not fail,
 * and a = [1; 2] and b = [2; 1] fails
 *)
val m_list_subsequence :
  ('a, 'b) general_matcher -> ('a list, 'b list) general_matcher

(*
   Usage: m_list_with_dots less_is_ok f is_dots list_a list_b

   less_is_ok: whether the empty list pattern can match a non-empty list.
*)
val m_list_with_dots :
  less_is_ok:bool -> 'a matcher -> ('a -> bool) -> 'a list matcher

val m_list_with_dots_and_metavar_ellipsis :
  less_is_ok:bool ->
  f:'a matcher ->
  is_dots:('a -> bool) ->
  is_metavar_ellipsis:
    ('a -> (AST_generic.ident * ('a list -> Metavariable.mvalue)) option) ->
  'a list matcher

val m_list_in_any_order :
  less_is_ok:bool ->
  ('a, 'b) general_matcher ->
  ('a list, 'b list) general_matcher

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

(* use =*= *)
val m_eq : 'a matcher
val m_bool : bool matcher
val m_int : int matcher
val m_parsed_int : Parsed_int.t matcher
val m_string : string matcher
val filepath_is_prefix : string -> string -> bool
val m_filepath_prefix : string matcher

val m_string_ellipsis_or_metavar_or_default :
  ?m_string_for_default:string matcher -> string AST_generic.wrap matcher

val m_ellipsis_or_metavar_or_string : string AST_generic.wrap matcher
val m_info : Tok.t matcher
val m_tok : Tok.t matcher
val m_wrap : 'a matcher -> 'a AST_generic.wrap matcher
val m_bracket : 'a matcher -> 'a AST_generic.bracket matcher
val m_tuple3 : 'a matcher -> 'b matcher -> 'c matcher -> ('a * 'b * 'c) matcher
val m_other_xxx : 'a matcher

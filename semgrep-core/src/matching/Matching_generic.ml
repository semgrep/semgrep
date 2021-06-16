(*s: semgrep/matching/Matching_generic.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: pad/r2c copyright *)
open Common
module A = AST_generic
module B = AST_generic
module MV = Metavariable
module H = AST_generic_helpers
module AST = AST_generic
module Flag = Flag_semgrep
module Env = Metavariable_capture
module PI = Parse_info

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helper types and functions for Generic_vs_generic.ml.
 * See Generic_vs_generic.ml top comment for more information.
 *
 * todo:
 *  - use m_list_in_any_order at some point for:
 *     * m_list__m_field
 *     * m_list__m_attribute
 *     * m_list__m_xml_attr
 *     * m_list__m_argument (harder)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------*)
(* Combinators history *)
(* ------------------------------------------------------------------------*)
(*
 * version0:
 *   type ('a, 'b) matcher = 'a -> 'b -> bool
 *
 *   This just lets you know if you matched something.
 *
 * version1:
 *   type ('a, 'b) matcher = 'a -> 'b -> unit -> ('a, 'b) option
 *
 *   The Maybe monad.
 *
 * version2:
 *   type ('a, 'b) matcher = 'a -> 'b -> binding -> binding list
 *
 *   Why not returning a binding option ? because we need sometimes
 *   to return multiple possible bindings for one matching code.
 *   For instance with the pattern do 'f(..., $X, ...)', $X could be binded
 *   to different parts of the code.
 *
 *   Note that the empty list means a match failure.
 *
 * version3:
 *   type ('a, 'b) matcher = 'a -> 'b -> tin -> ('a,'b) tout
 *
 * version4: back to simpler
 *   type ('a, 'b) matcher = 'a -> 'b -> tin -> tout
 *)

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

type 'a comb_result = tin -> ('a * tout) list

type 'a comb_matcher = 'a -> 'a list -> 'a list comb_result

(*e: type [[Matching_generic.matcher]] *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)
(*s: function [[Matching_generic.str_of_any]] *)
(*e: function [[Matching_generic.str_of_any]] *)

(*****************************************************************************)
(* Monadic operators *)
(*****************************************************************************)
(* The >>= combinator below allow you to configure the matching process
 * anyway you want. Essentially this combinator takes a matcher,
 * another matcher, and returns a matcher that combines the 2
 * matcher arguments.
 *
 * In the case of a simple boolean matcher, you just need to write:
 *
 *   let (>>=) m1 m2 = fun tin ->
 *    match m1 tin with
 *    | None -> None
 *    | Some x ->
 *        m2 x tin
 *
 * For more context, this tutorial on monads in OCaml can be useful:
 * https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/ads/ex_maybe_monad.html
 *)

(*s: function [[Matching_generic.monadic_bind]] *)
let (( >>= ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout) =
 fun m1 m2 tin ->
  (* let's get a list of possible environment match (could be
   * the empty list when it didn't match, playing the role None
   * had before)
   *)
  let xs = m1 tin in
  (* try m2 on each possible returned bindings *)
  let xxs = xs |> List.map (fun binding -> m2 () binding) in
  List.flatten xxs

(*e: function [[Matching_generic.monadic_bind]] *)

(*s: function [[Matching_generic.monadic_or]] *)
(* the disjunctive combinator *)
let (( >||> ) : (tin -> tout) -> (tin -> tout) -> tin -> tout) =
 fun m1 m2 tin ->
  (* CHOICE
        let xs = m1 tin in
        if null xs
        then m2 tin
        else xs
  *)
  (* opti? use set instead of list *)
  m1 tin @ m2 tin

(*e: function [[Matching_generic.monadic_or]] *)

(*s: function [[Matching_generic.monadic_if_fail]] *)
(* the if-fail combinator *)
let ( >!> ) m1 else_cont tin =
  match m1 tin with [] -> (else_cont ()) tin | xs -> xs

(*e: function [[Matching_generic.monadic_if_fail]] *)

let if_config f ~then_ ~else_ tin =
  if f tin.config then then_ tin else else_ tin

(*s: function [[Matching_generic.return]] *)
(* The classical monad combinators *)
let (return : tin -> tout) = fun tin -> [ tin ]

(*e: function [[Matching_generic.return]] *)

(*s: function [[Matching_generic.fail]] *)
let (fail : tin -> tout) =
 fun _tin ->
  if !Flag.debug_matching then failwith "Generic_vs_generic.fail: Match failure";
  []

(*e: function [[Matching_generic.fail]] *)

let or_list m a bs =
  let rec aux xs = match xs with [] -> fail | b :: bs -> m a b >||> aux bs in
  aux bs

(* Since OCaml 4.08 you can define your own let operators!
 * alt: use ppx_let, but you need to write it as let%bind (uglier)
 * You can use the ppx future_syntax to support older version of OCaml, but
 * then you can not use other PPX rewriters (which we do).
 *)
let ( let* ) o f = o >>= f

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

let add_mv_capture key value (env : tin) =
  { env with mv = Env.add_capture key value env.mv }

let get_mv_capture key (env : tin) = Env.get_capture key env.mv

let extend_stmts_match_span rightmost_stmt (env : tin) =
  let stmts_match_span =
    Stmts_match_span.extend rightmost_stmt env.stmts_match_span
  in
  { env with stmts_match_span }

(*s: function [[Matching_generic.equal_ast_binded_code]] *)
(* pre: both 'a' and 'b' contains only regular code; there are no
 * metavariables inside them.
 *)
let rec equal_ast_binded_code (config : Config_semgrep.t) (a : MV.mvalue)
    (b : MV.mvalue) : bool =
  let res =
    match (a, b) with
    (* if one of the two IDs is not resolved, then we allow
     * a match, so a pattern like 'self.$FOO = $FOO' matches
     * code like 'self.foo = foo'.
     * Maybe we should not ... but let's try.
     *
     * At least we don't allow a resolved id with a precise sid to match
     * another id with a different sid (same id but in different scope),
     * which we rely on with our deep stmt matching hacks.
     *
     * TODO: relax even more and allow some id_resolved EnclosedVar (a field)
     * to match anything?
     *)
    | ( MV.Id ((s1, _), Some { AST.id_resolved = { contents = None }; _ }),
        MV.Id ((s2, _), _) )
    | ( MV.Id ((s1, _), _),
        MV.Id ((s2, _), Some { AST.id_resolved = { contents = None }; _ }) ) ->
        s1 = s2
    (* A variable occurrence that is known to have a constant value is equal to
     * that same constant value.
     *
     * THINK: We could also equal two different variable occurrences that happen
     * to have the same constant value. *)
    | ( MV.E (A.L a_lit),
        MV.Id (_, Some { B.id_constness = { contents = Some (B.Lit b_lit) }; _ })
      )
    | ( MV.Id (_, Some { A.id_constness = { contents = Some (A.Lit a_lit) }; _ }),
        MV.E (B.L b_lit) )
      when config.constant_propagation ->
        A.equal_literal a_lit b_lit
    (* general case, equality modulo-position-and-constness.
     * TODO: in theory we should use user-defined equivalence to allow
     * equality modulo-equivalence rewriting!
     * TODO? missing MV.Ss _, MV.Ss _ ??
     *)
    | MV.Id _, MV.Id _
    | MV.E _, MV.E _
    | MV.S _, MV.S _
    | MV.P _, MV.P _
    | MV.T _, MV.T _
    | MV.Text _, MV.Text _
    | MV.Args _, MV.Args _ ->
        (* Note that because we want to retain the position information
         * of the matched code in the environment (e.g. for the -pvar
         * sgrep command line argument), we can not just use the
         * generic '=' OCaml operator as 'a' and 'b' may represent
         * the same code but they will contain leaves in their AST
         * with different position information.

         * old: So before doing
         * the comparison we just need to remove/abstract-away
         * the line number information in each ASTs.
         * let a = MV.abstract_position_info_mval a in
         * let b = MV.abstract_position_info_mval b in
         * a =*= b
         *)
        (* This will perform equality but not care about:
         * - position information (see adhoc AST_generic.equal_tok)
         * - id_constness (see the special @equal for id_constness)
         *)
        MV.Structural.equal_mvalue a b
    | MV.Id _, MV.E (A.N (A.Id (b_id, b_id_info))) ->
        (* TODO still needed now that we have the better MV.Id of id_info? *)
        (* TOFIX: regression if remove this code *)
        (* Allow identifier nodes to match pure identifier expressions *)

        (* You should prefer to add metavar as expression (A.E), not id (A.I),
         * (see Generic_vs_generic.m_ident_and_id_info_add_in_env_Expr)
         * but in some cases you have no choice and you need to match an expression
         * metavar with an id metavar.
         * For example, we want the pattern 'const $X = foo.$X' to match 'const bar = foo.bar'
         * (this is useful in the Javascript transpilation context of complex pattern parameter).
         *)
        equal_ast_binded_code config a (MV.Id (b_id, Some b_id_info))
    | _, _ -> false
  in

  if not res then
    logger#ldebug
      (lazy (spf "A = %s\nB = %s\n" (MV.str_of_mval a) (MV.str_of_mval b)));
  res

(*e: function [[Matching_generic.equal_ast_binded_code]] *)

(*s: function [[Matching_generic.check_and_add_metavar_binding]] *)
let check_and_add_metavar_binding ((mvar : MV.mvar), valu) (tin : tin) =
  match Common2.assoc_opt mvar tin.mv.full_env with
  | Some valu' ->
      (* Should we use generic_vs_generic itself for comparing the code?
       * Hmmm, we can't because it leads to a circular dependencies.
       * Moreover here we know both valu and valu' are regular code,
       * not patterns, so we can just use the generic '=' of OCaml.
       *)
      if equal_ast_binded_code tin.config valu valu' then Some tin
        (* valu remains the metavar witness *)
      else None
  | None ->
      (* 'backrefs' is the set of metavariables that may be referenced later
         in the pattern. It's inherited from the last stmt pattern,
         so it might contain a few extra members.
      *)
      (* first time the metavar is bound, just add it to the environment *)
      Some (add_mv_capture mvar valu tin)

(*e: function [[Matching_generic.check_and_add_metavar_binding]] *)

(*s: function [[Matching_generic.envf]] *)
let (envf : MV.mvar AST.wrap -> MV.mvalue -> tin -> tout) =
 fun (mvar, _imvar) any tin ->
  match check_and_add_metavar_binding (mvar, any) tin with
  | None ->
      (*s: [[Matching_generic.envf]] if [[verbose]] when fail *)
      logger#ldebug (lazy (spf "envf: fail, %s (%s)" mvar (MV.str_of_mval any)));
      (*e: [[Matching_generic.envf]] if [[verbose]] when fail *)
      fail tin
  | Some new_binding ->
      (*s: [[Matching_generic.envf]] if [[verbose]] when success *)
      logger#ldebug
        (lazy (spf "envf: success, %s (%s)" mvar (MV.str_of_mval any)));
      (*e: [[Matching_generic.envf]] if [[verbose]] when success *)
      return new_binding

(*e: function [[Matching_generic.envf]] *)

(*s: function [[Matching_generic.empty_environment]] *)
let empty_environment opt_cache config =
  { mv = Env.empty; stmts_match_span = Empty; cache = opt_cache; config }

(*e: function [[Matching_generic.empty_environment]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Matching_generic.has_ellipsis_stmts]] *)
(* guard for deep stmt matching *)
let has_ellipsis_stmts xs =
  xs
  |> List.exists (fun st ->
         match st.A.s with A.ExprStmt (A.Ellipsis _, _) -> true | _ -> false)

(*e: function [[Matching_generic.has_ellipsis_stmts]] *)

let rec inits_and_rest_of_list = function
  | [] -> failwith "inits_1 requires a non-empty list"
  | [ e ] -> [ ([ e ], []) ]
  | e :: l ->
      ([ e ], l)
      :: List.map (fun (l, rest) -> (e :: l, rest)) (inits_and_rest_of_list l)

let _ =
  Common2.example
    ( inits_and_rest_of_list [ 'a'; 'b'; 'c' ]
    = [
        ([ 'a' ], [ 'b'; 'c' ]); ([ 'a'; 'b' ], [ 'c' ]); ([ 'a'; 'b'; 'c' ], []);
      ] )

let inits_and_rest_of_list_empty_ok = function
  | [] -> [ ([], []) ]
  | xs -> [ ([], xs) ] @ inits_and_rest_of_list xs

let _ =
  Common2.example
    ( inits_and_rest_of_list_empty_ok [ 'a'; 'b'; 'c' ]
    = [
        ([], [ 'a'; 'b'; 'c' ]);
        ([ 'a' ], [ 'b'; 'c' ]);
        ([ 'a'; 'b' ], [ 'c' ]);
        ([ 'a'; 'b'; 'c' ], []);
      ] )

(*s: function [[Matching_generic.all_elem_and_rest_of_list]] *)
(* todo? optimize, probably not the optimal version ... *)
let all_elem_and_rest_of_list xs =
  let rec loop acc prev_xs = function
    | [] -> acc
    | x :: next_xs ->
        let other_xs = lazy (List.rev_append prev_xs next_xs) in
        let acc' = (x, other_xs) :: acc in
        let prev_xs' = x :: prev_xs in
        loop acc' prev_xs' next_xs
  in
  loop [] [] xs
  [@@profiling]

let rec all_splits = function
  | [] -> [ ([], []) ]
  | x :: xs ->
      all_splits xs
      |> List.map (function ls, rs -> [ (x :: ls, rs); (ls, x :: rs) ])
      |> List.flatten

(*e: function [[Matching_generic.all_elem_and_rest_of_list]] *)

(*s: toplevel [[Matching_generic._1]] *)
(* let _ = Common2.example
    (all_elem_and_rest_of_list ['a';'b';'c'] =
     [('a', ['b';'c']); ('b', ['a';'c']); ('c', ['a';'b'])]) *)
(*e: toplevel [[Matching_generic._1]] *)

(* Since all_elem_and_rest_of_list computes the rest of list lazily,
 * we want to still keep track of how much time we're spending on
  * computing the rest of the list *)
let lazy_rest_of_list v =
  Common.profile_code "Matching_generic.eval_rest_of_list" (fun () ->
      Lazy.force v)

(*s: function [[Matching_generic.return_bis]] *)
let return () = return

(*e: function [[Matching_generic.return_bis]] *)
(*s: function [[Matching_generic.fail_bis]] *)
let fail () = fail

(*e: function [[Matching_generic.fail_bis]] *)

(*s: constant [[Matching_generic.regexp_regexp_string]] *)
(*e: constant [[Matching_generic.regexp_regexp_string]] *)
(*s: function [[Matching_generic.is_regexp_string]] *)
(*e: function [[Matching_generic.is_regexp_string]] *)

(* TODO: deprecate *)
type regexp = Re.re (* old: Str.regexp *)

(*s: function [[Matching_generic.regexp_of_regexp_string]] *)
let regexp_matcher_of_regexp_string s =
  if s =~ Pattern.regexp_regexp_string then (
    let x, flags = Common.matched2 s in
    let flags =
      match flags with
      | "" -> []
      | "i" -> [ `CASELESS ]
      | "m" -> [ `MULTILINE ]
      | _ -> failwith (spf "This is not a valid PCRE regexp flag: %s" flags)
    in
    (* old: let re = Str.regexp x in (fun s -> Str.string_match re s 0) *)
    (* TODO: add `ANCHORED to be consistent with Python re.match (!re.search)*)
    let re = Re.Pcre.regexp ~flags x in
    fun s2 ->
      Re.Pcre.pmatch ~rex:re s2 |> fun b ->
      logger#debug "regexp match: %s on %s, result = %b" s s2 b;
      b )
  else failwith (spf "This is not a PCRE-compatible regexp: " ^ s)

(*e: function [[Matching_generic.regexp_of_regexp_string]] *)

(*****************************************************************************)
(* Generic matchers *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* stdlib: option *)
(* ---------------------------------------------------------------------- *)
(*s: function [[Matching_generic.m_option]] *)
let (m_option : 'a matcher -> 'a option matcher) =
 fun f a b ->
  match (a, b) with
  | None, None -> return ()
  | Some xa, Some xb -> f xa xb
  | None, _ | Some _, _ -> fail ()

(*e: function [[Matching_generic.m_option]] *)

(*s: function [[Matching_generic.m_option_ellipsis_ok]] *)
(* dots: *)
let m_option_ellipsis_ok f a b =
  match (a, b) with
  | None, None -> return ()
  (* dots: ... can match 0 or 1 expression *)
  | Some (A.Ellipsis _), None -> return ()
  | Some xa, Some xb -> f xa xb
  | None, _ | Some _, _ -> fail ()

(*e: function [[Matching_generic.m_option_ellipsis_ok]] *)

(*s: function [[Matching_generic.m_option_none_can_match_some]] *)
(* less-is-ok: *)
let m_option_none_can_match_some f a b =
  match (a, b) with
  (* Nothing specified in the pattern can match Some stuff *)
  | None, _ -> return ()
  | Some xa, Some xb -> f xa xb
  | Some _, _ -> fail ()

(*e: function [[Matching_generic.m_option_none_can_match_some]] *)

(* ---------------------------------------------------------------------- *)
(* stdlib: ref *)
(* ---------------------------------------------------------------------- *)
(*s: function [[Matching_generic.m_ref]] *)
let (m_ref : 'a matcher -> 'a ref matcher) =
 fun f a b ->
  match (a, b) with { contents = xa }, { contents = xb } -> f xa xb

(*e: function [[Matching_generic.m_ref]] *)

(* ---------------------------------------------------------------------- *)
(* stdlib: list *)
(* ---------------------------------------------------------------------- *)

(*s: function [[Matching_generic.m_list]] *)
let rec m_list f a b =
  match (a, b) with
  | [], [] -> return ()
  | xa :: aas, xb :: bbs -> f xa xb >>= fun () -> m_list f aas bbs
  | [], _ | _ :: _, _ -> fail ()

(*e: function [[Matching_generic.m_list]] *)

(*s: function [[Matching_generic.m_list_prefix]] *)
let rec m_list_prefix f a b =
  match (a, b) with
  | [], [] -> return ()
  | xa :: aas, xb :: bbs -> f xa xb >>= fun () -> m_list_prefix f aas bbs
  (* less-is-ok: prefix is ok *)
  | [], _ -> return ()
  | _ :: _, _ -> fail ()

(*e: function [[Matching_generic.m_list_prefix]] *)

(*s: function [[Matching_generic.m_list_with_dots]] *)
let rec m_list_with_dots f is_dots less_is_ok xsa xsb =
  match (xsa, xsb) with
  | [], [] -> return ()
  (*s: [[Matching_generic.m_list_with_dots()]]> empty list vs list case *)
  (* less-is-ok: empty list can sometimes match non-empty list *)
  | [], _ :: _ when less_is_ok -> return ()
  (*e: [[Matching_generic.m_list_with_dots()]]> empty list vs list case *)
  (*s: [[Matching_generic.m_list_with_dots()]]> ellipsis cases *)
  (* dots: '...', can also match no argument *)
  | [ a ], [] when is_dots a -> return ()
  | a :: xsa, xb :: xsb when is_dots a ->
      (* can match nothing *)
      m_list_with_dots f is_dots less_is_ok xsa (xb :: xsb)
      >||> (* can match more *)
      m_list_with_dots f is_dots less_is_ok (a :: xsa) xsb
  (*e: [[Matching_generic.m_list_with_dots()]]> ellipsis cases *)
  (* the general case *)
  | xa :: aas, xb :: bbs ->
      f xa xb >>= fun () -> m_list_with_dots f is_dots less_is_ok aas bbs
  | [], _ | _ :: _, _ -> fail ()

(*e: function [[Matching_generic.m_list_with_dots]] *)

(* todo? opti? try to go faster to the one with split_when?
 * need reflect tin so we can call the matcher and query whether there
 * was a match. Maybe a <??> monadic operator?
 *)
let rec m_list_in_any_order ~less_is_ok f xsa xsb =
  match (xsa, xsb) with
  | [], [] -> return ()
  (* less-is-ok: empty list can sometimes match non-empty list *)
  | [], _ :: _ -> if less_is_ok then return () else fail ()
  | a :: xsa, xsb ->
      let candidates = all_elem_and_rest_of_list xsb in
      (* less: could use a fold *)
      let rec aux xs =
        match xs with
        | [] -> fail ()
        | (b, xsb) :: xs ->
            f a b
            >>= (fun () ->
                  m_list_in_any_order ~less_is_ok f xsa (lazy_rest_of_list xsb))
            >||> aux xs
      in
      aux candidates

(* ---------------------------------------------------------------------- *)
(* stdlib: combinatorial search *)
(* ---------------------------------------------------------------------- *)

(* Used for Associative-Commutative (AC) matching! *)

let m_comb_unit xs : _ comb_result = fun tin -> [ (xs, [ tin ]) ]

let m_comb_bind (comb_result : _ comb_result) f : _ comb_result =
 fun tin ->
  let rec loop = function
    | [] -> []
    | (bs, tout) :: comb_matches' ->
        let bs_matches =
          tout |> List.map (fun tin -> f bs tin) |> List.flatten
        in
        bs_matches @ loop comb_matches'
  in
  loop (comb_result tin)

let m_comb_flatten (comb_result : _ comb_result) (tin : tin) : tout =
  comb_result tin |> List.map snd |> List.flatten

let m_comb_fold (m_comb : _ comb_matcher) (xs : _ list)
    (comb_result : _ comb_result) : _ comb_result =
  List.fold_left
    (fun comb_result' x -> m_comb_bind comb_result' (m_comb x))
    comb_result xs

let m_comb_1to1 (m : _ matcher) a bs : _ comb_result =
 fun tin ->
  bs |> all_elem_and_rest_of_list
  |> List.filter_map (fun (b, other_bs) ->
         match m a b tin with
         | [] -> None
         | tout -> Some (Lazy.force other_bs, tout))

let m_comb_1toN m_1toN a bs : _ comb_result =
 fun tin ->
  bs |> all_splits
  |> List.filter_map (fun (l, r) ->
         match m_1toN a l tin with [] -> None | tout -> Some (r, tout))

(* ---------------------------------------------------------------------- *)
(* stdlib: bool/int/string/... *)
(* ---------------------------------------------------------------------- *)

let m_eq a b = if a = b then return () else fail ()

(*s: function [[Matching_generic.m_bool]] *)
let m_bool a b = if a = b then return () else fail ()

(*e: function [[Matching_generic.m_bool]] *)

(*s: function [[Matching_generic.m_int]] *)
let m_int a b = if a =|= b then return () else fail ()

(*e: function [[Matching_generic.m_int]] *)

(*s: function [[Matching_generic.m_string]] *)
let m_string a b = if a =$= b then return () else fail ()

(*e: function [[Matching_generic.m_string]] *)

(*s: function [[Matching_generic.string_is_prefix]] *)
let string_is_prefix s1 s2 =
  let len1 = String.length s1 and len2 = String.length s2 in
  if len1 < len2 then false
  else
    let sub = Str.first_chars s1 len2 in
    sub = s2

(*e: function [[Matching_generic.string_is_prefix]] *)

(*s: function [[Matching_generic.m_string_prefix]] *)
(* less-is-ok: *)
let m_string_prefix a b = if string_is_prefix b a then return () else fail ()

(*e: function [[Matching_generic.m_string_prefix]] *)

(* ---------------------------------------------------------------------- *)
(* Token *)
(* ---------------------------------------------------------------------- *)

(*s: function [[Matching_generic.m_info]] *)
(* we do not care about position! or differences in space/indent/comment!
 * so we can just  'return ()'
 *)
let m_info _a _b = return ()

(*e: function [[Matching_generic.m_info]] *)

(*s: function [[Matching_generic.m_tok]] *)
let m_tok a b = m_info a b

(*e: function [[Matching_generic.m_tok]] *)

(*s: function [[Matching_generic.m_wrap]] *)
let m_wrap f a b =
  match (a, b) with
  | (xaa, ainfo), (xbb, binfo) -> f xaa xbb >>= fun () -> m_info ainfo binfo

(*e: function [[Matching_generic.m_wrap]] *)

(*s: function [[Matching_generic.m_bracket]] *)
let m_bracket f (a1, a2, a3) (b1, b2, b3) =
  m_info a1 b1 >>= fun () ->
  f a2 b2 >>= fun () -> m_info a3 b3

(*e: function [[Matching_generic.m_bracket]] *)

let m_tuple3 m_a m_b m_c (a1, b1, c1) (a2, b2, c2) =
  (m_a a1 a2 >>= fun () -> m_b b1 b2) >>= fun () -> m_c c1 c2

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(* TODO: this would be simpler if we had an
 * AST_generic.String of string wrap bracket, but this requires
 * lots of work in our Pfff parsers (less in tree-sitter which already
 * split strings in different tokens).
 *)
let adjust_info_remove_enclosing_quotes (s, info) =
  let loc = PI.token_location_of_info info in
  let raw_str = loc.PI.str in
  let re = Str.regexp_string s in
  try
    let pos = Str.search_forward re raw_str 0 in
    let loc =
      {
        loc with
        PI.str = s;
        charpos = loc.charpos + pos;
        column = loc.column + pos;
      }
    in
    let info = { PI.transfo = PI.NoTransfo; token = PI.OriginTok loc } in
    (s, info)
  with Not_found ->
    logger#error "could not find %s in %s" s raw_str;
    (* return original token ... better than failwith? *)
    (s, info)

(* TODO: should factorize with m_ellipsis_or_metavar_or_string at some
 * point when AST_generic.String is of string bracket
 *)
let m_string_ellipsis_or_metavar_or_default ?(m_string_for_default = m_string) a
    b =
  match fst a with
  (* dots: '...' on string *)
  | "..." -> return ()
  (* metavar: *)
  | astr when MV.is_metavar_name astr ->
      let text = adjust_info_remove_enclosing_quotes b in
      envf a (MV.Text text)
  (* TODO: deprecate *)
  | astr when Pattern.is_regexp_string astr ->
      let f = regexp_matcher_of_regexp_string astr in
      if f (fst b) then return () else fail ()
  | _ -> m_wrap m_string_for_default a b

let m_ellipsis_or_metavar_or_string a b =
  match fst a with
  (* dots: '...' on string in atom/regexp/string *)
  | "..." -> return ()
  (* metavar: *)
  | s when MV.is_metavar_name s -> envf a (MV.Text b)
  | _ -> m_wrap m_string a b

(*s: function [[Matching_generic.m_other_xxx]] *)
let m_other_xxx a b =
  match (a, b) with a, b when a =*= b -> return () | _ -> fail ()

(*e: function [[Matching_generic.m_other_xxx]] *)
(*e: semgrep/matching/Matching_generic.ml *)

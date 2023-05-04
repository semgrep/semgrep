(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module B = AST_generic
module G = AST_generic
module MV = Metavariable
module H = AST_generic_helpers
module Flag = Flag_semgrep
module Env = Metavariable_capture

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
 *   For instance with the pattern do 'f(..., $X, ...)', $X could be bound
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

(* tin is for 'type in' and tout for 'type out' *)
(* incoming environment *)
type tin = {
  mv : Metavariable_capture.t;
  stmts_match_span : Stmts_match_span.t;
  cache : tout Caching.Cache.t option;
  (* TODO: this does not have to be in tout; maybe split tin in 2? *)
  lang : Lang.t;
  config : Config_semgrep.t;
  deref_sym_vals : int;
}

(* list of possible outcoming matching environments *)
and tout = tin list

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
type ('a, 'b) general_matcher = 'a -> 'b -> tin -> tout
type 'a comb_result = tin -> ('a * tout) list
type 'a comb_matcher = 'a -> 'a list -> 'a list comb_result

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

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

let (( >>= ) : (tin -> tout) -> (unit -> tin -> tout) -> tin -> tout) =
 fun m1 m2 tin ->
  (* let's get a list of possible environment match (could be
   * the empty list when it didn't match, playing the role None
   * had before)
   *)
  let xs = m1 tin in
  (* try m2 on each possible returned bindings *)
  let xxs = xs |> Common.map (fun binding -> m2 () binding) in
  List.flatten xxs

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

(* the if-fail combinator *)
let ( >!> ) m1 else_cont tin =
  match m1 tin with
  | [] -> (else_cont ()) tin
  | xs -> xs

let if_config f ~then_ ~else_ tin =
  if f tin.config then then_ tin else else_ tin

let with_lang f tin = f tin.lang tin

(* The classical monad combinators *)
let (return : tin -> tout) = fun tin -> [ tin ]

let (fail : tin -> tout) =
 fun _tin ->
  if !Flag.debug_matching then failwith "Generic_vs_generic.fail: Match failure";
  []

let or_list m a bs =
  let rec aux xs =
    match xs with
    | [] -> fail
    | b :: bs -> m a b >||> aux bs
  in
  aux bs

(* Since OCaml 4.08 you can define your own let operators!
 * alt: use ppx_let, but you need to write it as let%bind (uglier)
 * You can use the ppx future_syntax to support older version of OCaml, but
 * then you can not use other PPX rewriters (which we do).
 *)
let ( let* ) o f = o >>= f

(* TODO: could maybe also define
   let (let/) o f =
     match o with
     | None -> fail ()
     | Some x -> f x
   useful in Generic_vs_generic when see code like 'None -> fail()'
*)

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

let add_mv_capture key value (env : tin) =
  { env with mv = Env.add_capture key value env.mv }

let extend_stmts_match_span rightmost_stmt (env : tin) =
  let stmts_match_span =
    Stmts_match_span.extend rightmost_stmt env.stmts_match_span
  in
  { env with stmts_match_span }

(* pre: both 'a' and 'b' contains only regular code; there are no
 * metavariables inside them.
 *)
let rec equal_ast_bound_code (config : Config_semgrep.t) (a : MV.mvalue)
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
    | ( MV.Id ((s1, _), Some { G.id_resolved = { contents = None }; _ }),
        MV.Id ((s2, _), _) )
    | ( MV.Id ((s1, _), _),
        MV.Id ((s2, _), Some { G.id_resolved = { contents = None }; _ }) ) ->
        s1 = s2
    (* In Ruby, they use atoms for metaprogramming to generate fields
     * (e.g., 'serialize :tags ... post.tags') in which case we want
     * a Text metavariable like :$INPUT to be compared with an Id
     * metavariable like post.$INPUT.
     * TODO? split MV.Text in a separate MV.Atom?
     *)
    | ( MV.Id ((s1, _), Some { G.id_resolved = { contents = None }; _ }),
        MV.Text (s2, _, _) )
    | ( MV.Text (s1, _, _),
        MV.Id ((s2, _), Some { G.id_resolved = { contents = None }; _ }) ) ->
        s1 = s2
    (* A variable occurrence that is known to have a constant value is equal to
     * that same constant value.
     *
     * THINK: We could also equal two different variable occurrences that happen
     * to have the same constant value. *)
    | ( MV.E { e = G.L a_lit; _ },
        MV.Id (_, Some { B.id_svalue = { contents = Some (B.Lit b_lit) }; _ }) )
    | ( MV.Id (_, Some { G.id_svalue = { contents = Some (G.Lit a_lit) }; _ }),
        MV.E { e = B.L b_lit; _ } )
      when config.constant_propagation ->
        G.equal_literal a_lit b_lit
    (* We're adding this in as a hack, so that idents without id_infos can be allowed to
       match to metavariables. Notably, this allows things like qualified identifiers
       (within decorators) to match to metavariables.
       This almost certainly should break something at some point in the future, but for
       now we can allow it.
    *)
    | MV.Id ((s1, _), None), MV.Id ((s2, _), Some _) -> s1 = s2
    (* general case, equality modulo-position-and-svalue.
     * TODO: in theory we should use user-defined equivalence to allow
     * equality modulo-equivalence rewriting!
     * TODO? missing MV.Ss _, MV.Ss _ ??
     *)
    | MV.Id _, MV.Id _
    | MV.N _, MV.N _
    | MV.E _, MV.E _
    | MV.S _, MV.S _
    | MV.P _, MV.P _
    | MV.T _, MV.T _
    | MV.Raw _, MV.Raw _
    | MV.Text _, MV.Text _
    | MV.Params _, MV.Params _
    | MV.Args _, MV.Args _
    | MV.Xmls _, MV.Xmls _ ->
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
         * - id_svalue (see the special @equal for id_svalue)
         *)
        MV.Structural.equal_mvalue a b
    (* TODO still needed now that we have the better MV.Id of id_info? *)
    | MV.Id _, MV.E { e = G.N (G.Id (b_id, b_id_info)); _ } ->
        (* TOFIX: regression if remove this code *)
        (* Allow identifier nodes to match pure identifier expressions *)

        (* You should prefer to add metavar as expression (G.E), not id (G.I),
         * (see Generic_vs_generic.m_ident_and_id_info_add_in_env_Expr)
         * but in some cases you have no choice and you need to match an expr
         * metavar with an id metavar.
         * For example, we want the pattern 'const $X = foo.$X' to match
         *  'const bar = foo.bar'
         * (this is useful in the Javascript transpilation context of
         * complex pattern parameter).
         *)
        equal_ast_bound_code config a (MV.Id (b_id, Some b_id_info))
    (* TODO: we should get rid of that too, we should properly bind to MV.N *)
    | MV.E { e = G.N (G.Id (a_id, a_id_info)); _ }, MV.Id _ ->
        equal_ast_bound_code config (MV.Id (a_id, Some a_id_info)) b
    | _, _ -> false
  in

  if not res then
    logger#ldebug
      (lazy
        (spf "A != B\nA = %s\nB = %s\n" (MV.str_of_mval a) (MV.str_of_mval b)));
  res

let check_and_add_metavar_binding ((mvar : MV.mvar), valu) (tin : tin) =
  match Common2.assoc_opt mvar tin.mv.full_env with
  | Some valu' ->
      (* Should we use generic_vs_generic itself for comparing the code?
       * Hmmm, we can't because it leads to a circular dependencies.
       * Moreover here we know both valu and valu' are regular code,
       * not patterns, so we can just use the generic '=' of OCaml.
       *)
      if equal_ast_bound_code tin.config valu valu' then Some tin
        (* valu remains the metavar witness *)
      else None
  | None ->
      (* 'backrefs' is the set of metavariables that may be referenced later
         in the pattern. It's inherited from the last stmt pattern,
         so it might contain a few extra members.
      *)
      (* first time the metavar is bound, just add it to the environment *)
      Some (add_mv_capture mvar valu tin)

let (envf : MV.mvar G.wrap -> MV.mvalue -> tin -> tout) =
 fun (mvar, _imvar) any tin ->
  match check_and_add_metavar_binding (mvar, any) tin with
  | None ->
      logger#ldebug (lazy (spf "envf: fail, %s (%s)" mvar (MV.str_of_mval any)));
      fail tin
  | Some new_binding ->
      logger#ldebug
        (lazy (spf "envf: success, %s (%s)" mvar (MV.str_of_mval any)));
      return new_binding

let empty_environment ?(mvar_context = None) opt_cache lang config =
  let mv =
    match mvar_context with
    | None -> Env.empty
    | Some bindings ->
        { full_env = bindings; min_env = []; last_stmt_backrefs = Set_.empty }
  in
  {
    mv;
    stmts_match_span = Empty;
    cache = opt_cache;
    lang;
    config;
    deref_sym_vals = 0;
  }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec inits_and_rest_of_list = function
  | [] -> failwith "inits_1 requires a non-empty list"
  | [ e ] -> [ ([ e ], []) ]
  | e :: l ->
      ([ e ], l)
      :: Common.map (fun (l, rest) -> (e :: l, rest)) (inits_and_rest_of_list l)

let _ =
  Common2.example
    (inits_and_rest_of_list [ 'a'; 'b'; 'c' ]
    =*= [
          ([ 'a' ], [ 'b'; 'c' ]);
          ([ 'a'; 'b' ], [ 'c' ]);
          ([ 'a'; 'b'; 'c' ], []);
        ])

let inits_and_rest_of_list_empty_ok = function
  | [] -> [ ([], []) ]
  | xs -> [ ([], xs) ] @ inits_and_rest_of_list xs

let _ =
  Common2.example
    (inits_and_rest_of_list_empty_ok [ 'a'; 'b'; 'c' ]
    =*= [
          ([], [ 'a'; 'b'; 'c' ]);
          ([ 'a' ], [ 'b'; 'c' ]);
          ([ 'a'; 'b' ], [ 'c' ]);
          ([ 'a'; 'b'; 'c' ], []);
        ])

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
      |> List.concat_map (function ls, rs -> [ (x :: ls, rs); (ls, x :: rs) ])

(* let _ = Common2.example
    (all_elem_and_rest_of_list ['a';'b';'c'] =
     [('a', ['b';'c']); ('b', ['a';'c']); ('c', ['a';'b'])]) *)

(* Since all_elem_and_rest_of_list computes the rest of list lazily,
 * we want to still keep track of how much time we're spending on
  * computing the rest of the list *)
let lazy_rest_of_list v =
  Profiling.profile_code "Matching_generic.eval_rest_of_list" (fun () ->
      Lazy.force v)

let return () = return
let fail () = fail

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
    let re = SPcre.regexp ~flags x in
    fun s2 ->
      SPcre.pmatch_noerr ~rex:re s2 |> fun b ->
      logger#debug "regexp match: %s on %s, result = %b" s s2 b;
      b)
  else failwith (spf "This is not a PCRE-compatible regexp: " ^ s)

(*****************************************************************************)
(* Generic matchers *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* stdlib: option *)
(* ---------------------------------------------------------------------- *)
(* you should probably use m_option_none_can_match_some instead *)
let (m_option : 'a matcher -> 'a option matcher) =
 fun f a b ->
  match (a, b) with
  | None, None -> return ()
  | Some xa, Some xb -> f xa xb
  | None, _
  | Some _, _ ->
      fail ()

(* dots: *)
let m_option_ellipsis_ok f a b =
  match (a, b) with
  | None, None -> return ()
  (* dots: ... can match 0 or 1 expression *)
  | Some { G.e = G.Ellipsis _; _ }, None -> return ()
  | Some xa, Some xb -> f xa xb
  | None, _
  | Some _, _ ->
      fail ()

(* less-is-ok: *)
let m_option_none_can_match_some f a b =
  match (a, b) with
  (* Nothing specified in the pattern can match Some stuff *)
  | None, _ -> return ()
  | Some xa, Some xb -> f xa xb
  | Some _, None -> fail ()

(* ---------------------------------------------------------------------- *)
(* stdlib: list *)
(* ---------------------------------------------------------------------- *)

let rec m_list f a b =
  match (a, b) with
  | [], [] -> return ()
  | xa :: aas, xb :: bbs -> f xa xb >>= fun () -> m_list f aas bbs
  | [], _
  | _ :: _, _ ->
      fail ()

let rec m_list_prefix f a b =
  match (a, b) with
  | [], [] -> return ()
  | xa :: aas, xb :: bbs -> f xa xb >>= fun () -> m_list_prefix f aas bbs
  (* less-is-ok: prefix is ok *)
  | [], _ -> return ()
  | _ :: _, _ -> fail ()

let rec m_list_with_dots ~less_is_ok f is_dots xsa xsb =
  match (xsa, xsb) with
  | [], [] -> return ()
  (* less-is-ok: empty list can sometimes match non-empty list *)
  | [], _ :: _ when less_is_ok -> return ()
  (* dots: '...', can also match no argument *)
  | [ a ], [] when is_dots a -> return ()
  | a :: xsa, xb :: xsb when is_dots a ->
      (* can match nothing *)
      m_list_with_dots f is_dots ~less_is_ok xsa (xb :: xsb)
      >||> (* can match more *)
      m_list_with_dots f is_dots ~less_is_ok (a :: xsa) xsb
  (* the general case *)
  | xa :: aas, xb :: bbs ->
      f xa xb >>= fun () -> m_list_with_dots f is_dots ~less_is_ok aas bbs
  | [], _
  | _ :: _, _ ->
      fail ()

let m_list_with_dots_and_metavar_ellipsis ~less_is_ok ~f ~is_dots
    ~is_metavar_ellipsis xsa xsb =
  let rec aux xsa xsb =
    match (xsa, xsb) with
    | [], [] -> return ()
    (* less-is-ok: empty list can sometimes match non-empty list *)
    | [], _ :: _ when less_is_ok -> return ()
    (* dots: '...', can also match no argument *)
    | [ a ], [] when is_dots a -> return ()
    (* opti: if is_metavar_ellipsis and less_is_ok is false, then
     * it's useless to enumerate all the candidates below; only the
     * one that match everything will work
    | [ a ], xs when is_metavar_ellipsis a <> None && not less_is_ok ->
     *)
    (* dots: metavars: $...ARGS *)
    | a :: xsa, xsb when is_metavar_ellipsis a <> None -> (
        match is_metavar_ellipsis a with
        | None -> raise Impossible
        | Some ((s, tok), metavar_build) ->
            (* can match 0 or more arguments (just like ...) *)
            let candidates = inits_and_rest_of_list_empty_ok xsb in
            let rec aux2 xs =
              match xs with
              | [] -> fail ()
              | (inits, rest) :: xs ->
                  envf (s, tok) (metavar_build inits)
                  >>= (fun () -> aux xsa rest)
                  >||> aux2 xs
            in
            aux2 candidates)
    | a :: xsa, xb :: xsb when is_dots a ->
        (* can match nothing *)
        aux xsa (xb :: xsb)
        >||> (* can match more *)
        aux (a :: xsa) xsb
    (* the general case *)
    | xa :: aas, xb :: bbs -> f xa xb >>= fun () -> aux aas bbs
    | [], _
    | _ :: _, _ ->
        fail ()
  in
  aux xsa xsb

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
        let bs_matches = tout |> List.concat_map (fun tin -> f bs tin) in
        bs_matches @ loop comb_matches'
  in
  loop (comb_result tin)

let m_comb_flatten (comb_result : _ comb_result) (tin : tin) : tout =
  comb_result tin |> List.concat_map snd

let m_comb_fold (m_comb : _ comb_matcher) (xs : _ list)
    (comb_result : _ comb_result) : _ comb_result =
  List.fold_left
    (fun comb_result' x -> m_comb_bind comb_result' (m_comb x))
    comb_result xs

let m_comb_1to1 (m : _ matcher) a bs : _ comb_result =
 fun tin ->
  bs |> all_elem_and_rest_of_list
  |> Common.map_filter (fun (b, other_bs) ->
         match m a b tin with
         | [] -> None
         | tout -> Some (Lazy.force other_bs, tout))

let m_comb_1toN m_1toN a bs : _ comb_result =
 fun tin ->
  bs |> all_splits
  |> Common.map_filter (fun (l, r) ->
         match m_1toN a l tin with
         | [] -> None
         | tout -> Some (r, tout))

(* ---------------------------------------------------------------------- *)
(* stdlib: bool/int/string/... *)
(* ---------------------------------------------------------------------- *)

(* try to not use m_eq, you could get bad surprise *)
let m_eq a b = if a =*= b then return () else fail ()
let m_bool a b = if a =:= b then return () else fail ()
let m_int a b = if a =|= b then return () else fail ()
let m_string a b = if a = b then return () else fail ()

(* old: Before we just checked whether `s2` was a prefix of `s1`, e.g.
 * "foo" is a prefix of "foobar". However we use this function to check
 * file paths, and the path "foo" is NOT a prefix of the path "foobar".
 * We must also check that what comes after "foo", if anything, is a
 * path separator. *)
let filepath_is_prefix s1 s2 =
  (* todo: can we assume that the strings are trimmed? *)
  let is_sep c = c =$= '/' || c =$= '\\' in
  let len1 = String.length s1 and len2 = String.length s2 in
  if len1 < len2 then false
  else
    let sub = Str.first_chars s1 len2 in
    sub = s2 && (len1 =|= len2 || is_sep s1.[len2])

(* less-is-ok: *)
let m_filepath_prefix a b =
  if filepath_is_prefix b a then return () else fail ()

(* ---------------------------------------------------------------------- *)
(* Token *)
(* ---------------------------------------------------------------------- *)

(* we do not care about position! or differences in space/indent/comment!
 * so we can just  'return ()'
 *)
let m_info _a _b = return ()
let m_tok a b = m_info a b

let m_wrap f a b =
  match (a, b) with
  | (xaa, ainfo), (xbb, binfo) -> f xaa xbb >>= fun () -> m_info ainfo binfo

let m_bracket f (a1, a2, a3) (b1, b2, b3) =
  m_info a1 b1 >>= fun () ->
  f a2 b2 >>= fun () -> m_info a3 b3

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
  match Tok.loc_of_tok info with
  | Error _ ->
      (* We have no token location to adjust (typically a fake token),
       * this happens if the string is the result of constant folding. *)
      (s, info)
  | Ok loc -> (
      let raw_str = loc.Tok.str in
      let re = Str.regexp_string s in
      try
        let pos = Str.search_forward re raw_str 0 in
        let loc =
          {
            Tok.str = s;
            pos =
              {
                loc.pos with
                charpos = loc.pos.charpos + pos;
                column = loc.pos.column + pos;
              };
          }
        in
        let info = Tok.OriginTok loc in
        (s, info)
      with
      | Not_found ->
          logger#error "could not find %s in %s" s raw_str;
          (* return original token ... better than failwith? *)
          (s, info))

(* TODO: should factorize with m_ellipsis_or_metavar_or_string at some
 * point when AST_generic.String is of string bracket
 *)
let m_string_ellipsis_or_metavar_or_default ?(m_string_for_default = m_string) a
    b =
  match fst a with
  (* dots: '...' on string *)
  | "..." -> return ()
  (* metavar: "$MVAR" *)
  | astr when MV.is_metavar_name astr ->
      let _, orig_info = b in
      let s, info = adjust_info_remove_enclosing_quotes b in
      envf a (MV.Text (s, info, orig_info))
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
  | s when MV.is_metavar_name s ->
      let str, info = b in
      envf a (MV.Text (str, info, info))
  | _ -> m_wrap m_string a b

let m_other_xxx a b =
  match (a, b) with
  | a, b when a =*= b -> return ()
  | _ -> fail ()

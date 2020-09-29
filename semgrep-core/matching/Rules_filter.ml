(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
module Flag = Flag_semgrep
module R = Rule
module V = Visitor_AST
open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Rules filtering using regexps.
 *
 * It is useless to run the engine on rules containing specific IDs
 * (e.g., eval) that are never mentioned in the file.
 *
 * We did something similar in Coccinelle I think. This also has been
 * mentioned many times (by Clint, HN, etc.).
 *
 * notes: I tried to use the ocaml-re (Re) regexp libray instead of Str
 * because I thought it would be faster, and because it offers regexp
 * combinators (alt, rep, etc.) which might be useful at some point to
 * handle patterns containing explicit DisjExpr. However when running
 * on Zulip codebase with zulip semgrep rules, Str is actually faster
 * than Re.
 *
 * todo:
 *  - we could even avoid parsing the file by using a lazy AST
 *    in Semgrep_generic.check.
 *)
let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

(* Those functions using Re instead of Str are actually slower on
 * Zulip.
let regexp_matching_str s =
  Re.str s
let compile_regexp t =
  Re.compile t
let run_regexp re str =
  Re.execp re str
*)

let regexp_matching_str s =
  Str.regexp_string s
let compile_regexp t = t
[@@profiling]
let run_regexp re str =
   (* bugfix:
    * this does not work!:  Str.string_match re str 0
    * because you need to add ".*" in front to make it work,
    * (but then you can not use regexp_string above)
    * => use Str.search_forward instead.
    *)
   try
     Str.search_forward re str 0 |> ignore; true
   with Not_found -> false
[@@profiling]

let reserved_id lang str =
  Metavars_generic.is_metavar_name str ||
  (* in JS field names can be regexps *)
  (lang = Lang.Javascript && Matching_generic.is_regexp_string str) ||
  (* ugly hack that we then need to handle also here *)
  str = AST_generic.special_multivardef_pattern ||
  (* ugly: because ast_js_build introduce some extra "!default" ids *)
  (lang = Lang.Javascript && str = Ast_js.default_entity) ||
  (* parser_js.mly inserts some implicit this *)
  (lang = Lang.Java && str = "this")

let reserved_str str =
  str = "..." ||
  Matching_generic.is_regexp_string str

(*****************************************************************************)
(* ID extractor  *)
(*****************************************************************************)
let extract_specific_strings lang any =
  let res = ref [] in
  let visitor = V.mk_visitor {V.default_visitor with
     V.kident = (fun (_k, _) (str, _tok) ->
       if not (reserved_id lang str)
       then Common.push str res
     );
     V.kexpr = (fun (k, _) x ->
       (match x with
       (* less: we could extract strings for the other literals too?
        * atoms, chars, even int?
        *)
       | L (String (str, _tok)) ->
         if not (reserved_str str)
         then Common.push str res
       (* do not recurse there, the type does not have to be in the source *)
       | TypedMetavar _ ->
          ()
       | _ -> k x
       );
     );
  } in
  visitor any;
  !res

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let filter_rules_relevant_to_file_using_regexp rules lang file =
  let str = Common.read_file file in
  rules |> List.filter (fun rule ->
    let pat = rule.R.pattern in
    let xs = extract_specific_strings lang pat in
    (* pr2_gen xs; *)
    let match_ =
    (* we could avoid running multiple regexps on the same file
     * by first orring them and do the and only of the or succeed,
     * but probably not worth the opti.
      let t = xs |> List.map (fun x -> regexp_matching_str x) |> Re.alt in
      let re = compile_regexp t in
      run_regexp re str
      *)
      (* Note that right now we do a for_all but it mighe be incorrect
       * at some point if the pattern contains DisjExpr for example, in
       * which case we will need extract_specific_strings to directly
       * extract a complex regexp instead handling itself disjunction.
       *)
      xs |> List.for_all (fun x ->
         let t = regexp_matching_str x in
         let re = compile_regexp t in
         run_regexp re str
    )

    in
    if not match_
    then logger#info "filtering rule %s" rule.R.id;
    match_
    )
[@@profiling "Rules_filter.filter"]

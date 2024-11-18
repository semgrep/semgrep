(* Brandon Wu
 *
 * Copyright (C) 2023-present Semgrep Inc.
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
module Log = Log_analyzing.Log
module R = Rule
module RM = Range_with_metavars
module ME = Matching_explanation

(* This Formula_tbl structure is used to create a "formula cache", which will
   permit the sharing of matches resulting from common formulas that occur as
   sources, sinks, sanitizers, or propagators.

   In particular, because of hardcoded propagators, we expect to see lots of
   sharing from Semgrep Pro Engine.
*)
module Formula_tbl = Hashtbl.Make (struct
  type t = R.formula

  let equal = R.equal_formula
  let hash = R.hash_formula
end)

type formula_matches = RM.t list * ME.t list
type t = (formula_matches option * int) Formula_tbl.t

(* This function is for creating a formula cache which only caches formula that
   it knows will be shared, at least once, among the formula in a bunch of
   taint rules.

   This is because it's obviously not useful to cache a formula's matches if
   that formula never comes up again. This cache stores an option, with keys
   that are only formula that are guaranteed to appear more than once in the
   collection.
*)
let mk_specialized_formula_cache (rules : R.taint_rule list) =
  let count_tbl = Formula_tbl.create 128 in
  let flat_formulas =
    rules
    |> List.concat_map (fun (rule : R.taint_mode R.rule_info) ->
           let (`Taint (spec : R.taint_spec)) = rule.R.mode in
           R.formulas_of_mode (`Taint spec))
  in
  flat_formulas
  |> List.iter (fun formula ->
         match Formula_tbl.find_opt count_tbl formula with
         | None -> Formula_tbl.add count_tbl formula (None, 1)
         | Some (_, x) -> Formula_tbl.replace count_tbl formula (None, 1 + x));
  (* We return the table with pairs of (None, count) itself.
     When we try to cache a find, we will first check whether decreasing this
     counter results in 0. Then, there are no more uses, and the result is no
     longer worth caching.
     This way we don't keep around entries when we don't need to.
  *)
  count_tbl

let cached_find_opt formula_cache formula ~get_matches =
  match Formula_tbl.find_opt formula_cache formula with
  | None ->
      (* it should not actually be possible for a formula to
         not be in the formula table

         just don't cache it I guess
      *)
      Log.warn (fun m ->
          m
            "Tried to compute matches for a formula not in the cache \
             (impossible?)");
      get_matches ()
  | Some (None, count) ->
      let ranges, expls = get_matches () in
      if count <= 1 then
        (* if there's only 1 more use left, there's no point
           in caching it
        *)
        (ranges, expls)
      else (
        (* otherwise, this is the first time we've seen this
           formula, and we should cache it
        *)
        Formula_tbl.replace formula_cache formula
          (Some (ranges, expls), count - 1);
        (ranges, expls))
  | Some (Some (ranges, expls), count) ->
      if count <= 1 then Formula_tbl.remove formula_cache formula;
      (ranges, expls)

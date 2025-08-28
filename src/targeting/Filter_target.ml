(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Filter target candidates.

   Filtering each (rule, target) pair can become problematic since the number
   of such pairs is O(number of targets * number of rules).
   TODO? This is why we should cache the results of this step.
   This allows reducing the number of rules to the number of different
   languages and patterns used by the rules.
   update: there used to be such an opti, but Pad removed it, because
   it was not clear that was the actual bottleneck. Gitignore
   seems currently to be the thing to optimize.

   Partially translated from target_manager.py
*)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

(* Used by Core_runner.split_jobs_by_language() *)
let filter_target_for_analyzer (analyzer : Analyzer.t) (path : Fpath.t) : bool =
  match analyzer with
  | L (lang, langs) ->
      (* ok if the file appears to be in one of rule's languages *)
      lang :: langs
      |> List.exists (fun lang -> Guess_lang.inspect_file_p lang path)
  | LRegex
  | LSpacegrep
  | LAliengrep ->
      true

let filter_paths (paths : Rule.path_filter) (path : Fppath.t) : bool =
  if Fppath.is_filterable_DEPRECATED path then
    let ppath = path.ppath in
    let match_glob (glob : Rule.glob) (path : Ppath.t) : bool =
      Glob.Match.run glob.compiled_pattern (Ppath.to_string_fast path)
    in
    let { Rule.require; exclude } = paths in
    let is_excluded =
      exclude |> List.exists (fun glob -> match_glob glob ppath)
    in
    (* from the doc: "when mixing inclusion and exclusion filters,
     * the exclusion ones take precedence."
     *)
    if is_excluded then false
    else
      let is_required =
        match require with
        (* no require patterns means no constraints *)
        | [] -> true
        | _ -> require |> List.exists (fun glob -> match_glob glob ppath)
      in
      is_required
  else
    (* bypass all filters! *)
    true
[@@profiling]

(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Strategy-based pattern matching for gitignore patterns.

   Classifies gitignore patterns at compile time into fast-path strategies
   that avoid PCRE2 regex calls for simple patterns. Inspired by ripgrep's
   GlobSet approach (crates/globset/src/lib.rs).

   Strategies (checked in this order at match time):

   - Basename_literal: O(1) hash lookup on the path's last segment.
     Covers patterns like '.git', 'build/', 'node_modules/'.
   - Literal: O(1) hash lookup on the full path string.
     Covers anchored all-literal patterns like '/src/vendor'.
   - Extension: O(1) hash lookup on the file extension.
     Covers patterns like '*.js', '*.py'.
   - Required_extension: O(1) hash lookup gates a PCRE2 match.
     Covers complex patterns ending with a literal extension,
     like '*_test.go' or '*.min.js'.
   - Regex: full PCRE2 match (fallback for everything else).

   A [t] groups all patterns from a single gitignore level into
   per-strategy hash tables, so that a single lookup can resolve many
   patterns at once.
*)

open Common
open Glob.Pattern

(*****************************************************************************)
(* Strategy type *)
(*****************************************************************************)

type strategy =
  | Basename_literal of { basename : string; dir_only : bool }
  | Literal of { path : string; dir_only : bool }
  | Extension of { ext : string; dir_only : bool }
  | Required_extension of { ext : string }
  | Regex

(*****************************************************************************)
(* Level index *)
(*****************************************************************************)

type index_entry = {
  (* index into patterns_array to get the underlying pattern *)
  pattern_idx : int;
  dir_only : bool;
  is_negated : bool;
  loc : Glob.Match.loc;
}

(* Each table maps a key to the list of patterns classified under it —
   e.g. both [*.js] and [!*.js] land in [extension_table] under ".js".
   Table iteration order does not matter: [select_level] tags each hit
   with its [pattern_idx] and sorts before emitting, so the final event
   order matches the gitignore spec regardless of insertion order. *)
type tables = {
  basename_table : (string, index_entry list) Hashtbl.t;
  literal_table : (string, index_entry list) Hashtbl.t;
  extension_table : (string, index_entry list) Hashtbl.t;
  (* required_extension_table cannot determine whether a path is a match directly,
   * so does not need dir_only, is_negated, etc. Hashtable holds only the
   * indices into [patterns_array] of each pattern that has the given
   * extension key. *)
  required_extension_table : (string, int list) Hashtbl.t;
  regex_indices : int list;
}

type t = {
  level : Gitignore.level;
  patterns_array : Gitignore.path_selector array;
  index : tables;
}

(*****************************************************************************)
(* Path helpers *)
(*****************************************************************************)

(* Extract the file "extension" (last '.' and after), including the leading
   dot. Returns "" if the basename has no '.' or ends in '.'.

   A leading dot IS treated as an extension separator: [extension_of
   ".gitignore"] is [".gitignore"]. This matches gitignore's [glob_period =
   true] semantics where ['*'] matches leading dots, so pattern ['*.gitignore']
   matches a file literally named [.gitignore]. Changing this to return [""]
   for dotfiles would cause the [Extension] strategy to miss real matches.

   This lives here (rather than in Ppath) because it operates on a plain
   basename string and its dotfile handling is gitignore-specific —
   standard path libraries like Fpath treat [.gitignore] as having no
   extension. *)
let extension_of (basename : string) : string =
  match String.rindex_opt basename '.' with
  | Some i when i < String.length basename - 1 ->
      String.sub basename i (String.length basename - i)
  | _ -> ""

(*****************************************************************************)
(* Pattern classification *)
(*****************************************************************************)

(* True iff [frags] is a non-empty sequence of plain [Char] fragments with
   no '.' or '/' — i.e. a literal file-extension suffix like ["j"; "s"]. *)
let is_literal_ext_suffix (frags : segment_fragment list) : bool =
  frags <> []
  && List.for_all
       (fun f ->
         match f with
         | Char c -> c <> '.' && c <> '/'
         | _ -> false)
       frags

(* If [frags] is the plain [*.<ext>] form (Star, dot, then a literal
   extension suffix), return [Some ".<ext>"]. Otherwise [None]. *)
let extension_of_frags (frags : segment_fragment list) : string option =
  match frags with
  | Star :: Char '.' :: rest when is_literal_ext_suffix rest ->
      Some ("." ^ string_of_chars rest)
  | _ -> None

(* If [frags] ends in a literal [.<ext>] suffix with at least one fragment
   before the [.], return [Some ".<ext>"]. Otherwise [None]. *)
let required_extension_of_frags (frags : segment_fragment list) : string option
    =
  (* Munch trailing literal-ext chars from the reversed fragment list.
     Prepending into [acc] while consuming the reversed list yields the
     chars in their original (forward) order. *)
  let rec munch_extension_from_end acc = function
    | (Char c as frag) :: rest when c <> '.' && c <> '/' ->
        munch_extension_from_end (frag :: acc) rest
    | rest -> (acc, rest)
  in
  match munch_extension_from_end [] (List.rev frags) with
  | (_ :: _ as ext_frags), Char '.' :: _ :: _ ->
      Some ("." ^ string_of_chars ext_frags)
  | _ -> None

(* Strip a trailing empty segment (the [Segment []] that encodes a trailing
   slash), if one is present. *)
let strip_trailing_slash (segs : segment list) : segment list =
  match List.rev segs with
  | Segment [] :: r -> List.rev r
  | _ -> segs

(* Is [segs] an anchored all-literal path body? That is, every element is a
   [Segment] whose fragments are all [Char] (no globs, no [Any_subpath]).
   A trailing [Segment []] (dir marker) is allowed and ignored. *)
let is_anchored_literal (segs : segment list) : bool =
  List.for_all
    (fun s ->
      match s with
      | Segment frags -> all_chars frags
      | Any_subpath -> false)
    (strip_trailing_slash segs)

let classify (pat : Glob.Pattern.t) : strategy =
  match pat with
  (* Unanchored patterns: Seg[] :: Any_subpath :: ... *)
  | [ Segment []; Any_subpath; Segment (_ :: _ as frags) ] when all_chars frags
    ->
      Basename_literal { basename = string_of_chars frags; dir_only = false }
  | [ Segment []; Any_subpath; Segment (_ :: _ as frags); Segment [] ]
    when all_chars frags ->
      Basename_literal { basename = string_of_chars frags; dir_only = true }
  | Segment []
    :: Any_subpath
    :: Segment frags
    :: (([] | [ Segment [] ]) as tail) ->
      let dir_only = has_trailing_slash tail in
      let as_extension =
        extension_of_frags frags
        |> Option.map (fun ext -> Extension { ext; dir_only })
      in
      let as_required_extension =
        required_extension_of_frags frags
        |> Option.map (fun ext -> Required_extension { ext })
      in
      as_extension ||| (as_required_extension ||| Regex)
  (* Anchored patterns: Seg[] :: Seg[chars] :: ... (no Any_subpath) *)
  | Segment [] :: rest when is_anchored_literal rest ->
      let dir_only = has_trailing_slash rest in
      let body = if dir_only then strip_trailing_slash rest else rest in
      let path_segs =
        List.filter_map
          (fun s ->
            match s with
            | Segment frags -> Some (string_of_chars frags)
            | Any_subpath -> None)
          body
      in
      let path = "/" ^ String.concat "/" path_segs in
      Literal { path; dir_only }
  | _ -> Regex

(*****************************************************************************)
(* Index construction *)
(*****************************************************************************)

(* Prepend [v] to the list stored under [key] in [table], creating a
   singleton if [key] isn't yet present. [select_level] sorts all hits by
   [pattern_idx] before emitting, so the order of entries within a bucket
   does not affect the final result. *)
let add_to_list_table (table : ('k, 'v list) Hashtbl.t) (key : 'k) (v : 'v) :
    unit =
  let existing = Option.value (Hashtbl.find_opt table key) ~default:[] in
  Hashtbl.replace table key (v :: existing)

let create_index (classified : (strategy * bool * Glob.Match.loc) list) : tables
    =
  let basename_table = Hashtbl.create 16 in
  let literal_table = Hashtbl.create 4 in
  let extension_table = Hashtbl.create 4 in
  let required_extension_table = Hashtbl.create 4 in
  let regex_indices = ref [] in
  List.iteri
    (fun pattern_idx (strategy, is_negated, loc) ->
      match strategy with
      | Basename_literal { basename; dir_only } ->
          add_to_list_table basename_table basename
            { pattern_idx; dir_only; is_negated; loc }
      | Literal { path; dir_only } ->
          add_to_list_table literal_table path
            { pattern_idx; dir_only; is_negated; loc }
      | Extension { ext; dir_only } ->
          add_to_list_table extension_table ext
            { pattern_idx; dir_only; is_negated; loc }
      | Required_extension { ext } ->
          add_to_list_table required_extension_table ext pattern_idx
      | Regex -> regex_indices := pattern_idx :: !regex_indices)
    classified;
  {
    basename_table;
    literal_table;
    extension_table;
    required_extension_table;
    regex_indices = List.rev !regex_indices;
  }

(*****************************************************************************)
(* Public API *)
(*****************************************************************************)

let of_parsed_patterns ~level_kind ~source_name
    (parsed : Parse_gitignore.parsed_pattern list) : t =
  let level : Gitignore.level =
    {
      level_kind;
      source_name;
      patterns =
        List.map (fun (p : Parse_gitignore.parsed_pattern) -> p.selector) parsed;
    }
  in
  let patterns_array =
    Array.of_list
      (List.map (fun (p : Parse_gitignore.parsed_pattern) -> p.selector) parsed)
  in
  let classified =
    List.map
      (fun (p : Parse_gitignore.parsed_pattern) ->
        let strat = classify p.absolute_pattern in
        (strat, p.is_negated, p.selector.loc))
      parsed
  in
  let index = create_index classified in
  { level; patterns_array; index }

let level (il : t) : Gitignore.level = il.level

(*
   Reference implementation: run every pattern's PCRE matcher against the
   path, skipping the strategy index entirely. Useful as an equivalence
   oracle for tests.
*)
let select_level_naive (il : t) (path : Ppath.t) :
    Gitignore.selection_event list =
  Array.fold_left
    (fun acc (ps : Gitignore.path_selector) ->
      match ps.matcher path with
      | Some ev -> ev :: acc
      | None -> acc)
    [] il.patterns_array

(*
   Filter a path against a single level using its strategy-based index.
   Hash lookups resolve most patterns in O(1); only Regex and
   Required_extension patterns fall through to PCRE2.
*)
let select_level (il : t) (path : Ppath.t) : Gitignore.selection_event list =
  let index = il.index in
  let pats = il.patterns_array in
  let is_dir = Ppath.is_dir_path path in
  let hit_of_entry (e : index_entry) =
    let ev =
      if e.is_negated then Gitignore.Deselected e.loc
      else Gitignore.Selected e.loc
    in
    (e.pattern_idx, ev)
  in
  let lookup table key ~filter_dir_only =
    Option.value (Hashtbl.find_opt table key) ~default:[]
    |> List.filter_map (fun (e : index_entry) ->
        if filter_dir_only && e.dir_only then None else Some (hit_of_entry e))
  in
  let try_match idx =
    Option.map (fun ev -> (idx, ev)) (pats.(idx).Gitignore.matcher path)
  in
  (* Determine effective segment and dir_only filtering *)
  let seg, filter_dir_only =
    if is_dir then (Ppath.dir_segment path, false)
    else (Ppath.last_segment path, true)
  in
  let ext = if seg <> "" then extension_of seg else "" in
  let lit_key =
    if is_dir then
      let s = Ppath.to_string_fast path in
      String.sub s 0 (String.length s - 1)
    else Ppath.to_string_fast path
  in
  let all_hits =
    List_.flatten
      [
        (if seg <> "" then lookup index.basename_table seg ~filter_dir_only
         else []);
        (if ext <> "" then lookup index.extension_table ext ~filter_dir_only
         else []);
        lookup index.literal_table lit_key ~filter_dir_only;
        (* Required extension: extension check gates regex *)
        (if ext <> "" then
           Option.value
             (Hashtbl.find_opt index.required_extension_table ext)
             ~default:[]
           |> List.filter_map try_match
         else []);
        (* Regex fallback: no pre-filter *)
        List.filter_map try_match index.regex_indices;
      ]
  in
  (* Hits from different buckets (basename, extension, literal, ...) can
     interleave arbitrarily in pattern_idx, so sort descending by index in the level so the
     last-applied pattern ends up at the head, matching the
     [selection_event] convention (most recent first). [all_hits] typically
     has 0–2 elements, so the sort is effectively free. *)
  all_hits |> List.sort (fun (a, _) (b, _) -> Int.compare b a) |> List.map snd

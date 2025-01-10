(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
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
open Ppx_hash_lib.Std.Hash.Builtin

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Extended languages: everything from Lang.t + spacegrep (generic) and regex.

   TODO: we probably want to rename this file Target_analyzer.ml to better
   match Target_selector.ml
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The type of an analyzer. An analyzer determines how a file or a collection
   of files gets analyzed. It is often parametrized by search patterns
   and options but not always.

   In the common case of searching for semgrep patterns in a program,
   the analyzer determines the parser for the program as well as the parser
   for the patterns and possibly some search-time options that are
   language-specific.

   Analyzers can be used to analyze any kind of file, not just programs
   written in a traditional human-readable programming language. For example,
   we might want to search for known problems in PDF files or images which may
   neither be parsed into a generic AST nor scanned with a simple search
   pattern. The Analyzer.t type must accommodate all sorts of analyzers.

   less: merge with xpattern_kind? -> no. Not every analyzer takes a pattern.
   TODO: add other analyzers such as 'entropy' (which doesn't take a pattern
   but some options).
*)
type t =
  (* for "real" semgrep (the first language is used to parse the pattern) *)
  | L of Lang.t * Lang.t list
  (* for pattern-regex (referred as 'regex' or 'none' in languages:) *)
  | LRegex
  (* generic mode uses either spacegrep or aliengrep *)
  | LSpacegrep
  | LAliengrep
[@@deriving show, eq, hash, yojson]

exception InternalInvalidLanguage of string (* rule id *) * string (* msg *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let of_lang (x : Lang.t) = L (x, [])

let to_lang (x : t) : (Lang.t, string) Result.t =
  match x with
  | L (lang, _) -> Ok lang
  | LRegex -> Error (Lang.unsupported_language_message "regex")
  | LSpacegrep
  | LAliengrep ->
      Error (Lang.unsupported_language_message "generic")

let to_lang_exn (x : t) : Lang.t =
  match to_lang x with
  | Ok lang -> lang
  | Error msg -> failwith msg

let to_langs (x : t) : Lang.t list =
  match x with
  | L (lang, langs) -> lang :: langs
  | LRegex
  | LSpacegrep
  | LAliengrep ->
      []

let lang_of_opt_analyzer_exn (x : t option) : Lang.t =
  match x with
  | None -> failwith (Lang.unsupported_language_message "unset")
  | Some analyzer -> to_lang_exn analyzer

let is_compatible ~require ~provide =
  match (require, provide) with
  | LRegex, LRegex
  | LSpacegrep, LSpacegrep
  | LAliengrep, LAliengrep ->
      true
  | (LRegex | LSpacegrep | LAliengrep), _ -> false
  | L (lang, []), L (lang2, langs2) ->
      Lang.equal lang lang2 || List.exists (Lang.equal lang) langs2
  | L (_lang, _extra_langs), _ ->
      (* invalid argument; could be an exception *)
      false

let flatten x =
  match x with
  | L (lang, langs) -> List_.map (fun x -> L (x, [])) (lang :: langs)
  | (LRegex | LSpacegrep | LAliengrep) as x -> [ x ]

let assoc : (string * t) list =
  List_.map (fun (k, v) -> (k, of_lang v)) Lang.assoc
  @ [
      ("regex", LRegex);
      ("none", LRegex);
      ("generic", LSpacegrep)
      (* this is commented because only 'generic' is allowed in
       * the languages: field in a Semgrep rule and we don't
       * want error messages about supported_analyzers to display
       * those entries.
       * coupling: see Parse_rule.parse_languages
       *
       * ("spacegrep", LSpacegrep);
       * ("aliengrep", LAliengrep);
       *);
    ]

let map = Hashtbl_.hash_of_list assoc
let keys = Hashtbl_.hkeys map
let supported_analyzers : string = String.concat ", " keys

let unsupported_analyzer_message (analyzer_s : string) =
  if analyzer_s = "unset" then "no language specified; use -lang"
  else
    Common.spf "unsupported language: %s; supported language tags are: %s"
      analyzer_s supported_analyzers

let of_string ?rule_id:id_opt s =
  match s with
  | "none"
  | "regex" ->
      LRegex
  | "generic"
  | "spacegrep" ->
      LSpacegrep
  | "aliengrep" -> LAliengrep
  | __else__ -> (
      match Lang.of_string_opt s with
      | None -> (
          match id_opt with
          | None -> failwith (unsupported_analyzer_message s)
          | Some id ->
              raise
                (InternalInvalidLanguage
                   (id, Common.spf "unsupported language: %s" s)))
      | Some l -> L (l, []))

let to_string = function
  | L (l, _) -> Lang.to_string l
  | LRegex -> "regex"
  | LSpacegrep -> "spacegrep"
  | LAliengrep -> "aliengrep"

let is_proprietary = function
  | L (lang, _) -> Lang.is_proprietary lang
  | LRegex
  | LSpacegrep
  | LAliengrep ->
      false

let wrap str = of_string str
let unwrap analyzer = to_string analyzer

let informative_suffix analyzer =
  match analyzer with
  | L (lang, _) -> (
      match Lang.ext_of_lang lang with
      | x :: _ -> x
      | [] -> "." ^ Lang.to_string lang)
  | LRegex
  | LSpacegrep
  | LAliengrep ->
      ".target-for-" ^ to_string analyzer

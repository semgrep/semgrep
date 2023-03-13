(*
   Extended languages: everything from Lang.t + spacegrep (generic) and regex.
*)

open Ppx_hash_lib.Std.Hash.Builtin

(* eXtended language, stored in the languages: field in the rule.
 * less: merge with xpattern_kind? *)
type t =
  (* for "real" semgrep (the first language is used to parse the pattern) *)
  | L of Lang.t * Lang.t list
  (* for pattern-regex (referred as 'regex' or 'none' in languages:) *)
  | LRegex
  (* for spacegrep *)
  | LGeneric
[@@deriving show, eq, hash]

exception InternalInvalidLanguage of string (* rule id *) * string (* msg *)

let of_lang (x : Lang.t) = L (x, [])

let to_lang_exn (x : t) : Lang.t =
  match x with
  | L (lang, _) -> lang
  | LRegex -> failwith (Lang.unsupported_language_message "regex")
  | LGeneric -> failwith (Lang.unsupported_language_message "generic")

let to_langs (x : t) : Lang.t list =
  match x with
  | L (lang, langs) -> lang :: langs
  | LRegex
  | LGeneric ->
      []

let lang_of_opt_xlang_exn (x : t option) : Lang.t =
  match x with
  | None -> failwith (Lang.unsupported_language_message "unset")
  | Some xlang -> to_lang_exn xlang

let flatten x =
  match x with
  | L (lang, langs) -> Common.map (fun x -> L (x, [])) (lang :: langs)
  | (LRegex | LGeneric) as x -> [ x ]

let assoc : (string * t) list =
  Common.map (fun (k, v) -> (k, of_lang v)) Lang.assoc
  @ [ ("regex", LRegex); ("none", LRegex); ("generic", LGeneric) ]

let map = Common.hash_of_list assoc
let keys = Common2.hkeys map
let supported_xlangs : string = String.concat ", " keys

let unsupported_xlang_message (xlang_s : string) =
  if xlang_s = "unset" then "no language specified; use -lang"
  else
    Common.spf "unsupported language: %s; supported language tags are: %s"
      xlang_s supported_xlangs

(* coupling: Parse_mini_rule.parse_languages *)
let of_string ?id:(id_opt = None) s =
  match s with
  | "none"
  | "regex" ->
      LRegex
  | "generic" -> LGeneric
  | __else__ -> (
      match Lang.of_string_opt s with
      | None -> (
          match id_opt with
          | None -> failwith (unsupported_xlang_message s)
          | Some id ->
              raise
                (InternalInvalidLanguage
                   (id, Common.spf "unsupported language: %s" s)))
      | Some l -> L (l, []))

let to_string = function
  | L (l, _) -> Lang.to_string l
  | LRegex -> "regex"
  | LGeneric -> "generic"

let is_proprietary = function
  | L (lang, _) -> Lang.is_proprietary lang
  | LRegex
  | LGeneric ->
      false

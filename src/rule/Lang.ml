(* Yoann Padioleau
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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
module FT = File_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module mirrors partially the content of lang.json
 * (in the semgrep-langs repository) in OCaml.
 * See semgrep-langs/README.md for more information.
 *
 * The languages supported by Semgrep used to be defined in a simple
 * Lang.ml file. However, this list of languages was duplicated in Python
 * for the Semgrep CLI and partially duplicated also for Semgrep App.
 * This info is now kept in a shared repo and OCaml files are generated
 * there.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*
   This constructor aliasing trick allows us to have all the types and
   functions in a single module, Lang.

   We need to keep this type definition to Language.t, which is generated.
   It should be easy since the compiler tells us what to when the type
   expressions don't match.
*)
type t = Language.t =
  | Apex
  | Bash
  | C
  | Cairo
  | Circom
  | Clojure
  | Cpp
  | Csharp
  | Dart
  | Dockerfile
  | Elixir
  | Go
  | Hack
  | Html
  | Java
  | Js
  | Json
  | Jsonnet
  | Julia
  | Kotlin
  | Lisp
  | Lua
  | Move_on_sui
  | Move_on_aptos
  | Ocaml
  | Php
  | Promql
  | Protobuf
  | Python2
  | Python3
  | Python
  | Ql
  | R
  | Ruby
  | Rust
  | Scala
  | Scheme
  | Solidity
  | Swift
  | Terraform
  | Ts
  | Vue
  | Xml
  | Yaml
[@@deriving show { with_path = false }, eq, hash, yojson]

let has_tag tag_name =
  let tbl = Hashtbl.create 50 in
  Language.list
  |> List.iter (fun (x : Language.info) ->
         if List.mem tag_name x.tags then Hashtbl.add tbl x.id ());
  fun lang -> Hashtbl.mem tbl lang

let is_js = has_tag "is_js"
let is_proprietary = has_tag "is_proprietary"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Get the info associated with a language *)
let info : t -> Language.info =
  let assoc =
    List_.map (fun (info : Language.info) -> (info.id, info)) Language.list
  in
  let tbl = Hashtbl_.hash_of_list assoc in
  fun key ->
    try Hashtbl.find tbl key with
    | Not_found ->
        (* code generation guarantees that there's one info entry per language *)
        assert false

let assoc =
  List.concat_map
    (fun (info : Language.info) ->
      List_.map (fun key -> (key, info.id)) info.keys)
    Language.list

let lang_map = Hashtbl_.hash_of_list assoc
let of_string_opt x = Hashtbl.find_opt lang_map (String.lowercase_ascii x)
let keys = Common2.hkeys lang_map
let supported_langs : string = String.concat ", " keys

(* TODO: move file identification to lang.json *)
(* TODO: Solidity *)
let langs_of_filename filename =
  let typ = File_type.file_type_of_file filename in
  match typ with
  | FT.PL (FT.Web FT.Js) -> [ Js ] (* Add TypeScript too? *)
  | FT.PL (FT.Web FT.TypeScript) -> [ Ts ]
  | FT.PL (FT.Web FT.Vue) -> [ Vue ]
  | FT.PL FT.Python -> [ Python; Python2; Python3 ]
  (* .h could also be Cpp at some point *)
  | FT.PL (FT.C "c") -> [ C ]
  | FT.PL (FT.C "h") -> [ C; Cpp ]
  | FT.PL (FT.Cplusplus _) -> [ Cpp ]
  | FT.PL (FT.OCaml ("ml" | "mli")) -> [ Ocaml ]
  | FT.PL FT.Java -> [ Java ]
  | FT.PL FT.Go -> [ Go ]
  | FT.Config FT.Json -> [ Json ]
  | FT.Config FT.Yaml -> [ Yaml ]
  | FT.Config FT.Terraform -> [ Terraform ]
  | FT.PL FT.Ruby -> [ Ruby ]
  | FT.PL FT.Julia -> [ Julia ]
  | FT.PL FT.Csharp -> [ Csharp ]
  | FT.PL (FT.Web (FT.Php _)) -> [ Php ]
  | FT.PL (FT.Web FT.Hack) -> [ Hack ]
  | FT.PL FT.Kotlin -> [ Kotlin ]
  | FT.PL FT.Lua -> [ Lua ]
  | FT.PL (FT.Script "bash") -> [ Bash ]
  | FT.PL FT.Rust -> [ Rust ]
  | FT.PL FT.Move -> [ Move_on_sui; Move_on_aptos ]
  | FT.PL FT.R -> [ R ]
  | FT.PL FT.Scala -> [ Scala ]
  | FT.PL FT.Swift -> [ Swift ]
  | FT.PL (FT.Web FT.Html) -> [ Html ]
  | FT.PL FT.Elixir -> [ Elixir ]
  | _ -> []

let lang_of_filename_exn filename =
  match langs_of_filename filename with
  | x :: _ -> x
  | [] ->
      failwith
        ("Could not infer a language from the filename: "
       ^ Fpath.to_string filename)

let to_string lang = (info lang).name

(* must match [a-z][a-z0-9]* *)
let to_lowercase_alnum lang = (info lang).id_string

(* must match [A-Z][a-z0-9]* *)
let to_capitalized_alnum lang = show lang

(*
   Exclusive file extensions for the language. See mli.
*)
let ext_of_lang lang = (info lang).exts
let excluded_exts_of_lang lang = (info lang).excluded_exts
let shebangs_of_lang lang = (info lang).shebangs

let unsupported_language_message lang =
  if lang = "unset" then "no language specified; use -lang"
  else
    Common.spf "unsupported language: %s; supported language tags are: %s" lang
      supported_langs

let of_string string =
  match of_string_opt string with
  | None -> failwith (unsupported_language_message string)
  | Some l -> l

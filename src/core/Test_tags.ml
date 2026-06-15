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
(*
   Tags used to filter tests.

   Tags are shared by Testo and Testo_lwt.
*)

open Common

(* A test that sometimes fails for unknown reasons *)
let flaky = Testo.Tag.declare "flaky"
let e2e = Testo.Tag.declare "e2e"

(* Curated smoke-test subset; see Test_tags.mli and 'make test-osx-smoke'. *)
let smoke = Testo.Tag.declare "smoke"

(* "lang.none" would be shorter but possibly confusing since we're using
   the term "generic" everywhere. *)
let lang_generic = Testo.Tag.declare "lang.generic"
let lang_tags = Hashtbl.create 100

let make_lang_tag lang =
  let tag = spf "lang.%s" (Lang.to_lowercase_alnum lang) |> Testo.Tag.declare in
  Hashtbl.add lang_tags lang tag

let get_lang_tag lang =
  match Hashtbl.find_opt lang_tags lang with
  | None -> assert false
  | Some tag -> tag

let () =
  Language.list |> List.iter (fun (x : Language.info) -> make_lang_tag x.id)

let tags_of_lang (lang : Lang.t) =
  let lang_tags = [ get_lang_tag lang ] in
  List_.flatten [ lang_tags ]

let tags_of_langs (langs : Lang.t list) =
  match langs with
  | [] -> [ lang_generic ]
  | langs ->
      langs |> List.map tags_of_lang |> List_.flatten
      |> List.sort Testo.Tag.compare
      |> List_.deduplicate_gen ~get_key:Testo.Tag.to_string

(* Supply Chain Analysis *)
let sca = Testo.Tag.declare "sca"

(*
   Declare the tag 'tr' for Transitive Reachability tests.
   It's also used for the end-to-end tests that are defined in another
   module.
*)
let tr = Testo.Tag.declare "tr"

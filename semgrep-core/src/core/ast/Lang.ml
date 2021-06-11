(*s: pfff/lang_GENERIC/parsing/Lang.ml *)
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
module FT = File_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* coupling: if you add a language here, after fixing the compilation errors,
 * you probably still need to add also special code in list_of_lang and
 * langs_of_filename below.
 *)
(*s: type [[Lang.t]] *)
type t =
  | Python
  (*s: [[Lang.t]] extra Python cases *)
  (* Python will start with Python3 mode and fall back to Python2 in case
   * of error. Python2 and Python3 are for specific version of Python
   * (no fallback) *)
  | Python2
  | Python3
  (*e: [[Lang.t]] extra Python cases *)
  (* system *)
  | C
  | Cplusplus
  | Rust
  (* mainstream with Gc *)
  | Javascript
  | Typescript
  | Java
  | Kotlin
  | Csharp
  | Go
  (* functional *)
  | OCaml
  | Scala
  (* scripting (Python is above) *)
  | Ruby
  | PHP
  | Hack
  | Lua
  (* data science *)
  | R
  (* config files *)
  | JSON
  | Yaml

(*e: type [[Lang.t]] *)
[@@ocamlformat "disable"]
[@@deriving show, eq]

let is_js = function Javascript | Typescript -> true | _ -> false

let is_python = function Python | Python2 | Python3 -> true | _ -> false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: constant [[Lang.list_of_lang]] *)
let list_of_lang =
  [
    ("py", Python);
    ("python", Python);
    ("python2", Python2);
    ("python3", Python3);
    ("js", Javascript);
    ("javascript", Javascript);
    ("json", JSON);
    ("ts", Typescript);
    ("typescript", Typescript);
    ("go", Go);
    ("golang", Go);
    ("c", C);
    ("cpp", Cplusplus);
    ("c++", Cplusplus);
    ("ml", OCaml);
    ("ocaml", OCaml);
    ("java", Java);
    ("ruby", Ruby);
    ("rb", Ruby);
    ("cs", Csharp);
    ("csharp", Csharp);
    ("c#", Csharp);
    ("php", PHP);
    ("hack", Hack);
    ("kt", Kotlin);
    ("lua", Lua);
    ("rs", Rust);
    ("rust", Rust);
    ("r", R);
    ("yaml", Yaml);
    ("scala", Scala);
  ]

(*e: constant [[Lang.list_of_lang]] *)

(*s: constant [[Lang.lang_of_string_map]] *)
let lang_of_string_map = Common.hash_of_list list_of_lang

(*e: constant [[Lang.lang_of_string_map]] *)

(*s: function [[Lang.lang_of_string_opt]] *)
let lang_of_string_opt x =
  Hashtbl.find_opt lang_of_string_map (String.lowercase_ascii x)

(*e: function [[Lang.lang_of_string_opt]] *)

(*s: function [[Lang.langs_of_filename]] *)
let langs_of_filename filename =
  let typ = File_type.file_type_of_file filename in
  match typ with
  | FT.PL (FT.Web FT.Js) -> [ Javascript ] (* Add TypeScript too? *)
  | FT.PL (FT.Web FT.TypeScript) -> [ Typescript ]
  | FT.PL FT.Python -> [ Python; Python2; Python2 ]
  (* .h could also be Cpp at some point *)
  | FT.PL (FT.C "c") -> [ C ]
  | FT.PL (FT.C "h") -> [ C; Cplusplus ]
  | FT.PL (FT.Cplusplus _) -> [ Cplusplus ]
  | FT.PL (FT.ML _) -> [ OCaml ]
  | FT.PL FT.Java -> [ Java ]
  | FT.PL FT.Go -> [ Go ]
  | FT.Config FT.Json -> [ JSON ]
  | FT.Config FT.Yaml -> [ Yaml ]
  | FT.PL FT.Ruby -> [ Ruby ]
  | FT.PL FT.Csharp -> [ Csharp ]
  | FT.PL (FT.Web (FT.Php _)) -> [ PHP ]
  | FT.PL (FT.Web FT.Hack) -> [ Hack ]
  | FT.PL FT.Kotlin -> [ Kotlin ]
  | FT.PL FT.Lua -> [ Lua ]
  | FT.PL FT.Rust -> [ Rust ]
  | FT.PL FT.R -> [ R ]
  | FT.PL FT.Scala -> [ Scala ]
  | _ -> []

(*e: function [[Lang.langs_of_filename]] *)

(*s: function [[Lang.string_of_lang]] *)
let string_of_lang = function
  | Python -> "Python"
  | Python2 -> "Python2"
  | Python3 -> "Python3"
  | Javascript -> "Javascript"
  | Typescript -> "Typescript"
  | JSON -> "JSON"
  | Java -> "Java"
  | C -> "C"
  | Cplusplus -> "C++"
  | OCaml -> "OCaml"
  | Go -> "Golang"
  | Ruby -> "Ruby"
  | Csharp -> "C#"
  | PHP -> "PHP"
  | Hack -> "Hack"
  | Kotlin -> "Kotlin"
  | Lua -> "Lua"
  | Rust -> "Rust"
  | R -> "R"
  | Yaml -> "Yaml"
  | Scala -> "Scala"

(*e: function [[Lang.string_of_lang]] *)

(*s: function [[Lang.ext_of_lang]] *)
(* Manually pulled from file_type_of_file2 in file_type.ml *)
let ext_of_lang = function
  | Python | Python2 | Python3 -> [ "py"; "pyi" ]
  | Javascript -> [ "js"; "jsx" ]
  | Typescript -> [ "ts"; "tsx" ]
  | JSON -> [ "json" ]
  | Java -> [ "java" ]
  | C -> [ "c" ]
  | Cplusplus -> [ "cc"; "cpp" ]
  | OCaml -> [ "ml"; "mli" ] (* this is not parsed yet: "mly"; "mll" *)
  | Go -> [ "go" ]
  | Ruby -> [ "rb" ]
  | Csharp -> [ "cs" ]
  | PHP -> [ "php" ]
  | Hack -> [ "hh"; "hck"; "hack" ]
  | Kotlin -> [ "kt" ]
  | Lua -> [ "lua" ]
  | Rust -> [ "rs" ]
  | R -> [ "r"; "R" ]
  | Yaml -> [ "yaml"; "yml" ]
  | Scala -> [ "scala" ]

(*e: function [[Lang.ext_of_lang]] *)

(*s: function [[Lang.find_source]] *)
let find_source lang xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename -> List.mem lang (langs_of_filename filename))
  |> Common.sort

(*e: function [[Lang.find_source]] *)

(*s: function [[Lang.files_of_dirs_or_files]] *)
(* this is used by sgrep, so it is probably better to keep the logic
 * simple and not perform any Skip_code filtering (bento already does that)
 *)
let files_of_dirs_or_files lang xs =
  (* old: let xs = List.map Common.fullpath xs in
   * better to not transform in fullpath; does not interact
   * well with -exclude-dir and anyway this should be done in the caller
   * or not at all. Better just do one thing here.
   *)
  find_source lang xs

(*e: function [[Lang.files_of_dirs_or_files]] *)
(*e: pfff/lang_GENERIC/parsing/Lang.ml *)

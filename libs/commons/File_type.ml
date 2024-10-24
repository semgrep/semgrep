(* Yoann Padioleau
 *
 * Copyright (C) 2010-2013 Facebook
 * Copyright (C) 2023 Semgrep Inc.
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
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* see also dircolors.el and LFS *)
type file_type =
  | PL of pl_type
  | Obj of string (* .o, .a, .aux, .bak, etc *)
  | Binary of string
  | Text of string (* tex, txt, readme, noweb, org, etc *)
  | Doc of string (* ps, pdf *)
  | Config of config_type (* json, yaml, ini *)
  | Media of media_type
  | Archive of string (* tgz, rpm, etc *)
  | Other of string
[@@deriving yojson]

(* programming languages *)
and pl_type =
  (* functional *)
  | OCaml of string (* mli, ml, mly, mll *)
  | FSharp of string (* fsi, fsx, fs *)
  | MLOther of string
  | Haskell of string
  | Lisp of lisp_type
  | Skip
  | Scala
  (* logic *)
  | Prolog of string
  (* classic script *)
  | Script of string (* sh, csh, awk, sed, etc *)
  (* mainstream *)
  | C of string
  | Cplusplus of string
  | Java
  | Kotlin
  | Csharp
  | ObjectiveC of string
  | Swift
  (* advanced script *)
  | Julia
  | Perl
  | Python
  | Ruby
  | Lua
  | R
  (* other *)
  | Erlang
  | Go
  | Rust
  | Move
  | Beta
  | Pascal
  | Haxe
  | Bytecode of string
  | Asm
  | Web of webpl_type
  | IDL of idl_type
  | MiscPL of string
  | Elixir
[@@deriving yojson]

and config_type =
  | Makefile
  | Dockerfile
  (* note: XML is in webpl_type below *)
  | Json
  | Jsonnet
  | Properties (* Java config *)
  | Ignore of string (* any sort of .gitignore *)
  | RC of string (* Usually key value, .yarnrc, .npmrc etc. *)
  (* kinda pl_type *)
  | Yaml
  | Terraform
  | Sexp (* e.g., dune files *)
  | Toml
[@@deriving yojson]

and lisp_type = CommonLisp | Elisp | Scheme | Clojure [@@deriving yojson]

and webpl_type =
  | Php of string (* php or phpt or script *)
  | Hack
  | Js
  | TypeScript (* JSX/TSX are converted in Js/Typescript *)
  | Coffee
  | Vue
  | Css
  | Scss
  | Html
  | Xml
  | Opa
  | Flash
  | Sql
[@@deriving yojson]

and idl_type = Thrift | ATD | Protobuf [@@deriving yojson]

and media_type = Sound of string | Picture of string | Video of string
[@@deriving yojson]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* this function is used by codemap and archi_parse and called for each
 * filenames, so it has to be fast!
 *)
let file_type_of_file file =
  let _d, b, e = Filename_.dbe_of_filename_noext_ok !!file in
  (* extensions are not case sensitive, at least on windows! *)
  let e = String.lowercase_ascii e in
  match e with
  | "ml"
  | "mli"
  | "mly"
  | "mll"
  | "dyp" (* dypgen =~ GLR ocamlyacc *) ->
      PL (OCaml e)
  | "mlb" (* mlburg *)
  | "mlp" (* used in some source *)
  | "eliom" (* ocsigen, obviously *) ->
      PL (OCaml e)
  | "sml" -> PL (MLOther e)
  (* fsharp *)
  | "fsi"
  | "fsx"
  | "fs" ->
      PL (FSharp e)
  (* linear ML *)
  | "lml" -> PL (MLOther e)
  | "hs"
  | "lhs" ->
      PL (Haskell e)
  | "scala"
  | "sc" ->
      PL Scala
  | "erl"
  | "hrl" ->
      PL Erlang
  | "hx"
  | "hxp"
  | "hxml" ->
      PL Haxe
  | "opa" -> PL (Web Opa)
  | "sk" -> PL Skip
  | "as" -> PL (Web Flash)
  | "bet" -> PL Beta
  (* todo detect false C file, look for "Mode: Objective-C++" string in file ?
   * can also be a c++, use Parser_cplusplus.is_problably_cplusplus_file
   *)
  | "c" -> PL (C e)
  | "h" -> PL (C e)
  (* todo? have a PL of xxx_kind * pl_kind ?  *)
  | "y"
  | "l" ->
      PL (C e)
  | "hpp" -> PL (Cplusplus e)
  | "hxx" -> PL (Cplusplus e)
  | "hh" -> PL (Cplusplus e)
  | "cpp" -> PL (Cplusplus e)
  | "C" -> PL (Cplusplus e)
  | "cc" -> PL (Cplusplus e)
  | "cxx" -> PL (Cplusplus e)
  (* used in libstdc++ *)
  | "tcc" -> PL (Cplusplus e)
  | "m"
  | "mm" ->
      PL (ObjectiveC e)
  | "swift" -> PL Swift
  | "java" -> PL Java
  | "kt" -> PL Kotlin
  | "cs" -> PL Csharp
  | "p" -> PL Pascal
  | "thrift" -> PL (IDL Thrift)
  | "atd" -> PL (IDL ATD)
  | "proto" -> PL (IDL Protobuf)
  | "scm"
  | "rkt"
  | "ss"
  | "lsp" ->
      PL (Lisp Scheme)
  | "lisp" -> PL (Lisp CommonLisp)
  | "el" -> PL (Lisp Elisp)
  | "clj" -> PL (Lisp Clojure)
  (* Perl or Prolog ... I made my choice *)
  | "pl" -> PL (Prolog "pl")
  | "perl" -> PL Perl
  | "py"
  | "pyi" ->
      PL Python
  | "jl" -> PL Julia
  | "rb" -> PL Ruby
  | "logic" -> PL (Prolog "logic") (* datalog of logicblox *)
  | "dtl" -> PL (Prolog "dtl") (* bddbddb *)
  | "dl" -> PL (Prolog "dl") (* datalog *)
  | "ql"
  | "qll" ->
      PL (MiscPL e) (* Semmle Query language *)
  | "clp" -> PL (Prolog e)
  | "s"
  | "S"
  | "asm" ->
      PL Asm
  | "c--" -> PL (MiscPL e)
  | "oz" -> PL (MiscPL e)
  | "groovy" -> PL (MiscPL e)
  | "sh"
  | "rc"
  | "csh"
  | "bash" ->
      PL (Script e)
  | "m4" -> PL (MiscPL e)
  | "conf" -> PL (MiscPL e)
  (* Andrew Appel's Tiger toy language *)
  | "tig" -> PL (MiscPL e)
  (* merd *)
  | "me" -> PL (MiscPL "me")
  | "vim" -> PL (MiscPL "vim")
  | "nanorc" -> PL (MiscPL "nanorc")
  (* from hex to bcc *)
  | "he" -> PL (MiscPL "he")
  | "bc" -> PL (MiscPL "bc")
  | "php"
  | "phpt" ->
      PL (Web (Php e))
  | "hck"
  | "hack" (* | "hh" *) ->
      (* ".hh" is also a popular choice for C++ header files *)
      PL (Web Hack)
  | "scss" -> PL (Web Scss)
  | "css" -> PL (Web Css)
  (* "javascript" | "es" | ? *)
  | "js" -> PL (Web Js)
  | "jsx" -> PL (Web Js) (* Js with JSX enabled *)
  | "coffee" -> PL (Web Coffee)
  | "ts" -> PL (Web TypeScript)
  | "tsx" -> PL (Web TypeScript) (* Typescript with JSX enabled *)
  | "vue" -> PL (Web Vue)
  | "html"
  | "htm" ->
      PL (Web Html)
  | "xml" -> PL (Web Xml)
  | "properties" -> Config Properties
  | "json" -> Config Json
  | "jsonnet"
  | "libsonnet" ->
      Config Jsonnet
  | "yml"
  | "yaml" ->
      Config Yaml
  | "tf" -> Config Terraform
  | "toml" -> Config Toml
  (* sometimes people use foo.Dockerfile *)
  | "Dockerfile" -> Config Dockerfile
  | "sql" -> PL (Web Sql)
  | "sqlite" -> PL (Web Sql)
  (* apple stuff ? *)
  | "xib" -> PL (Web Xml)
  (* xml i18n stuff for apple *)
  | "nib" -> Obj e
  (* facebook: sqlshim files *)
  | "sql3" -> PL (Web Sql)
  | "fbobj" -> PL (MiscPL "fbobj")
  | "png"
  | "psd" (* photoshop *)
  | "ai" (* adobe illustrator *)
  | "jpg"
  | "gif"
  | "svg"
  | "tif"
  | "tiff" ->
      Media (Picture e)
  | "xcf"
  | "xpm" ->
      Media (Picture e)
  | "icns"
  | "icon"
  | "ico" ->
      Media (Picture e)
  | "ppm" -> Media (Picture e)
  | "tga" -> Media (Picture e)
  | "woff2"
  | "ttf"
  | "font" ->
      Media (Picture e)
  | "wav" -> Media (Sound e)
  | "swf" -> Media (Picture e)
  | "indd" (* indesign document *)
  | "ps"
  | "pdf" ->
      Doc e
  | "ppt" -> Doc e
  | "tex"
  | "texi" ->
      Text e
  | "txt"
  | "doc" ->
      Text e
  | "nw"
  | "web" ->
      Text e
  | "ms" -> Text e
  | "org"
  | "md"
  | "rest"
  | "textile"
  | "wiki"
  | "rst" ->
      Text e
  | "rtf" -> Text e
  | "cmi"
  | "cmo"
  | "cmx"
  | "cma"
  | "cmxa"
  | "annot"
  | "cmt"
  | "cmti"
  | "o"
  | "a"
  | "pyc"
  | "log"
  | "toc"
  | "brf"
  | "out"
  | "output"
  | "hi"
  | "msi" ->
      Obj e
  (* pad: I use it to store marshalled data *)
  | "db" -> Obj e
  | "po"
  | "pot"
  | "gmo" ->
      Obj e
  (* facebook fbcode stuff *)
  | "apcarc"
  | "serialized"
  | "wsdl"
  | "dat"
  | "train" ->
      Obj e
  | "facts" -> Obj e (* logicblox *)
  (* pad specific, cached git blame info *)
  | "git_annot" -> Obj e
  (* pad specific, codegraph cached data *)
  | "marshall"
  | "matrix" ->
      Obj e
  | "byte"
  | "top" ->
      Binary e
  | "tar" -> Archive e
  | "tgz" -> Archive e
  (* was PL Bytecode, but more accurate as an Obj *)
  | "class" -> Obj e
  (* pad specific, clang ast dump *)
  | "clang"
  | "c.clang2"
  | "h.clang2"
  | "clang2" ->
      Obj e
  (* was Archive *)
  | "xlsx" -> Archive e (* excel spreadsheets. They're zip files! *)
  | "jar" -> Archive e
  | "bz2" -> Archive e
  | "gz" -> Archive e
  | "rar" -> Archive e
  | "zip" -> Archive e
  | "exe" -> Binary e
  | "mk" -> Config Makefile
  | "rs" -> PL Rust
  | "move" -> PL Move
  | "mod"
  | "go" ->
      PL Go
  | "lua" -> PL Lua
  | "r" -> PL R
  | "ex" -> PL Elixir
  | _ when UFile.is_executable file -> Binary e
  | _ when b = "Makefile" || b = "mkfile" || b = "Imakefile" -> Config Makefile
  | _ when b = "Dockerfile" -> Config Dockerfile
  | _ when b = "dune" -> Config Sexp
  | _ when b = "README" -> Text "txt"
  | _ when b = "CODEOWNERS" -> Text "txt"
  | _ when b = "LICENSE" -> Text "txt"
  | _ when b = "TAGS" -> Binary e
  | _ when b = "TARGETS" -> Config Makefile
  | _ when b = ".depend" -> Obj "depend"
  | _ when b = ".emacs" -> PL (Lisp Elisp)
  | _ when b = ".gitattributes" -> Text b
  | _ when b = ".gitkeep" -> Text b
  | _ when String.starts_with "." b && String.ends_with "ignore" b ->
      Config (Ignore b)
  | _ when String.starts_with "." b && String.ends_with "rc" b -> Config (RC b)
  | _ when UFile.filesize file > 300_000 -> Obj e
  | _ -> Other e

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* this is used in codemap, to know whether to display a file *)
let is_textual_file file =
  match file_type_of_file file with
  (* still? if this contains weird code then pfff_visual crash *)
  | PL (Web Sql) -> false
  | PL _
  | Text _
  | Config _ ->
      true
  | Obj _
  | Binary _
  | Media _
  | Doc _
  | Archive _
  | Other _ ->
      false

let webpl_type_of_file file =
  match file_type_of_file file with
  | PL (Web x) -> Some x
  | _ -> None

let is_syncweb_obj_file file = !!file =~ ".*md5sum_"
let is_json_filename filename = !!filename =~ ".*\\.json$"

let files_of_dirs_or_files p xs =
  xs |> UFile.files_of_dirs_or_files_no_vcs_nofilter
  |> List.filter (fun filename -> p (file_type_of_file filename))
  |> List_.sort

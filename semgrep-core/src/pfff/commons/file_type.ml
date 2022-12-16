(* Yoann Padioleau
 *
 * Copyright (C) 2010-2013 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* see also dircolors.el and LFS *)
type file_type =
  | PL of pl_type
  | Obj  of string (* .o, .a, .aux, .bak, etc *)
  | Binary of string
  | Text of string (* tex, txt, readme, noweb, org, etc *)
  | Doc  of string (* ps, pdf *)
  | Config of config_type (* json, yaml, ini *)
  | Media of media_type
  | Archive of string (* tgz, rpm, etc *)
  | Other of string

(* programming languages *)
and pl_type =
  (* functional *)
  | OCaml of string  (* mli, ml, mly, mll *)
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
  | C of string | Cplusplus of string
  | Java | Kotlin | Csharp
  | ObjectiveC of string
  | Swift

  (* advanced script *)
  | Perl | Python | Ruby | Lua | R

  (* other *)
  | Erlang
  | Go | Rust
  | Beta
  | Pascal

  | Web of webpl_type
  | Haxe | Opa | Flash

  | Bytecode of string
  | Asm

  | Thrift

  | MiscPL of string

and config_type =
  | Makefile
  | Json
  | Jsonnet (* kinda pl_type *)
  | Yaml
  | HCL

and lisp_type = CommonLisp | Elisp | Scheme | Clojure

and webpl_type =
  | Php of string (* php or phpt or script *)
  | Hack
  | Js | TypeScript  (* JSX/TSX are converted in Js/Typescript *)
  | Coffee | Vue
  | Css
  | Html | Xml
  | Sql

and media_type =
  | Sound of string
  | Picture of string
  | Video of string

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* this function is used by codemap and archi_parse and called for each
 * filenames, so it has to be fast!
*)
let file_type_of_file2 file =
  let (_d,b,e) = Common2.dbe_of_filename_noext_ok file in
  match e with

  | "ml" | "mli"
  | "mly" | "mll"
  | "dyp" (* dypgen =~ GLR ocamlyacc *)
    -> PL (OCaml e)
  | "mlb" (* mlburg *)
  | "mlp" (* used in some source *)
  | "eliom" (* ocsigen, obviously *)
    -> PL (OCaml e)

  | "sml" -> PL (MLOther e)
  (* fsharp *)
  | "fsi" | "fsx" | "fs"  -> PL (FSharp e)
  (* linear ML *)
  | "lml"  -> PL (MLOther e)

  | "hs" | "lhs" -> PL (Haskell e)
  | "scala" | "sc" -> PL (Scala)

  | "erl" | "hrl" -> PL Erlang

  | "hx" | "hxp" | "hxml" -> PL Haxe
  | "opa" -> PL Opa
  | "sk" -> PL Skip

  | "as" -> PL Flash

  | "bet" -> PL Beta

  (* todo detect false C file, look for "Mode: Objective-C++" string in file ?
   * can also be a c++, use Parser_cplusplus.is_problably_cplusplus_file
  *)
  | "c" -> PL (C e)
  | "h" -> PL (C e)
  (* todo? have a PL of xxx_kind * pl_kind ?  *)
  | "y" | "l" -> PL (C e)

  | "hpp" -> PL (Cplusplus e) | "hxx" -> PL (Cplusplus e)
  | "hh" -> PL (Cplusplus e)
  | "cpp" -> PL (Cplusplus e) | "C" -> PL (Cplusplus e)
  | "cc" -> PL (Cplusplus e)  | "cxx" -> PL (Cplusplus e)
  (* used in libstdc++ *)
  | "tcc" -> PL (Cplusplus e)

  | "m" | "mm" -> PL (ObjectiveC e)
  | "swift" -> PL Swift

  | "java" -> PL Java
  | "kt" -> PL Kotlin
  | "cs" -> PL Csharp

  | "p" -> PL Pascal

  | "thrift" -> PL Thrift

  | "scm" | "rkt" | "ss" | "lsp" -> PL (Lisp Scheme)
  | "lisp" -> PL (Lisp CommonLisp)
  | "el" -> PL (Lisp Elisp)
  | "clj" -> PL (Lisp Clojure)

  (* Perl or Prolog ... I made my choice *)
  | "pl" -> PL (Prolog "pl")
  | "perl" -> PL Perl
  | "py" | "pyi" -> PL Python
  | "rb" -> PL Ruby

  | "logic" -> PL (Prolog "logic") (* datalog of logicblox *)
  | "dtl" -> PL (Prolog "dtl") (* bddbddb *)
  | "dl" -> PL (Prolog "dl") (* datalog *)
  | "ql" | "qll" -> PL (MiscPL e) (* Semmle Query language *)

  | "clp" -> PL (Prolog e)

  | "s" | "S" | "asm" -> PL Asm

  | "c--" -> PL (MiscPL e)
  | "oz" -> PL (MiscPL e)

  | "groovy" -> PL (MiscPL e)

  | "sh" | "rc" | "csh" | "bash" -> PL (Script e)
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

  | "php" | "phpt" -> PL (Web (Php e))
  | "hck" | "hack" (* | "hh" *)  ->
      (* ".hh" is also a popular choice for C++ header files *)
      PL (Web Hack)
  | "css" -> PL (Web Css)
  (* "javascript" | "es" | ? *)
  | "js" -> PL (Web Js)
  | "jsx" -> PL (Web Js) (* Js with JSX enabled *)
  | "coffee" -> PL (Web Coffee)
  | "ts" -> PL (Web TypeScript)
  | "tsx" -> PL (Web TypeScript) (* Typescript with JSX enabled *)
  | "vue" -> PL (Web Vue)
  | "html" | "htm" -> PL (Web Html)
  | "xml" -> PL (Web Xml)
  | "json" -> Config Json
  | "jsonnet" -> Config Jsonnet
  | "yml" | "yaml" -> Config Yaml
  | "tf" -> Config HCL
  | "sql" -> PL (Web Sql)
  | "sqlite" -> PL (Web Sql)

  (* apple stuff ? *)
  | "xib" -> PL (Web Xml)
  (* xml i18n stuff for apple *)
  | "nib" -> Obj e

  (* facebook: sqlshim files *)
  | "sql3" -> PL (Web Sql)
  | "fbobj" -> PL (MiscPL "fbobj")

  | "png" | "jpg" | "JPG" | "gif" | "tiff" -> Media (Picture e)
  | "xcf" | "xpm" -> Media (Picture e)
  | "icns" | "icon" | "ico" -> Media (Picture e)
  | "ppm" -> Media (Picture e)
  | "tga" -> Media (Picture e)
  | "ttf" | "font"  -> Media (Picture e)

  | "wav"  -> Media (Sound e)

  | "swf" -> Media (Picture e)


  | "ps" | "pdf" -> Doc e
  | "ppt" -> Doc e

  | "tex" | "texi" -> Text e
  | "txt" | "doc" -> Text e
  | "nw" | "web" -> Text e
  | "ms" -> Text e

  | "org"
  | "md" | "rest" | "textile" | "wiki" | "rst"
    -> Text e

  | "rtf" -> Text e

  | "cmi" | "cmo" | "cmx" | "cma" | "cmxa"
  | "annot" | "cmt" | "cmti"
  | "o" | "a"
  | "pyc"
  | "log"
  | "toc" | "brf"
  | "out" | "output"
  | "hi"
  | "msi"
    -> Obj e
  (* pad: I use it to store marshalled data *)
  | "db" -> Obj e
  | "po"  | "pot"  | "gmo" -> Obj e
  (* facebook fbcode stuff *)
  | "apcarc"  | "serialized" | "wsdl" | "dat"  | "train" ->  Obj e
  | "facts" -> Obj e (* logicblox *)
  (* pad specific, cached git blame info *)
  | "git_annot" -> Obj e
  (* pad specific, codegraph cached data *)
  | "marshall" | "matrix" -> Obj e

  | "byte" | "top" -> Binary e

  | "tar" -> Archive e
  | "tgz" -> Archive e

  (* was PL Bytecode, but more accurate as an Obj *)
  | "class" -> Obj e
  (* pad specific, clang ast dump *)
  | "clang" | "c.clang2" | "h.clang2" | "clang2" -> Obj e

  (* was Archive *)
  | "jar" -> Archive e

  | "bz2" -> Archive e
  | "gz" -> Archive e
  | "rar" -> Archive e
  | "zip" -> Archive e


  | "exe" -> Binary e
  | "mk" -> Config Makefile

  | "rs" -> PL Rust
  | "go" -> PL Go
  | "lua" -> PL Lua
  | "r" | "R" -> PL R


  | _ when Common2.is_executable file -> Binary e

  | _ when b = "Makefile" || b = "mkfile" || b = "Imakefile" -> Config Makefile
  | _ when b = "README" -> Text "txt"

  | _ when b = "TAGS" -> Binary e
  | _ when b = "TARGETS" -> Config Makefile
  | _ when b = ".depend" -> Obj "depend"
  | _ when b = ".emacs" -> PL (Lisp (Elisp))

  | _ when Common2.filesize file > 300_000 -> Obj e
  | _ -> Other e

let file_type_of_file a =
  Common.profile_code "file_type_of_file" (fun () -> file_type_of_file2 a)

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
  | Config _
    -> true
  | Obj _ | Binary _ | Media _ | Doc _ | Archive _ | Other _ -> false

let webpl_type_of_file file =
  match file_type_of_file file with
  | PL (Web x) -> Some x
  | _ -> None


(*
let detect_pl_of_file file =
  raise Todo

let string_of_pl x =
  raise Todo
  | C -> "c"
  | Cplusplus -> "c++"
  | Java -> "java"

  | Web _ -> raise Todo
*)

let is_syncweb_obj_file file =
  file =~ ".*md5sum_"

let is_json_filename filename =
  filename =~ ".*\\.json$"
  (*
  match File_type.file_type_of_file filename with
  | File_type.PL (File_type.Web (File_type.Json)) -> true
  | _ -> false
  *)

let files_of_dirs_or_files p xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
    p (file_type_of_file filename)
  ) |> Common.sort

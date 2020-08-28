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
  | Media of media_type
  | Archive of string (* tgz, rpm, etc *)
  | Other of string

 (* programming languages *)
 and pl_type = 
  | ML of string  (* mli, ml, mly, mll *)
  | Haskell of string
  | Lisp of lisp_type
  | Skip

  | Prolog of string

  | Makefile
  | Script of string (* sh, csh, awk, sed, etc *)

  | C of string | Cplusplus of string 
  | Java | Csharp
  | ObjectiveC of string 
  | Swift

  | Perl | Python | Ruby | Lua

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

   and lisp_type = CommonLisp | Elisp | Scheme | Clojure

   and webpl_type = 
     | Php of string (* php or phpt or script *)
     | Js | Coffee | TypeScript
     | Css
     | Html | Xml | Json
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
      -> PL (ML e)
  | "mlb" (* mlburg *)
  | "mlp" (* used in some source *)
  | "eliom" (* ocsigen, obviously *)
      -> PL (ML e)

  | "sml" -> PL (ML e)
  (* fsharp *)
  | "fsi" | "fsx" | "fs"  -> PL (ML e)
  (* linear ML *)
  | "lml"  -> PL (ML e)

  | "hs" | "lhs" -> PL (Haskell e)

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
  | "R" | "Rd" -> PL (MiscPL e)

  | "scala" -> PL (MiscPL e)
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
  | "css" -> PL (Web Css)
  (* "javascript" | "es" | ? *)
  | "js" -> PL (Web Js)
  | "coffee" -> PL (Web Coffee)
  | "ts" -> PL (Web TypeScript)
  | "tsx" -> PL (Web TypeScript) (* Typescript with JSX enabled *)
  | "html" | "htm" -> PL (Web Html)
  | "xml" -> PL (Web Xml)
  | "json" -> PL (Web Json)
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
  | "mk" -> PL Makefile

  | "rs" -> PL Rust
  | "go" -> PL Go
  | "lua" -> PL Lua

  | _ when Common2.is_executable file -> Binary e

  | _ when b = "Makefile" || b = "mkfile" || b = "Imakefile" -> PL Makefile
  | _ when b = "README" -> Text "txt"

  | _ when b = "TAGS" -> Binary e
  | _ when b = "TARGETS" -> PL Makefile
  | _ when b = ".depend" -> Obj "depend"
  | _ when b = ".emacs" -> PL (Lisp (Elisp))

  | _ when Common2.filesize file > 300_000 -> Obj e
  | _ -> Other e

let file_type_of_file a = 
  Common.profile_code "file_type_of_file" (fun () -> file_type_of_file2 a)



(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let is_textual_file file =
  match file_type_of_file file with
  (* if this contains weird code then pfff_visual crash *)
  | PL (Web Sql) -> false

  | PL _ 
  | Text _ -> true
  | _ -> false

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


type file_type =
  | PL of pl_type
  | Obj of string
  | Binary of string
  | Text of string
  | Doc of string
  | Media of media_type
  | Archive of string
  | Other of string

 and pl_type = 
  | ML of string | Haskell of string | Lisp of lisp_type | Skip
  | Prolog of string
  | Makefile
  | Script of string
  | C of string | Cplusplus of string | Java | Csharp | ObjectiveC of string 
  | Swift
  | Perl | Python | Ruby | Lua
  | Erlang | Go | Rust
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
     | Php of string
     | Js | Coffee | TypeScript
     | Css
     | Html | Xml | Json
     | Sql

 and media_type =
   | Sound of string
   | Picture of string
   | Video of string


val file_type_of_file: 
  Common.filename -> file_type

val is_textual_file: 
  Common.filename -> bool
val is_syncweb_obj_file:
  Common.filename -> bool
val is_json_filename:
  Common.filename -> bool

(* specialisations *)
val webpl_type_of_file: 
  Common.filename -> webpl_type option

(* val string_of_pl: pl_kind -> string *)

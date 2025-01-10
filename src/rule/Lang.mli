(*
   Type representing the languages for which Semgrep has a dedicated
   parser that uses the generic AST and Semgrep's usual pattern matching.

   File targeting: a Lang.t identifies the rules (extensions, shebangs, ...)
   for filtering files of a certain type.

   File analysis: a Lang.t guarantees that we have a parser for that language.
   Other ways to parse and analyze files are supported by Semgrep (Regex,
   Spacegrep, ...). To represent a way to analyze a file, use Analyzer.t,
   not Lang.t.

   See Rule.language for more on the distinction between target selection and
   target analysis.
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
[@@deriving show, eq, hash, yojson]

(* unsupported_language_message [lang] takes the language as a string and
 * returns an error message.
 *
 * If the language is "unset", it returns a "no language specified error"
 * coupling: Main.lang reference.
 * Otherwise it returns an error with the list of supported languages.
 *)
val unsupported_language_message : string -> string
val of_string_opt : string -> t option

(* may raise Failure unsupported_language_message *)
val of_string : string -> t

(*
   Produce a human-readable representation of the language e.g. "C#"
*)
val to_string : t -> string

(*
   Convert to the most standard and unambiguous representation of the
   language name of form [a-z][a-z0-9]*
   e.g. 'python3' or 'csharp'.

   This is meant to be URL-friendly, filesystem-friendly, and generally
   programmer-friendly.
*)
val to_lowercase_alnum : t -> string

(*
   Convert to the most standard and unambiguous representation of the
   language name of form [A-Z][a-z0-9]*
   e.g. 'Python3' or 'Csharp'.

   This is meant to be URL-friendly, filesystem-friendly, and generally
   programmer-friendly. Furthermore, it matches the type name.
*)
val to_capitalized_alnum : t -> string

(*
   Return a list of extensions for a language such that a file with that
   extension can reasonably be expected to be in that language.

   Dubious extensions like '.php' for Hack files won't be returned by this
   function because most '.php' files actually contain PHP.

   Likewise, executable scripts without an extension are common for many
   languages and we don't return <no extension> as a valid suffix
   for these languages.
*)
val ext_of_lang : t -> string list

(*
   Return a list of extensions that Semgrep will never scan.

   This will be the case even if the extension contains on of ext_of_lang in
   its tail.

   E.g., if excluded_exts_of_lang is [".min.js"], and ext_of_lang is [".js"],
   the file should not be scanned.
*)
val excluded_exts_of_lang : t -> string list

(*
   Return a list of programs that can run a script written in this language.
*)
val shebangs_of_lang : t -> string list

(* See also Find_target.files_of_dirs_or_files *)
val langs_of_filename : Fpath.t -> t list

(* This should be used only in testing code. This may raise
 * Failure "Could not infer a language from the filename".
 * Use langs_of_filename() if you can instead.
 *)
val lang_of_filename_exn : Fpath.t -> t

(* accept any variants *)
val is_js : t -> bool

(* accept any variants *)
val is_proprietary : t -> bool
val lang_map : (string, t) Hashtbl.t

(* Association from a valid name for a language to its unique internal ID. *)
val assoc : (string * t) list

(* list of languages *)
val keys : string list

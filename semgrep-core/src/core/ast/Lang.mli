type t =
  | Python
  (* Python will start with Python3 mode and fall back to Python2 in case
   * of error. Python2 and Python3 are for specific version of Python
   * (no fallback) *)
  | Python2
  | Python3
  (* system *)
  | C
  | Cplusplus
  | Rust
  (* mainstream with Gc *)
  | Javascript
  | Typescript
  | Vue
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
  (* shells *)
  | Bash
  (* data science *)
  | R
  (* config files *)
  | JSON
  | Yaml
  | HCL
  (* doc files *)
  | HTML

(* from deriving eq *)
val pp : Format.formatter -> t -> unit

val show : t -> string

val equal : t -> t -> bool

(* accept any variants *)
val is_js : t -> bool

val is_python : t -> bool

val lang_map : (string, t) Hashtbl.t

val lang_of_string_opt : string -> t option

(* Association from a valid name for a language to its unique internal ID. *)
val assoc : (string * t) list

(* list of languages *)
val keys : string list

(* list of languages comma separated *)
val supported_langs : string

(* See also Find_target.files_of_dirs_or_files *)
val langs_of_filename : Common.filename -> t list

(*
   Produce a human-readable representation of the language e.g. "C#"
*)
val to_string : t -> string

(* legacy alias for 'to_string' *)
val string_of_lang : t -> string

(*
   Convert to the most standard and unambiguous representation of the
   language name of form [a-z][a-z0-9]*
   e.g. 'python3' or 'csharp'.

   This is meant to be URL-friendly, filesystem-friendly, and generally
   programmer-friendly.
*)
val to_lowercase_alnum : t -> string

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

(* unsupported_language_message [lang] takes the language as a string and
 * returns an error message.
 *
 * If the language is "unset", it returns a "no language specified error"
 * coupling: Main.lang reference.
 * Otherwise it returns an error with the list of supported languages.
 *)
val unsupported_language_message : string -> string

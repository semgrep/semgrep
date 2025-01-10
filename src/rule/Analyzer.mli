(*
   The type of a program analyzer. Formerly known as 'Xlang'.

   An analyzer is a tool or component used by Semgrep to analyze a
   project and return findings. An analyzer has an evocative name that
   appears in Semgrep rules and in Semgrep results. An analyzer takes
   parameters whose validity depends from one analyzer to another.

   Examples:
   - The "python" analyzer takes Python patterns combined into
     a search query. It will analyze only project files that look like
     Python files.
   - The "entropy" analyzer takes no search query but
     some options can be specified to override defaults. It will analyze
     any text file. Currently this particular analyzer runs only on
     string literals that are extracted from programs but it could apply
     directly to a whole file secret-key.txt.
   - A "JavaScript in PDF" analyzer that reports JavaScript code
     embedded in PDF documents.

   In most cases, the language analyzer is the name of the programming
   language (Lang.t). These languages are regular Semgrep languages
   using the generic AST. Other analyzers exist, though, and they are
   included in this type.

   In code that references 'Analyzer' a lot, define a local module alias:

     module A = Analyzer

   See also Lang.mli.
*)

type t =
  (* For "real" semgrep.
     The first language is used to parse the pattern and targets.
     The other languages are extra target languages that can use the
     same pattern.

     TODO: enforce the ordering of the list for the derived equality function
     to be correct (could be done by making the type private)
  *)
  | L of Lang.t * Lang.t list
  (* for pattern-regex (referred as 'regex' or 'none' in languages:) *)
  | LRegex
  (* semgrep-like pattern matching *)
  | LSpacegrep
  | LAliengrep
[@@deriving show, eq, hash, yojson]

exception InternalInvalidLanguage of string (* rule id *) * string (* msg *)

val of_lang : Lang.t -> t

(* Returns a lang or an Error with an error message *)
val to_lang : t -> (Lang.t, string) Result.t

(* raises an exception with error message *)
val to_lang_exn : t -> Lang.t

(* Does not raise, but returns empty list for all but the L variant *)
val to_langs : t -> Lang.t list

(* raises an exception with error message *)
val lang_of_opt_analyzer_exn : t option -> Lang.t

(*
   Determine whether a single analyzer exist in an set of
   analyzers. If the first analyzer is more than one language, it's
   an error and the result is always false (it could be an exception but
   this is safer).

   This is normally used for determining if a target that's
   expected to be analyzable with one analyzer can be analyzed with
   the rule's listed analyzers ("languages").

   Examples:
   - 'LRegex' is not compatible with 'L (JavaScript, [])'
   - 'LRegex' is compatible only with 'LRegex'
   - 'L (JavaScript, [])' is compatible with 'L (TypeScript, [JavaScript])'
   - '~require:(L (TypeScript, [JavaScript]))' does not make sense.
*)
val is_compatible : require:t -> provide:t -> bool

(*
   Convert an object that represent a pattern's multiple languages into
   the list of target languages of the form 'L (lang, [])'.
*)
val flatten : t -> t list

(* map from valid extended language names to unique analyzer ID *)
val assoc : (string * t) list

(* efficient map from valid extended language names to unique analyzer ID *)
val map : (string, t) Hashtbl.t

(* list of valid names for extended languages, sorted alphabetically *)
val keys : string list

(* comma-separated list of the supported languages *)
val supported_analyzers : string

(*
   Convert from a string or raise an exception with an error message.
*)
val of_string : ?rule_id:string -> string -> t
val to_string : t -> string
val is_proprietary : t -> bool

(* of_string/to_string for ATD e.g.
   type analyzer = string wrap <ocaml module="Analyzer">
*)
val wrap : string -> t
val unwrap : t -> string

(*
   Produce an extension to be appended to temporary files.
   This helps when debugging.
*)
val informative_suffix : t -> string

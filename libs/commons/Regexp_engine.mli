(*
   Type that holds the original pattern in PCRE syntax as well as its
   compiled form.
*)
type t = SPcre.t

(* Extract the pattern in PCRE syntax *)
val pcre_pattern : t -> string

(* Extract the compiled regexp *)
val pcre_regexp : t -> Pcre.regexp
val show : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit

(* will quote special chars in the string *)
val matching_exact_string : string -> t

(* add the \b around the quoted string *)
val matching_exact_word : string -> t

(* Compile a regexp in PCRE syntax with the given flags. *)
val pcre_compile_with_flags : flags:Pcre.cflag list -> string -> t

(* Compile a regexp in PCRE syntax. *)
val pcre_compile : string -> t

val anchored_match : ?on_error:bool -> t -> string -> bool
(** Match the pattern at the beginning of the string (anchored match)
 * @param on_error is the value to return in case we encounter a PCRE error. *)

val unanchored_match : ?on_error:bool -> t -> string -> bool
(** Match the pattern at any position in the string (unanchored match)
* @param on_error is the value to return in case we encounter a PCRE error. *)

(*
   Hack used for -filter_irrelevant_rules and metavariable-regex.

   We want to translate a regexp meant to match a metavariable into a regexp
   that matches at least in the same spots when applied to the whole
   target file. It may match more but not less.

   Rewrite a pattern to remove assertions that match the beginning or the
   end of strings. This is done to patterns that apply to a substring
   of the target file (the value of a metavariable) because e.g. ^
   matches at the beginning of the substring but not match at the same
   location in the target file.

   The built-in assertions that match the beginning or the end of a string
   are: ^ $ \A \Z \z
   If we're not sure whether these assertions were removed, the result
   is None. For example the pattern '(?:a|^)' requires proper parsing
   and analysis to rewrite it as '(?:a|(?<=\n))', while '[^x]' and
   '[a-z^]' should be left untouched. In those cases where '^' is present but
   not at the beginning or end of the pattern, the result of the function
   will be None.
*)
val remove_end_of_string_assertions : t -> t option

(* for testing *)
val remove_end_of_string_assertions_from_string : string -> string option

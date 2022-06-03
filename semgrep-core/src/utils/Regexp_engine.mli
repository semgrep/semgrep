(*
   Type that holds the original pattern in PCRE syntax as well as its
   compiled form.
*)
type t

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

(* Compile a regexp in PCRE syntax. *)
val pcre_compile : string -> t

(* Match the pattern at the beginning of the string (anchored match) *)
val anchored_match : t -> string -> bool

(* Match the pattern at any position in the string (unanchored match) *)
val unanchored_match : t -> string -> bool

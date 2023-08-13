type t [@@deriving show, eq, ord, hash]
(** A bit-vector encoding id-related flags, see 'AST_generic.id_info'. *)

val empty : t
(** No flags set *)

val make : hidden:bool -> case_insensitive:bool -> t

val is_hidden : t -> bool
(**
     Flag 'hidden' must be set for any artificial identifier that never
     appears in source code but is introduced in the AST after parsing.

     Don't use this for syntax desugaring or transpilation because the
     resulting function name might exist in some source code. Consider the
     following normalization:

       !foo -> foo.contents
                   ^^^^^^^^
                 should not be marked as hidden because it could appear
                 in target source code.

     However, an artificial identifier like "!sh_quoted_expand!" should
     be marked as hidden in bash.

     This allows not breaking the -fast/-filter_irrelevant_rules optimization
     that skips a target file if some identifier in the pattern AST doesn't
     exist in the source of the target.
 *)

val set_hidden : t -> t

val is_case_insensitive : t -> bool
(**
   The case_insensitive flag is indicates that equality of ASTs
   should not care about the case of the characters being used to
   determine if two idents are equal.  This is useful for languages
   like php and apex.
*)

val set_case_insensitive : t -> t
val to_int : t -> int

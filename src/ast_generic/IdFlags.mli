type t [@@deriving show, eq, ord, hash]
(** A bit-vector encoding id-related flags, see 'AST_generic.id_info'. *)

val empty : t
(** No flags set *)

val make :
  hidden:bool -> case_insensitive:bool -> final:bool -> static:bool -> t

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

val is_final : t -> bool
(** Flag 'is_final' is used within variable definitions/assignments to record
    whether the variable being defined is "effectively final" (as in Java).
    This is used by e.g. taint analysis to propagate taint via globals and
    private class attributes. *)

val set_final : t -> t

val is_static : t -> bool
(** Flag 'is_static' indicates that the identifier is "static",
    e.g. to mark a static class field/variable. *)

val set_static : t -> t

val union : t -> t -> t
(** Union two sets of flags. *)

val to_int : t -> int
(** Get the underlying representation of a set of flags. *)

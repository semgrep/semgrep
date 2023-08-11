type t [@@deriving show, eq, ord, hash]
(** A bit-vector encoding id-related flags, see 'AST_generic.id_info'. *)

val empty : t
(** No flags set *)

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
val to_int : t -> int

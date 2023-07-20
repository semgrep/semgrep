(* Location extraction for different AST_dockerfile constructs *)

val docker_string_fragment_loc :
  AST_dockerfile.docker_string_fragment -> Tok_range.t

val docker_string_loc : AST_dockerfile.docker_string -> Tok_range.t
val instruction_loc : AST_dockerfile.instruction -> Tok_range.t
val argv_or_shell_loc : AST_dockerfile.argv_or_shell -> Tok_range.t
val param_loc : AST_dockerfile.param -> Tok_range.t
val image_spec_loc : AST_dockerfile.image_spec -> Tok_range.t
val label_pair_loc : AST_dockerfile.label_pair -> Tok_range.t
val expose_port_loc : AST_dockerfile.expose_port -> Tok_range.t
val str_or_ellipsis_loc : AST_dockerfile.str_or_ellipsis -> Tok_range.t
val array_or_paths_loc : AST_dockerfile.array_or_paths -> Tok_range.t
val healthcheck_loc : AST_dockerfile.healthcheck -> Tok_range.t
val wrap_loc : 'a * Tok.t -> Tok_range.t

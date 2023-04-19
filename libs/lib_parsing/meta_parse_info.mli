val vof_info : Tok.t -> OCaml.v

type dumper_precision = {
  full_info : bool;
  token_info : bool;
  type_info : bool;
}

val default_dumper_precision : dumper_precision
val _current_precision : dumper_precision ref
val vof_info_adjustable_precision : Tok.t -> OCaml.v
val cmdline_flags_precision : unit -> Arg_helpers.flag_spec list

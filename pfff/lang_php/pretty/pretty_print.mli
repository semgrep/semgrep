
(* used by prettyphp *)
val program:
  (string -> unit) -> Ast_pp.program -> unit

(* used by test *)
val program_env:
  Pretty_print_code.env -> Ast_pp.program -> unit

(* used by spatch *)
val stmts:
  Pretty_print_code.env -> Ast_pp.stmt list -> unit

val class_elements:
  Pretty_print_code.env -> Ast_pp.class_element list -> unit

val class_header:
  Pretty_print_code.env -> Ast_pp.stmt list -> unit

val class_footer:
  Pretty_print_code.env -> unit -> unit

(* used by xhpize *)
val stmt:
  Pretty_print_code.env -> Ast_pp.stmt -> unit

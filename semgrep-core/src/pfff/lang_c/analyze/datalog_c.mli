type fact = Datalog_code.fact

type env = {
  scope : string;
  c_file_readable : Common.filename;
  long_format : bool;
  globals : Graph_code.t;
  globals_renames : Ast_c.name -> Ast_c.name;
  locals : (string * Ast_c.type_ option) list ref;
  facts : fact list ref;
}

val instrs_of_expr : env -> Ast_c.expr -> Ast_cil.instr list
val long_format : bool ref
val facts_of_instr : env -> Ast_cil.instr -> fact list
val facts_of_def : env -> Ast_c.toplevel -> fact list
val return_fact : env -> Ast_cil.instr -> fact

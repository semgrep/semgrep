(*s: xdebug.mli *)

type kind_call = 
    | FunCall of string
    | ObjectCall of string (* class *) * string (* method *)
    | ClassCall of string (* module *) * string

val s_of_kind_call: kind_call -> string

type call_trace = {
  f_call: kind_call;
  f_file: Common.filename;
  f_line: int;
  f_params: Cst_php.expr list;
  f_return: Cst_php.expr option;

  (* f_type: *)
}
val string_of_call_trace: call_trace -> string

type config = {
  auto_trace: int;
  trace_options: int;

  trace_format: int;
  collect_params: params_mode;
  collect_return: bool;

  var_display_max_children: int;
  val_display_max_data: int;
  var_display_max_depth: int;
}
 and params_mode = 
   | NoParam
   | TypeAndArity
   | TypeAndArityAndTooltip
   | FullParam
   | FullParamAndVar

val default_config: config

val php_has_xdebug_extension: unit -> bool

val iter_dumpfile: 
  ?config:config ->
  ?show_progress:bool -> 
  ?fatal_when_exn:bool ->
  (call_trace -> unit) -> Common.filename -> unit

val xdebug_main_name: string

val php_cmd_with_xdebug_on: 
  ?config:config ->
  trace_file:Common.filename -> unit -> 
  string
(*x: xdebug.mli *)
(*e: xdebug.mli *)

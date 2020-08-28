
val globals_builtins: string list

type env = { 
  globals: 
    (string, string) Hashtbl.t;
  global_arrays: 
    (string, (string, string) Hashtbl.t) Hashtbl.t;

  constants:
    (string, string) Hashtbl.t;

  (* hook *)
  globals_specials: 
    string -> Common.filename -> string option;
}

val mk_env: php_root:Common.dirname -> env

val hbad_functions: string Common.hashset
val hdynamic_call_wrappers: string Common.hashset

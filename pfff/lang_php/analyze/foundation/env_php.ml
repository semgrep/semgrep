(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * In PHP many globals are defined in a php.ini file and can be accessed in 
 * the program, e.g. PHP_ROOT. It is useful for some static analysis
 * to know the value of those globals, hence this file.
 * 
 * related: builtins_php.ml that take care of not global variables but
 * builtin functions (and extensions)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo? could have more general fields for globals containing integers, 
 * or complex objects. But for now I use this mostly in include_require_php.ml
 * to know paths, so string hash seems good enough.
 *)
type env = { 
  globals: 
    (string, string) Hashtbl.t;
  global_arrays: 
    (string, (string, string) Hashtbl.t) Hashtbl.t;

  constants:
    (string, string) Hashtbl.t;

  (* hook. Sometimes some variables like BASE_PATH are not really globals as
   * different files give different values to it. Hence this hook that 
   * given some variable and a dir can try to infer what is the value
   * for this variable in the context of the dir.
   *)
  globals_specials: 
    string -> Common.dirname -> string option;
}


let mk_env ~php_root = {
  globals = Common.hash_of_list [
  ];
  global_arrays = Common.hash_of_list [
    "_SERVER", Common.hash_of_list [
      "PHP_ROOT", php_root;
    ];
    "GLOBALS", Common.hash_of_list [
        (* the relevant code should use _SERVER['PHP_ROOT'] *)
        "PHP_ROOT", php_root;
    ];
  ];
  constants = Common.hash_of_list [
  ];
  globals_specials = (fun _s _dir -> 
    None
  );
}

(* src: http://www.php.net/manual/en/reserved.variables.php
 *  and http://php.net/manual/en/language.variables.superglobals.php 
 *)
let globals_builtins = [
  "GLOBALS";

  "_SERVER";
  "_GET";
  "_POST";
  "_FILES";
  "_SESSION";
  "_COOKIE";
  "_SESSION";
  "_REQUEST";
  "_ENV";

  "php_errormsg";
  "HTTP_RAW_POST_DATA";
  "http_response_header";
  "argc";
  "argv";
]

(* todo: facebook specific. Also would be good to associate a message with *)
let hbad_functions = Common.hashset_of_list [
  "HTML";
  "curl_exec";
  "debug_rlog";
]

let hdynamic_call_wrappers = Common.hashset_of_list [
  "call_user_func";
  "call_user_func_array";
  (* facebook specific *)
  "fb_call_user_func_safe";
  "fb_call_user_func_safe_return";
]

(* Arg wrappers and small extension to Arg for "actions". See pfff's Main.ml for
 * an example of use.
 *
 * DEPRECATED: this module is deprecated, you should use the
 * Cmdliner library instead.
 *)

type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list
type options_with_title = string * string * arg_spec_full list
type cmdline_sections = options_with_title list

(* A wrapper around Arg modules that have more logical argument order,
 * and returns the remaining args.
 *)
val parse_options :
  cmdline_options -> Arg.usage_msg -> string array -> string list

(* Another wrapper that does Arg.align automatically *)
val usage : Arg.usage_msg -> cmdline_options -> unit

(* Work with the options_with_title type way to organize a long
 * list of command line switches.
 *)
val short_usage : Arg.usage_msg -> short_opt:cmdline_options -> unit

val long_usage :
  Arg.usage_msg ->
  short_opt:cmdline_options ->
  long_opt:cmdline_sections ->
  unit

(* With the options_with_title way, we don't want the default -help and --help
 * so need adapter of Arg module, not just wrapper.
 *)
val arg_align2 : cmdline_options -> cmdline_options

val arg_parse2 :
  cmdline_options ->
  Arg.usage_msg ->
  (unit -> unit) ->
  (* short_usage func *)
  string list

(* The action lib. Useful to debug subpart of your system. See some of
 * my Main.ml for example of use. *)
type flag_spec = Arg.key * Arg.spec * Arg.doc

type action_spec = Arg.key * Arg.doc * action_func
and action_func = string list -> unit

type cmdline_actions = action_spec list

exception WrongNumberOfArguments

val mk_action_0_arg : (unit -> unit) -> action_func
val mk_action_1_arg : (string -> unit) -> action_func
val mk_action_2_arg : (string -> string -> unit) -> action_func
val mk_action_3_arg : (string -> string -> string -> unit) -> action_func

val mk_action_4_arg :
  (string -> string -> string -> string -> unit) -> action_func

val mk_action_n_arg : (string list -> unit) -> action_func

val options_of_actions :
  string ref (* the action ref *) -> cmdline_actions -> cmdline_options

val do_action : Arg.key -> string list (* args *) -> cmdline_actions -> unit
val action_list : cmdline_actions -> Arg.key list

(*
   Wrappers around mk_action_1_arg and mk_action_n_arg
   that can handle the conversion from string to Fpath.t or some other type:

     mk_action_1_conv Fpath.v do_something_with_the_file
*)
val mk_action_1_conv : (string -> 'a) -> ('a -> unit) -> action_func
val mk_action_n_conv : (string -> 'a) -> ('a list -> unit) -> action_func

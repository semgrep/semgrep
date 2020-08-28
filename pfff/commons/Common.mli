(*s: pfff/commons/Common.mli *)

(*s: signature equality operators *)
val (=|=) : int    -> int    -> bool
val (=<=) : char   -> char   -> bool
val (=$=) : string -> string -> bool
val (=:=) : bool   -> bool   -> bool
(*e: signature equality operators *)

(*s: signature [[Common.TODOOPERATOR (pfff/commons/Common.mli)4]] *)
val (=*=): 'a -> 'a -> bool
(*e: signature [[Common.TODOOPERATOR (pfff/commons/Common.mli)4]] *)

(*s: signature [[Common.pr]] *)
val pr : string -> unit
(*e: signature [[Common.pr]] *)
(*s: signature [[Common.pr2]] *)
val pr2 : string -> unit
(*e: signature [[Common.pr2]] *)

(*s: signature [[Common._already_printed]] *)
(* forbid pr2_once to do the once "optimisation" *)
val _already_printed : (string, bool) Hashtbl.t
(*e: signature [[Common._already_printed]] *)
(*s: signature [[Common.disable_pr2_once]] *)
val disable_pr2_once : bool ref
(*e: signature [[Common.disable_pr2_once]] *)
(*s: signature [[Common.pr2_once]] *)
val pr2_once : string -> unit
(*e: signature [[Common.pr2_once]] *)

(*s: signature [[Common.pr2_gen]] *)
val pr2_gen: 'a -> unit
(*e: signature [[Common.pr2_gen]] *)
(*s: signature [[Common.dump]] *)
val dump: 'a -> string
(*e: signature [[Common.dump]] *)

(*s: exception [[Common.Todo]] *)
exception Todo
(*e: exception [[Common.Todo]] *)
(*s: exception [[Common.Impossible]] *)
exception Impossible
(*e: exception [[Common.Impossible]] *)

(*s: exception [[Common.Multi_found]] *)
exception Multi_found
(*e: exception [[Common.Multi_found]] *)

(*s: signature [[Common.exn_to_s]] *)
val exn_to_s : exn -> string
(*e: signature [[Common.exn_to_s]] *)

(*s: signature [[Common.i_to_s]] *)
val i_to_s : int -> string
(*e: signature [[Common.i_to_s]] *)
(*s: signature [[Common.s_to_i]] *)
val s_to_i : string -> int
(*e: signature [[Common.s_to_i]] *)

(*s: signature [[Common.null_string]] *)
val null_string : string -> bool
(*e: signature [[Common.null_string]] *)

(*s: signature match operator *)
val (=~) : string -> string -> bool
(*e: signature match operator *)
(*s: signature matched functions *)
val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 : string -> string * string * string * string * string * string
val matched7 : string -> string * string * string * string * string * string * string
(*e: signature matched functions *)

(*s: signature [[Common.spf]] *)
val spf : ('a, unit, string) format -> 'a
(*e: signature [[Common.spf]] *)

(*s: signature [[Common.join]] *)
val join : string (* sep *) -> string list -> string
(*e: signature [[Common.join]] *)
(*s: signature [[Common.split]] *)
val split : string (* sep regexp *) -> string -> string list
(*e: signature [[Common.split]] *)

(*s: type [[Common.filename]] *)
type filename = string
(*e: type [[Common.filename]] *)
(*s: type [[Common.dirname]] *)
type dirname = string
(*e: type [[Common.dirname]] *)
(*s: type [[Common.path]] *)
type path = string
(*e: type [[Common.path]] *)

(*s: signature [[Common.cat]] *)
val cat :      filename -> string list
(*e: signature [[Common.cat]] *)

(*s: signature [[Common.write_file]] *)
val write_file : file:filename -> string -> unit
(*e: signature [[Common.write_file]] *)
(*s: signature [[Common.read_file]] *)
val read_file : filename -> string
(*e: signature [[Common.read_file]] *)

(*s: signature [[Common.with_open_outfile]] *)
val with_open_outfile : 
  filename -> ((string -> unit) * out_channel -> 'a) -> 'a
(*e: signature [[Common.with_open_outfile]] *)
(*s: signature [[Common.with_open_infile]] *)
val with_open_infile : 
  filename -> (in_channel -> 'a) -> 'a
(*e: signature [[Common.with_open_infile]] *)

(*s: exception [[Common.CmdError]] *)
exception CmdError of Unix.process_status * string
(*e: exception [[Common.CmdError]] *)
(*s: signature [[Common.command2]] *)
val command2 : string -> unit
(*e: signature [[Common.command2]] *)
(*s: signature [[Common.cmd_to_list]] *)
val cmd_to_list :  ?verbose:bool -> string -> string list (* alias *)
(*e: signature [[Common.cmd_to_list]] *)
(*s: signature [[Common.cmd_to_list_and_status]] *)
val cmd_to_list_and_status:
  ?verbose:bool -> string -> string list * Unix.process_status
(*e: signature [[Common.cmd_to_list_and_status]] *)

(*s: signature [[Common.null]] *)
val null : 'a list -> bool
(*e: signature [[Common.null]] *)
(*s: signature [[Common.exclude]] *)
val exclude : ('a -> bool) -> 'a list -> 'a list
(*e: signature [[Common.exclude]] *)
(*s: signature [[Common.sort]] *)
val sort : 'a list -> 'a list
(*e: signature [[Common.sort]] *)

(*s: signature [[Common.map_filter]] *)
val map_filter : ('a -> 'b option) -> 'a list -> 'b list
(*e: signature [[Common.map_filter]] *)
(*s: signature [[Common.find_opt]] *)
val find_opt: ('a -> bool) -> 'a list -> 'a option
(*e: signature [[Common.find_opt]] *)
(*s: signature [[Common.find_some]] *)
val find_some : ('a -> 'b option) -> 'a list -> 'b
(*e: signature [[Common.find_some]] *)
(*s: signature [[Common.find_some_opt]] *)
val find_some_opt : ('a -> 'b option) -> 'a list -> 'b option
(*e: signature [[Common.find_some_opt]] *)
(*s: signature [[Common.filter_some]] *)
val filter_some: 'a option list -> 'a list
(*e: signature [[Common.filter_some]] *)

(*s: signature [[Common.take]] *)
val take : int -> 'a list -> 'a list
(*e: signature [[Common.take]] *)
(*s: signature [[Common.take_safe]] *)
val take_safe : int -> 'a list -> 'a list
(*e: signature [[Common.take_safe]] *)
(*s: signature [[Common.drop]] *)
val drop : int -> 'a list -> 'a list
(*e: signature [[Common.drop]] *)
(*s: signature [[Common.span]] *)
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
(*e: signature [[Common.span]] *)

(*s: signature [[Common.index_list]] *)
val index_list   : 'a list -> ('a * int) list
(*e: signature [[Common.index_list]] *)
(*s: signature [[Common.index_list_0]] *)
val index_list_0 : 'a list -> ('a * int) list
(*e: signature [[Common.index_list_0]] *)
(*s: signature [[Common.index_list_1]] *)
val index_list_1 : 'a list -> ('a * int) list
(*e: signature [[Common.index_list_1]] *)

(*s: type [[Common.assoc]] *)
type ('a, 'b) assoc = ('a * 'b) list
(*e: type [[Common.assoc]] *)

(*s: signature [[Common.sort_by_val_lowfirst]] *)
val sort_by_val_lowfirst: ('a,'b) assoc -> ('a * 'b) list
(*e: signature [[Common.sort_by_val_lowfirst]] *)
(*s: signature [[Common.sort_by_val_highfirst]] *)
val sort_by_val_highfirst: ('a,'b) assoc -> ('a * 'b) list
(*e: signature [[Common.sort_by_val_highfirst]] *)

(*s: signature [[Common.sort_by_key_lowfirst]] *)
val sort_by_key_lowfirst: ('a,'b) assoc -> ('a * 'b) list
(*e: signature [[Common.sort_by_key_lowfirst]] *)
(*s: signature [[Common.sort_by_key_highfirst]] *)
val sort_by_key_highfirst: ('a,'b) assoc -> ('a * 'b) list
(*e: signature [[Common.sort_by_key_highfirst]] *)

(*s: signature [[Common.group_by]] *)
val group_by: ('a -> 'b) -> 'a list -> ('b * 'a list) list
(*e: signature [[Common.group_by]] *)
(*s: signature [[Common.group_assoc_bykey_eff]] *)
val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list
(*e: signature [[Common.group_assoc_bykey_eff]] *)
(*s: signature [[Common.group_by_mapped_key]] *)
val group_by_mapped_key: ('a -> 'b) -> 'a list -> ('b * 'a list) list
(*e: signature [[Common.group_by_mapped_key]] *)
(*s: signature [[Common.group_by_multi]] *)
val group_by_multi: ('a -> 'b list) -> 'a list -> ('b * 'a list) list
(*e: signature [[Common.group_by_multi]] *)

(*s: type [[Common.stack]] *)
type 'a stack = 'a list
(*e: type [[Common.stack]] *)
(*s: signature [[Common.push]] *)
val push : 'a -> 'a stack ref -> unit
(*e: signature [[Common.push]] *)

(*s: signature [[Common.hash_of_list]] *)
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
(*e: signature [[Common.hash_of_list]] *)
(*s: signature [[Common.hash_to_list]] *)
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
(*e: signature [[Common.hash_to_list]] *)

(*s: type [[Common.hashset]] *)
type 'a hashset = ('a, bool) Hashtbl.t 
(*e: type [[Common.hashset]] *)
(*s: signature [[Common.hashset_of_list]] *)
val hashset_of_list : 'a list -> 'a hashset
(*e: signature [[Common.hashset_of_list]] *)
(*s: signature [[Common.hashset_to_list]] *)
val hashset_to_list : 'a hashset -> 'a list
(*e: signature [[Common.hashset_to_list]] *)

(*s: signature [[Common.map_opt]] *)
val map_opt: ('a -> 'b) -> 'a option -> 'b option
(*e: signature [[Common.map_opt]] *)
(*s: signature [[Common.opt]] *)
val opt: ('a -> unit) -> 'a option -> unit
(*e: signature [[Common.opt]] *)
(*s: signature [[Common.do_option]] *)
val do_option : ('a -> unit) -> 'a option -> unit
(*e: signature [[Common.do_option]] *)
(*s: signature [[Common.opt_to_list]] *)
val opt_to_list: 'a option -> 'a list
(*e: signature [[Common.opt_to_list]] *)
(*s: signature [[Common.TODOOPERATOR (pfff/commons/Common.mli)6]] *)
val (>>=): 'a option -> ('a -> 'b option) -> 'b option
(*e: signature [[Common.TODOOPERATOR (pfff/commons/Common.mli)6]] *)
(*s: signature [[Common.TODOOPERATOR (pfff/commons/Common.mli)7]] *)
val (|||): 'a option -> 'a -> 'a
(*e: signature [[Common.TODOOPERATOR (pfff/commons/Common.mli)7]] *)


(*s: type [[Common.either]] *)
type ('a, 'b) either = Left of 'a | Right of 'b
(*e: type [[Common.either]] *)
val pp_either: (Format.formatter -> 'a -> 'b) ->
               (Format.formatter -> 'c -> 'd) ->
               Format.formatter -> ('a, 'c) either -> unit
(*s: type [[Common.either3]] *)
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
(*e: type [[Common.either3]] *)
val pp_either3: (Format.formatter -> 'a -> 'b) ->
               (Format.formatter -> 'c -> 'd) ->
               (Format.formatter -> 'e -> 'f) ->
               Format.formatter -> ('a, 'c, 'e) either3 -> unit
(*s: signature [[Common.partition_either]] *)
val partition_either :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
(*e: signature [[Common.partition_either]] *)
(*s: signature [[Common.partition_either3]] *)
val partition_either3 :
    ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list
(*e: signature [[Common.partition_either3]] *)



(*s: type [[Common.arg_spec_full]] *)
type arg_spec_full = Arg.key * Arg.spec * Arg.doc
(*e: type [[Common.arg_spec_full]] *)
(*s: type [[Common.cmdline_options]] *)
type cmdline_options = arg_spec_full list
(*e: type [[Common.cmdline_options]] *)

(*s: type [[Common.options_with_title]] *)
type options_with_title = string * string * arg_spec_full list
(*e: type [[Common.options_with_title]] *)
(*s: type [[Common.cmdline_sections]] *)
type cmdline_sections = options_with_title list
(*e: type [[Common.cmdline_sections]] *)

(*s: signature [[Common.parse_options]] *)
(* A wrapper around Arg modules that have more logical argument order, 
 * and returns the remaining args.
 *)
val parse_options : 
  cmdline_options -> Arg.usage_msg -> string array -> string list
(* Another wrapper that does Arg.align automatically *)
(*e: signature [[Common.parse_options]] *)
(*s: signature [[Common.usage]] *)
(* Another wrapper that does Arg.align automatically *)
val usage : Arg.usage_msg -> cmdline_options -> unit
(*e: signature [[Common.usage]] *)

(*s: signature [[Common.short_usage]] *)
(* Work with the options_with_title type way to organize a long
 * list of command line switches.
 *)
val short_usage : 
  Arg.usage_msg -> short_opt:cmdline_options -> unit
(*e: signature [[Common.short_usage]] *)
(*s: signature [[Common.long_usage]] *)
val long_usage : 
  Arg.usage_msg -> short_opt:cmdline_options -> long_opt:cmdline_sections -> 
  unit
(*e: signature [[Common.long_usage]] *)

(*s: signature [[Common.arg_align2]] *)
(* With the options_with_title way, we don't want the default -help and --help
 * so need adapter of Arg module, not just wrapper.
 *)
val arg_align2 : cmdline_options -> cmdline_options
(*e: signature [[Common.arg_align2]] *)
(*s: signature [[Common.arg_parse2]] *)
val arg_parse2 : 
  cmdline_options -> Arg.usage_msg -> (unit -> unit) (* short_usage func *) -> 
  string list
(*e: signature [[Common.arg_parse2]] *)

(*s: type [[Common.flag_spec]] *)
(* The action lib. Useful to debug supart of your system. cf some of
 * my Main.ml for example of use. *)
type flag_spec   = Arg.key * Arg.spec * Arg.doc
(*e: type [[Common.flag_spec]] *)
(*s: type [[Common.action_spec]] *)
type action_spec = Arg.key * Arg.doc * action_func 
(*e: type [[Common.action_spec]] *)
(*s: type [[Common.action_func]] *)
   and action_func = (string list -> unit)
(*e: type [[Common.action_func]] *)

(*s: type [[Common.cmdline_actions]] *)
type cmdline_actions = action_spec list
(*e: type [[Common.cmdline_actions]] *)
(*s: exception [[Common.WrongNumberOfArguments]] *)
exception WrongNumberOfArguments
(*e: exception [[Common.WrongNumberOfArguments]] *)

(*s: signature [[Common.mk_action_0_arg]] *)
val mk_action_0_arg : (unit -> unit)                       -> action_func
(*e: signature [[Common.mk_action_0_arg]] *)
(*s: signature [[Common.mk_action_1_arg]] *)
val mk_action_1_arg : (string -> unit)                     -> action_func
(*e: signature [[Common.mk_action_1_arg]] *)
(*s: signature [[Common.mk_action_2_arg]] *)
val mk_action_2_arg : (string -> string -> unit)           -> action_func
(*e: signature [[Common.mk_action_2_arg]] *)
(*s: signature [[Common.mk_action_3_arg]] *)
val mk_action_3_arg : (string -> string -> string -> unit) -> action_func
(*e: signature [[Common.mk_action_3_arg]] *)
(*s: signature [[Common.mk_action_4_arg]] *)
val mk_action_4_arg : (string -> string -> string -> string -> unit) -> 
  action_func
(*e: signature [[Common.mk_action_4_arg]] *)

(*s: signature [[Common.mk_action_n_arg]] *)
val mk_action_n_arg : (string list -> unit) -> action_func
(*e: signature [[Common.mk_action_n_arg]] *)

(*s: signature [[Common.options_of_actions]] *)
val options_of_actions: 
  string ref (* the action ref *) -> cmdline_actions -> cmdline_options
(*e: signature [[Common.options_of_actions]] *)
(*s: signature [[Common.do_action]] *)
val do_action: 
  Arg.key -> string list (* args *) -> cmdline_actions -> unit
(*e: signature [[Common.do_action]] *)
(*s: signature [[Common.action_list]] *)
val action_list: 
  cmdline_actions -> Arg.key list
(*e: signature [[Common.action_list]] *)


(*s: signature [[Common.debugger]] *)
(* if set then will not do certain finalize so faster to go back in replay *)
val debugger : bool ref
(*e: signature [[Common.debugger]] *)

(*s: signature [[Common.unwind_protect]] *)
(* emacs spirit *)
val unwind_protect : (unit -> 'a) -> (exn -> 'b) -> 'a
(* java spirit *)
(*e: signature [[Common.unwind_protect]] *)
(*s: signature [[Common.finalize]] *)
(* java spirit *)
val finalize :       (unit -> 'a) -> (unit -> 'b) -> 'a
(*e: signature [[Common.finalize]] *)

(*s: signature [[Common.save_excursion]] *)
val save_excursion : 'a ref -> 'a -> (unit -> 'b) -> 'b
(*e: signature [[Common.save_excursion]] *)

(*s: signature [[Common.memoized]] *)
val memoized : 
  ?use_cache:bool -> ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b
(*e: signature [[Common.memoized]] *)

(*s: exception [[Common.UnixExit]] *)
exception UnixExit of int 
(*e: exception [[Common.UnixExit]] *)

(*s: exception [[Common.Timeout]] *)
exception Timeout
(*e: exception [[Common.Timeout]] *)
(*s: signature [[Common.timeout_function]] *)
val timeout_function :
  ?verbose:bool ->
  int -> (unit -> 'a) -> 'a
(*e: signature [[Common.timeout_function]] *)

(*s: type [[Common.prof]] *)
type prof = ProfAll | ProfNone | ProfSome of string list
(*e: type [[Common.prof]] *)
(*s: signature [[Common.profile]] *)
val profile : prof ref
(*e: signature [[Common.profile]] *)
(*s: signature [[Common.show_trace_profile]] *)
val show_trace_profile : bool ref
(*e: signature [[Common.show_trace_profile]] *)

(*s: signature [[Common._profile_table]] *)
val _profile_table : (string, (float ref * int ref)) Hashtbl.t ref
(*e: signature [[Common._profile_table]] *)
(*s: signature [[Common.profile_code]] *)
val profile_code : string -> (unit -> 'a) -> 'a
(*e: signature [[Common.profile_code]] *)
(*s: signature [[Common.profile_diagnostic]] *)
val profile_diagnostic : unit -> string
(*e: signature [[Common.profile_diagnostic]] *)
(*s: signature [[Common.profile_code_exclusif]] *)
val profile_code_exclusif : string -> (unit -> 'a) -> 'a
(*e: signature [[Common.profile_code_exclusif]] *)
(*s: signature [[Common.profile_code_inside_exclusif_ok]] *)
val profile_code_inside_exclusif_ok : string -> (unit -> 'a) -> 'a
(*e: signature [[Common.profile_code_inside_exclusif_ok]] *)
(*s: signature [[Common.report_if_take_time]] *)
val report_if_take_time : int -> string -> (unit -> 'a) -> 'a
(* similar to profile_code but print some information during execution too *)
(*e: signature [[Common.report_if_take_time]] *)
(*s: signature [[Common.profile_code2]] *)
(* similar to profile_code but print some information during execution too *)
val profile_code2 : string -> (unit -> 'a) -> 'a
(*e: signature [[Common.profile_code2]] *)

(*s: signature [[Common._temp_files_created]] *)
(* creation of /tmp files, a la gcc 
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" 
 *)
val _temp_files_created : string list ref
(*e: signature [[Common._temp_files_created]] *)
(*s: signature [[Common.save_tmp_files]] *)
val save_tmp_files : bool ref
(*e: signature [[Common.save_tmp_files]] *)
(*s: signature [[Common.new_temp_file]] *)
val new_temp_file : string (* prefix *) -> string (* suffix *) -> filename
(*e: signature [[Common.new_temp_file]] *)
(*s: signature [[Common.erase_temp_files]] *)
val erase_temp_files : unit -> unit
(*e: signature [[Common.erase_temp_files]] *)
(*s: signature [[Common.erase_this_temp_file]] *)
val erase_this_temp_file : filename -> unit
(*e: signature [[Common.erase_this_temp_file]] *)

(*s: signature [[Common.fullpath]] *)
(* val realpath: filename -> filename *)
val fullpath: filename -> filename
(*e: signature [[Common.fullpath]] *)

(*s: signature [[Common.cache_computation]] *)
val cache_computation : 
  ?verbose:bool -> ?use_cache:bool -> filename  -> string (* extension *) -> 
  (unit -> 'a) -> 'a
(*e: signature [[Common.cache_computation]] *)

(*s: signature [[Common.filename_without_leading_path]] *)
val filename_without_leading_path : string -> filename -> filename
(*e: signature [[Common.filename_without_leading_path]] *)
(*s: signature [[Common.readable]] *)
val readable: root:string -> filename -> filename
(*e: signature [[Common.readable]] *)

(*s: signature [[Common.follow_symlinks]] *)
val follow_symlinks: bool ref
(*e: signature [[Common.follow_symlinks]] *)
(*s: signature [[Common.files_of_dir_or_files_no_vcs_nofilter]] *)
val files_of_dir_or_files_no_vcs_nofilter:
 string list -> filename list
(*e: signature [[Common.files_of_dir_or_files_no_vcs_nofilter]] *)

(*s: signature [[Common.main_boilerplate]] *)
(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit
(*e: signature [[Common.main_boilerplate]] *)

(* type of maps from string to `a *)
module SMap : Map.S with type key = String.t
(*s: type [[Common.smap]] *)
type 'a smap = 'a SMap.t
(*e: type [[Common.smap]] *)

(*e: pfff/commons/Common.mli *)

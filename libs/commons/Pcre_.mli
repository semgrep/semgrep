[@@@ocaml.deprecated
"Pcre_ is deprecated in favour of Pcre2_. Pcre_ should only be used in \
 existing generic mode code."]
(*
   Wrappers for using the Pcre module safely with settings that make
   sense for semgrep such as automatically setting some flags and
   handling exceptions.

   If you need a function from Pcre that is not being exposed by this module,
   please add it.
*)

(*
   The type holding the source pattern and a compiled regexp.

   Note that the default 'equal' function is based only on the source
   patterns and doesn't take into account compilation options.
*)
type t = private { pattern : string; regexp : Pcre.regexp }
[@@deriving show, eq]

(*
  val show : Pcre.error -> string
*)
type error = Pcre.error =
  | Partial
  | BadPartial
  | BadPattern of string * int
  | BadUTF8
  | BadUTF8Offset
  | MatchLimit
  | RecursionLimit
  | WorkspaceSize
  | InternalError of string
[@@deriving show]

(*
   To be used instead of Pcre.regexp. Refer to the Pcre documentation
   for usage.
   https://mmottl.github.io/pcre-ocaml/api/pcre/Pcre/index.html#val-regexp

   This takes care of setting PCRE_EXTRA_MATCH_LIMIT and
   PCRE_EXTRA_MATCH_LIMIT_RECURSION to the same value across platforms.

   Any flags needed to make things work with UTF-8 are passed automatically.
*)
val regexp :
  ?study:bool ->
  ?iflags:Pcre.icflag ->
  ?flags:Pcre.cflag list ->
  ?chtables:Pcre.chtables ->
  string ->
  t

(*
   Same as Pcre.pmatch but makes errors explicit.
   Option '?pat' was removed so as to force the use of our modified 'regexp'
   function. TODO: add it back for convenience.
*)
val pmatch :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  string ->
  (bool, Pcre.error) result

(* Return 'on_error' in case of a PCRE error. The error is logged. *)
val pmatch_noerr :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  ?on_error:bool ->
  string ->
  bool

(*
   See notes about 'pmatch'.
   Additionally, exception 'Not_found' is converted to a 'None' value.
*)
val exec :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  string ->
  (Pcre.substrings option, Pcre.error) result

(* Return 'None' in case of a PCRE error. The error is logged. *)
val exec_noerr :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  string ->
  Pcre.substrings option

(*
   See notes about 'pmatch'.
   Additionally, exception 'Not_found' is converted to the empty array.
*)
val exec_all :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  string ->
  (Pcre.substrings array, Pcre.error) result

(* Return all captured subgroups as strings.
   This is useful for debugging in utop. *)
val exec_to_strings :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  string ->
  (string array array, Pcre.error) result

(* Return '[| |]' in case of a PCRE error. The error is logged. *)
val exec_all_noerr :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  string ->
  Pcre.substrings array

(* See notes about 'pmatch'. *)
val split :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?max:int ->
  ?callout:Pcre.callout ->
  string ->
  (string list, Pcre.error) result

(* See notes about 'pmatch'. *)
val full_split :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?max:int ->
  ?callout:Pcre.callout ->
  string ->
  (Pcre.split_result list, Pcre.error) result

(* Return 'on_error' in case of a PCRE error. The error is logged. *)
val split_noerr :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?max:int ->
  ?callout:Pcre.callout ->
  on_error:string list ->
  string ->
  string list

(*
   Register printers for the Pcre module/library so that exceptions show up
   nicely with 'Printexc.to_string' e.g. 'Pcre.Error(RecursionLimit)'
   instead of 'Pcre.Error(5)'.

   See issue https://github.com/mmottl/pcre-ocaml/issues/24
*)
val register_exception_printer : unit -> unit

val substitute :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  subst:(string -> string) ->
  string ->
  string
(** [substitute] replaces according to the substitution function [subst] *)

val replace_first :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  template:string ->
  string ->
  string
(** [replace_first] replaces the first match according to the substitution template `templ` *)

val replace :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?callout:Pcre.callout ->
  template:string ->
  string ->
  string
(** [replace] replaces all matches according to the substitution template `templ` *)

val extract_all :
  ?iflags:Pcre.irflag ->
  ?flags:Pcre.rflag list ->
  rex:t ->
  ?pos:int ->
  ?full_match:bool ->
  ?callout:Pcre.callout ->
  string ->
  string array array

(*
   Exception-less version of Pcre.get_named_substring and Pcre.get_named_substring_ofs

   Ok None: variable name is valid but unbound
   Error msg: no such variable in the original pattern
*)
val get_named_substring_and_ofs :
  t ->
  string ->
  Pcre.substrings ->
  ((string * (int * int)) option, string) Result.t

val quote : string -> string

(* internals, reused in Pcre2_.ml *)
val src : Logs.src

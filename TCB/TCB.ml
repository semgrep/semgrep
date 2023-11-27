(*###########################################################################*)
(* Prelude *)
(*###########################################################################*)
(* The Trusted Computing Base (TCB).
 *
 * This module is meant as a replacement for Stdlib.mli but making it
 * impossible to use sensitive resources such as stdout, stdin, files, etc.
 * In some sense, it is insane Stdlib.ml contains [open_in] and [open_out]
 * which gives the ability for any line of code to destruct entirely the
 * filesystem of the user.
 *
 * This file started with the content of ~/.opam/4.14.1/lib/pervasives.ml,
 * which itself is a summary and alias of Stdlib.mli. I changed the value
 * of many functions to '()' to mask and forbid their use. Indeed, even if
 * you use ocamlc -open TCB, the compiler still open Stdlib, so the only way
 * to forbid access to the sensitive resource in Stdlib is to reuse the same
 * name but change the type and value to unit.
 *
 * references:
 *  - https://en.wikipedia.org/wiki/Trusted_computing_base
 *
 * Currently the TCB for semgrep itself is this file and lots of other things:
 *  - Logs library, BOS, cohttp, ..., all the libs we use, see opam.lock
 *  - tree-sitter C runtime and all the parser.c (could write
 *    semgrep rules to audit and check those parser.c files)
 *  - ... lots of stuff, huge TCB
 *
 *)

(*###########################################################################*)
(* Stdlib (was pervasive) *)
(*###########################################################################*)

module UStdlib = Stdlib

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(* Exceptions *)
(**************************************************************************)

external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"

let invalid_arg = invalid_arg
let failwith = failwith

exception Exit

(* TODO? many more exns defined in Stdlib.mli were not in pervasives.ml *)

(**************************************************************************)
(* Comparisons *)
(**************************************************************************)

external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"

let min = min
let max = max

external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

(**************************************************************************)
(* Boolean *)
(**************************************************************************)

external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"

(* was deprecated before *)
let ( & ) = ()
let ( or ) = ()

(**************************************************************************)
(* __XXX__ *)
(**************************************************************************)

external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"
external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

(**************************************************************************)
(* Control *)
(**************************************************************************)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

(**************************************************************************)
(* Int *)
(**************************************************************************)

external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
external ( mod ) : int -> int -> int = "%modint"

let abs = abs
let max_int = max_int
let min_int = min_int

external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"

let lnot = lnot

external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"

(**************************************************************************)
(* Float *)
(**************************************************************************)

external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"

external ( ** ) : float -> float -> float = "caml_power_float" "pow"
[@@unboxed] [@@noalloc]

external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"

external sqrt : float -> float = "caml_sqrt_float" "sqrt"
[@@unboxed] [@@noalloc]

external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]

external log10 : float -> float = "caml_log10_float" "log10"
[@@unboxed] [@@noalloc]

external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
[@@unboxed] [@@noalloc]

external log1p : float -> float = "caml_log1p_float" "caml_log1p"
[@@unboxed] [@@noalloc]

external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]

external acos : float -> float = "caml_acos_float" "acos"
[@@unboxed] [@@noalloc]

external asin : float -> float = "caml_asin_float" "asin"
[@@unboxed] [@@noalloc]

external atan : float -> float = "caml_atan_float" "atan"
[@@unboxed] [@@noalloc]

external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
[@@unboxed] [@@noalloc]

external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
[@@unboxed] [@@noalloc]

external cosh : float -> float = "caml_cosh_float" "cosh"
[@@unboxed] [@@noalloc]

external sinh : float -> float = "caml_sinh_float" "sinh"
[@@unboxed] [@@noalloc]

external tanh : float -> float = "caml_tanh_float" "tanh"
[@@unboxed] [@@noalloc]

external ceil : float -> float = "caml_ceil_float" "ceil"
[@@unboxed] [@@noalloc]

external floor : float -> float = "caml_floor_float" "floor"
[@@unboxed] [@@noalloc]

external abs_float : float -> float = "%absfloat"

external copysign : float -> float -> float
  = "caml_copysign_float" "caml_copysign"
[@@unboxed] [@@noalloc]

external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
[@@unboxed] [@@noalloc]

external frexp : float -> float * int = "caml_frexp_float"

external ldexp : (float[@unboxed]) -> (int[@untagged]) -> (float[@unboxed])
  = "caml_ldexp_float" "caml_ldexp_float_unboxed"
[@@noalloc]

external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"

let infinity = infinity
let neg_infinity = neg_infinity
let nan = nan
let max_float = max_float
let min_float = min_float
let epsilon_float = epsilon_float

type nonrec fpclass = fpclass

external classify_float : (float[@unboxed]) -> fpclass
  = "caml_classify_float" "caml_classify_float_unboxed"
[@@noalloc]

(**************************************************************************)
(* Chars and strings *)
(**************************************************************************)

let ( ^ ) = ( ^ )

external int_of_char : char -> int = "%identity"

let char_of_int = char_of_int
let string_of_bool = string_of_bool
let bool_of_string = bool_of_string
let bool_of_string_opt = bool_of_string_opt
let string_of_int = string_of_int

external int_of_string : string -> int = "caml_int_of_string"

let int_of_string_opt = int_of_string_opt
let string_of_float = string_of_float

external float_of_string : string -> float = "caml_float_of_string"

let float_of_string_opt = float_of_string_opt

(**************************************************************************)
(* Pairs *)
(**************************************************************************)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(**************************************************************************)
(* Lists *)
(**************************************************************************)

let ( @ ) = ( @ )

(**************************************************************************)
(* Stdin/Stdout (FORBIDDEN) *)
(**************************************************************************)

let stdin = ()
let stdout = ()

(* printing on stdout *)
let print_char = ()
let print_string = ()
let print_bytes = ()
let print_int = ()
let print_float = ()
let print_endline = ()
let print_newline = ()

(* reading on stdin *)
let read_line = ()
let read_int = ()
let read_int_opt = ()
let read_float = ()
let read_float_opt = ()

(**************************************************************************)
(* Stderr *)
(**************************************************************************)

(* stderr is an ambient authority *)
let stderr = stderr
let prerr_char = prerr_char
let prerr_string = prerr_string
let prerr_bytes = prerr_bytes
let prerr_int = prerr_int
let prerr_float = prerr_float
let prerr_endline = prerr_endline
let prerr_newline = prerr_newline

(**************************************************************************)
(* Filesystem (FORBIDDEN) *)
(**************************************************************************)

type nonrec open_flag = open_flag

(* nosemgrep: ocaml.lang.portability.crlf-support.prefer-write-in-binary-mode *)
let open_out = ()
let open_out_bin = ()
let open_out_gen = ()

(* nosemgrep: ocaml.lang.portability.crlf-support.prefer-read-in-binary-mode *)
let open_in = ()
let open_in_bin = ()
let open_in_gen = ()

(**************************************************************************)
(* Marshalling *)
(**************************************************************************)

let input_value = ()

(* this we could potentially allow, but more symetric to forbid *)
let output_value = ()

(**************************************************************************)
(* Channel IO *)
(**************************************************************************)

type nonrec in_channel = in_channel
type nonrec out_channel = out_channel

(* Those functions are ok; they already take a channel as a parameter, which
 * is a capability.
 *)

let flush = flush
let flush_all = flush_all

(* output *)
let output_char = output_char
let output_string = output_string
let output_bytes = output_bytes
let output = output
let output_substring = output_substring
let output_byte = output_byte
let output_binary_int = output_binary_int
let seek_out = seek_out
let pos_out = pos_out
let out_channel_length = out_channel_length

(* input *)
let input_char = input_char
let input_line = input_line
let input = input
let really_input = really_input
let really_input_string = really_input_string
let input_byte = input_byte
let input_binary_int = input_binary_int
let seek_in = seek_in
let pos_in = pos_in
let in_channel_length = in_channel_length

(* closing is ok *)
let close_in = close_in
let close_in_noerr = close_in_noerr
let set_binary_mode_in = set_binary_mode_in
let close_out = close_out
let close_out_noerr = close_out_noerr
let set_binary_mode_out = set_binary_mode_out

(* was not in pervasive.ml but was in Stdlib.mli *)
let unsafe_really_input = ()

module LargeFile = LargeFile

(**************************************************************************)
(* Refs *)
(**************************************************************************)

type nonrec 'a ref = 'a ref

external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"

(**************************************************************************)
(* Result *)
(**************************************************************************)

type nonrec ('a, 'b) result = ('a, 'b) result

(**************************************************************************)
(* Format *)
(**************************************************************************)

type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

let string_of_format = string_of_format

external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
  = "%identity"

let ( ^^ ) = ( ^^ )

(**************************************************************************)
(* Exit (FORBIDDEN) *)
(**************************************************************************)

let exit = ()
let at_exit = ()
let do_at_exit = ()

(**************************************************************************)
(* Misc *)
(**************************************************************************)

(* safe, but better to forbid to use *)
let valid_float_lexem = ()

external ignore : 'a -> unit = "%ignore"

(*###########################################################################*)
(* Module aliases *)
(*###########################################################################*)
(* those were at the end of Stdlib.ml *)

(* Basic data structures (usually safe) *)
module Unit = Unit
module Bool = Bool
module Uchar = Uchar
module Char = Char
module String = String

(* Numbers *)
module Complex = Complex
module Float = Float
module Int = Int
module Int32 = Int32
module Int64 = Int64
module Nativeint = Nativeint

(* Composite data structures *)
module Option = Option
module Result = Result
module Either = Either

(* Containers *)
module Seq = Seq
module List = List
module Set = Set
module Stack = Stack
module Map = Map
module Queue = Queue
module Hashtbl = Hashtbl
module Lazy = Lazy

(* less: could forbid the unsafe variants *)
module Array = Array
module Bigarray = Bigarray
module Bytes = Bytes
module Buffer = Buffer

(* only unsafe is [usage()] printing on stdout, but not worth it for now *)
module Arg = Arg

(* label variants (forbidden, not worth it) *)
module ArrayLabels = struct end
module BytesLabels = struct end
module ListLabels = struct end
module MoreLabels = struct end
module StringLabels = struct end
module StdLabels = struct end

(* deprecated anyway *)
module Genlex = struct end
module Stream = struct end
module Pervasives = struct end

(* TODO: to review *)
module Filename = Filename

(* ?? *)
module Atomic = Atomic
module Callback = Callback
module Digest = Digest
module Ephemeron = Ephemeron
module Fun = Fun
module Gc = Gc

(*###########################################################################*)
(* Sys (RESTRICTED) *)
(*###########################################################################*)

(* U for "Unsafe/Unvetted" Sys *)
module USys = Sys

module Sys = struct
  let interactive = Sys.interactive

  type signal_behavior = Sys.signal_behavior =
    | Signal_default
    | Signal_ignore
    | Signal_handle of (int -> unit)

  type backend_type = Sys.backend_type
  type extra_prefix = Sys.extra_prefix
  type extra_info = Sys.extra_info
  type ocaml_release_info = Sys.ocaml_release_info

  (* less: Immediate64 submodule *)
end

(*###########################################################################*)
(* Unix (RESTRICTED) *)
(*###########################################################################*)

(* Unsafe (original) Unix *)
module UUnix = Unix

module Unix = struct
  type error = Unix.error =
    | E2BIG  (** Argument list too long *)
    | EACCES  (** Permission denied *)
    | EAGAIN  (** Resource temporarily unavailable; try again *)
    | EBADF  (** Bad file descriptor *)
    | EBUSY  (** Resource unavailable *)
    | ECHILD  (** No child process *)
    | EDEADLK  (** Resource deadlock would occur *)
    | EDOM  (** Domain error for math functions, etc. *)
    | EEXIST  (** File exists *)
    | EFAULT  (** Bad address *)
    | EFBIG  (** File too large *)
    | EINTR  (** Function interrupted by signal *)
    | EINVAL  (** Invalid argument *)
    | EIO  (** Hardware I/O error *)
    | EISDIR  (** Is a directory *)
    | EMFILE  (** Too many open files by the process *)
    | EMLINK  (** Too many links *)
    | ENAMETOOLONG  (** Filename too long *)
    | ENFILE  (** Too many open files in the system *)
    | ENODEV  (** No such device *)
    | ENOENT  (** No such file or directory *)
    | ENOEXEC  (** Not an executable file *)
    | ENOLCK  (** No locks available *)
    | ENOMEM  (** Not enough memory *)
    | ENOSPC  (** No space left on device *)
    | ENOSYS  (** Function not supported *)
    | ENOTDIR  (** Not a directory *)
    | ENOTEMPTY  (** Directory not empty *)
    | ENOTTY  (** Inappropriate I/O control operation *)
    | ENXIO  (** No such device or address *)
    | EPERM  (** Operation not permitted *)
    | EPIPE  (** Broken pipe *)
    | ERANGE  (** Result too large *)
    | EROFS  (** Read-only file system *)
    | ESPIPE  (** Invalid seek e.g. on a pipe *)
    | ESRCH  (** No such process *)
    | EXDEV  (** Invalid link *)
    | EWOULDBLOCK  (** Operation would block *)
    | EINPROGRESS  (** Operation now in progress *)
    | EALREADY  (** Operation already in progress *)
    | ENOTSOCK  (** Socket operation on non-socket *)
    | EDESTADDRREQ  (** Destination address required *)
    | EMSGSIZE  (** Message too long *)
    | EPROTOTYPE  (** Protocol wrong type for socket *)
    | ENOPROTOOPT  (** Protocol not available *)
    | EPROTONOSUPPORT  (** Protocol not supported *)
    | ESOCKTNOSUPPORT  (** Socket type not supported *)
    | EOPNOTSUPP  (** Operation not supported on socket *)
    | EPFNOSUPPORT  (** Protocol family not supported *)
    | EAFNOSUPPORT  (** Address family not supported by protocol family *)
    | EADDRINUSE  (** Address already in use *)
    | EADDRNOTAVAIL  (** Can't assign requested address *)
    | ENETDOWN  (** Network is down *)
    | ENETUNREACH  (** Network is unreachable *)
    | ENETRESET  (** Network dropped connection on reset *)
    | ECONNABORTED  (** Software caused connection abort *)
    | ECONNRESET  (** Connection reset by peer *)
    | ENOBUFS  (** No buffer space available *)
    | EISCONN  (** Socket is already connected *)
    | ENOTCONN  (** Socket is not connected *)
    | ESHUTDOWN  (** Can't send after socket shutdown *)
    | ETOOMANYREFS  (** Too many references: can't splice *)
    | ETIMEDOUT  (** Connection timed out *)
    | ECONNREFUSED  (** Connection refused *)
    | EHOSTDOWN  (** Host is down *)
    | EHOSTUNREACH  (** No route to host *)
    | ELOOP  (** Too many levels of symbolic links *)
    | EOVERFLOW  (** File size or position not representable *)
    | EUNKNOWNERR of int  (** Unknown error *)

  let error_message = Unix.error_message

  type process_status = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  type wait_flag = Unix.wait_flag = WNOHANG | WUNTRACED

  (* already a capability *)
  type file_descr = Unix.file_descr

  let stdin = ()
  let stdout = ()
  let stderr = Unix.stderr
  let descr_of_out_channel = Unix.descr_of_out_channel
  let descr_of_in_channel = Unix.descr_of_in_channel

  (* safe? need to restrict? *)
  let dup = Unix.dup
  let dup2 = Unix.dup2

  (* already a capability *)
  type dir_handle = Unix.dir_handle

  let closedir = Unix.closedir
  let readdir = Unix.readdir

  type open_flag = Unix.open_flag =
    | O_RDONLY  (** Open for reading *)
    | O_WRONLY  (** Open for writing *)
    | O_RDWR  (** Open for reading and writing *)
    | O_NONBLOCK  (** Open in non-blocking mode *)
    | O_APPEND  (** Open for append *)
    | O_CREAT  (** Create if nonexistent *)
    | O_TRUNC  (** Truncate to 0 length if existing *)
    | O_EXCL  (** Fail if existing *)
    | O_NOCTTY  (** Don't make this dev a controlling tty *)
    | O_DSYNC
    | O_SYNC
    | O_RSYNC
    | O_SHARE_DELETE
    | O_CLOEXEC
    | O_KEEPEXEC

  type file_perm = Unix.file_perm
  type seek_command = Unix.seek_command

  type file_kind = Unix.file_kind =
    | S_REG  (** Regular file *)
    | S_DIR  (** Directory *)
    | S_CHR  (** Character device *)
    | S_BLK  (** Block device *)
    | S_LNK  (** Symbolic link *)
    | S_FIFO  (** Named pipe *)
    | S_SOCK  (** Socket *)

  type stats = Unix.stats = {
    st_dev : int;  (** Device number *)
    st_ino : int;  (** Inode number *)
    st_kind : file_kind;  (** Kind of the file *)
    st_perm : file_perm;  (** Access rights *)
    st_nlink : int;  (** Number of links *)
    st_uid : int;  (** User id of the owner *)
    st_gid : int;  (** Group ID of the file's group *)
    st_rdev : int;  (** Device ID (if special file) *)
    st_size : int;  (** Size in bytes *)
    st_atime : float;  (** Last access time *)
    st_mtime : float;  (** Last modification time *)
    st_ctime : float;  (** Last status change time *)
  }

  let close_process_in = Unix.close_process_in
  let close_process_out = Unix.close_process_out
  let read = Unix.read
  let write = Unix.write
  let close = Unix.close

  type access_permission = Unix.access_permission
  type lock_command = Unix.lock_command
  type sigprocmask_command = Unix.sigprocmask_command
  type process_times = Unix.process_times

  type tm = Unix.tm = {
    tm_sec : int;  (** Seconds 0..60 *)
    tm_min : int;  (** Minutes 0..59 *)
    tm_hour : int;  (** Hours 0..23 *)
    tm_mday : int;  (** Day of month 1..31 *)
    tm_mon : int;  (** Month of year 0..11 *)
    tm_year : int;  (** Year - 1900 *)
    tm_wday : int;  (** Day of week (Sunday is 0) *)
    tm_yday : int;  (** Day of year 0..365 *)
    tm_isdst : bool;  (** Daylight time savings in effect *)
  }

  let mktime = Unix.mktime
  let gmtime = Unix.gmtime

  (* potentially side-channel, but probably fine *)
  let localtime = Unix.localtime

  type interval_timer = Unix.interval_timer
  type interval_timer_status = Unix.interval_timer_status
  type passwd_entry = Unix.passwd_entry
  type group_entry = Unix.group_entry

  (* already a capability *)
  type inet_addr = Unix.inet_addr
  type socket_domain = Unix.socket_domain
  type socket_type = Unix.socket_type
  type sockaddr = Unix.sockaddr
  type shutdown_command = Unix.shutdown_command
  type msg_flag = Unix.msg_flag
  type socket_bool_option = Unix.socket_bool_option
  type socket_int_option = Unix.socket_int_option
  type socket_optint_option = Unix.socket_optint_option
  type socket_float_option = Unix.socket_float_option
  type host_entry = Unix.host_entry
  type protocol_entry = Unix.protocol_entry
  type service_entry = Unix.service_entry
  type addr_info = Unix.addr_info
  type getaddrinfo_option = Unix.getaddrinfo_option
  type name_info = Unix.name_info
  type getnameinfo_option = Unix.getnameinfo_option
  type terminal_io = Unix.terminal_io
  type setattr_when = Unix.setattr_when
  type flush_queue = Unix.flush_queue
  type flow_action = Unix.flow_action
end

(*###########################################################################*)
(* Obj (FORBIDDEN) *)
(*###########################################################################*)

module Obj = struct end

(*###########################################################################*)
(* Other *)
(*###########################################################################*)

(**************************************************************************)
(* XXX *)
(**************************************************************************)

(* TODO to mask *)
module Out_channel = Out_channel
module In_channel = In_channel
module Format = Format
module Lexing = Lexing
module Marshal = Marshal
module Oo = Oo
module Parsing = Parsing
module Printexc = Printexc
module Printf = Printf
module Random = Random
module Scanf = Scanf
module Weak = Weak

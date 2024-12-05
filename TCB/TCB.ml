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
 * If you really need to use the sensitive functions/constants in Stdlib.ml,
 * at least use the explicitely qualified UStdlib.xxx (e.g., UStdlib.exit),
 * but if you can it is better to use the capability-aware variant in one of
 * the CapXxx.ml file (e.g., CapStdlib.exit).
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
(* Unsafe variants *)
(*###########################################################################*)

(* U for "Unsafe/Unvetted" *)
module UStdlib = Stdlib
module Stdlib = struct end
(* See also UUnix, USys, ... later *)

(*###########################################################################*)
(* Stdlib *)
(*###########################################################################*)

(* Stdlib.ml, which was called pervasive.ml for a long time, is [open]ed
 * implicitely in every file, so we must mask the dangerous functions in it.
 * I tried to keep the same order of the definitions than in Stdlib.mli
 * (and pervasive.ml) below so if new versions of OCaml introduce new
 * builtins, we can easily just go thouch each file in parallel and
 * notice those new builtins.
 *)

(**************************************************************************)
(* Copyright *)
(**************************************************************************)

(* This file started with the content of ~/.opam/4.14.1/lib/pervasives.ml
 * with the following copyright:
 *)

(*                                 OCaml                                  *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)

(**************************************************************************)
(* Exceptions (see also Printexc module) *)
(**************************************************************************)
(* see also libs/commons/Exception.ml *)

(*
external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"

let invalid_arg = invalid_arg
let failwith = failwith

exception Exit
*)

(* TODO? many more exns defined in Stdlib.mli were not in pervasives.ml *)

(**************************************************************************)
(* Comparisons *)
(**************************************************************************)

(* See also libs/commons/Common.mli which restrict those operations *)

(*
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
*)

(**************************************************************************)
(* Boolean (See also Bool module) *)
(**************************************************************************)

(*
external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
*)

(* was deprecated before *)
let ( & ) = ()
let ( or ) = ()

(**************************************************************************)
(* __XXX__ *)
(**************************************************************************)

(*
external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"
external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
*)

(**************************************************************************)
(* Control (see also Fun module) *)
(**************************************************************************)

(*
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
*)

(**************************************************************************)
(* Int (see also Intxxx modules) *)
(**************************************************************************)

(*
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
*)

(**************************************************************************)
(* Float *)
(**************************************************************************)

(*
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

*)

(**************************************************************************)
(* Char (see also Char/Uchar) modules *)
(**************************************************************************)

(*
external int_of_char : char -> int = "%identity"

let char_of_int = char_of_int
*)

(**************************************************************************)
(* String (see also String/Bytes modules *)
(**************************************************************************)

(*
let ( ^ ) = ( ^ )

(* bool *)
let string_of_bool = string_of_bool
let bool_of_string = bool_of_string
let bool_of_string_opt = bool_of_string_opt

(* int *)
let string_of_int = string_of_int

external int_of_string : string -> int = "caml_int_of_string"

let int_of_string_opt = int_of_string_opt

(* float *)
let string_of_float = string_of_float

external float_of_string : string -> float = "caml_float_of_string"

let float_of_string_opt = float_of_string_opt

*)
(**************************************************************************)
(* Pairs *)
(**************************************************************************)

(*
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
*)

(**************************************************************************)
(* Lists (see also List module) *)
(**************************************************************************)

(*
let ( @ ) = ( @ )
*)

(**************************************************************************)
(* Stdin/Stdout/Stderr (FORBIDDEN) (see also Sys/Unix/... modules)  *)
(**************************************************************************)

(* See also for Out: Printf/Format/Printexc
 * See also for In: Scanf/Lexing/Parsing
 *)

(* See the toplevel comment, but if you really need to use directly stdin,
 * at least qualify it with UStdlib.stdin
 *)
let stdin = ()
let stdout = ()
let stderr = ()

(* printing on stdout *)
let print_char = ()
let print_string = ()
let print_bytes = ()
let print_int = ()
let print_float = ()
let print_endline = ()
let print_newline = ()

(* stderr is forbidden, buts logs are an ambient authority *)
let prerr_char = ()
let prerr_string = ()
let prerr_bytes = ()
let prerr_int = ()
let prerr_float = ()
let prerr_endline = ()
let prerr_newline = ()

(* reading on stdin *)
let read_line = ()
let read_int = ()
let read_int_opt = ()
let read_float = ()
let read_float_opt = ()

(**************************************************************************)
(* Filesystem (FORBIDDEN) (see also Sys/Unix modules) *)
(**************************************************************************)

(*
type nonrec open_flag = open_flag
*)

(* nosemgrep: ocaml.lang.portability.crlf-support.prefer-write-in-binary-mode *)
let open_out = ()
let open_out_bin = ()
let open_out_gen = ()

(* nosemgrep: ocaml.lang.portability.crlf-support.prefer-read-in-binary-mode *)
let open_in = ()
let open_in_bin = ()
let open_in_gen = ()

(**************************************************************************)
(* Channel IO (see also Out_channel/In_channel/Unix modules) *)
(**************************************************************************)

(* already a capability *)
(*
type nonrec in_channel = in_channel
type nonrec out_channel = out_channel
*)

(* Those functions are ok; they already take a channel as a parameter, which
 * is a capability.
 *)

(*
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

*)

(* was not in pervasive.ml but was in Stdlib.mli *)
let unsafe_really_input = ()

(* we could reexport it, but I don't think code in Semgrep use it *)
module LargeFile = struct end

(**************************************************************************)
(* Marshalling (see also Marshal module) (FORBIDDEN) *)
(**************************************************************************)

let input_value = ()

(* We could potentially allow output_value(), but more symetric to forbid,
 * and anyway better to explicitely use the Marshal module
 *)
let output_value = ()

(**************************************************************************)
(* Refs *)
(**************************************************************************)

(*
type nonrec 'a ref = 'a ref

external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"
*)

(**************************************************************************)
(* Result (see also Result module) *)
(**************************************************************************)

(*
type nonrec ('a, 'b) result = ('a, 'b) result
*)

(**************************************************************************)
(* Format (see also Format module) *)
(**************************************************************************)

type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

(*
let string_of_format = string_of_format

external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
  = "%identity"

let ( ^^ ) = ( ^^ )
*)

(**************************************************************************)
(* Exit (FORBIDDEN) (see also Sys/Unix modules) *)
(**************************************************************************)

let exit = ()
let at_exit = ()
let do_at_exit = ()

(**************************************************************************)
(* Misc *)
(**************************************************************************)

(* safe, but better to forbid to use *)
let valid_float_lexem = ()

(*
external ignore : 'a -> unit = "%ignore"
*)

(*###########################################################################*)
(* Safe Module aliases *)
(*###########################################################################*)
(* Those modules were listed (alphabetically) at the end of Stdlib.ml
 * I've reordered them to give more structures, but if new OCaml versions
 * introduce new standard module, it can be important to update the list
 * below.
 *)

(**************************************************************************)
(* Basic data structures (usually safe) *)
(**************************************************************************)

(* Those modules used to not be in comments, but I think it slows down
 * ocaml because the signature of 'TCB' (which is opened for every module)
 * becomes bigger. Anyway, we were aliasing them to their original content,
 * so simpler to just do nothing. What matters is to restrict the
 * dangerous modules
 *)

(*
module Unit = Unit
module Bool = Bool
module Uchar = Uchar
module Char = Char
module String = String
*)
(**************************************************************************)
(* Numbers *)
(**************************************************************************)
(*
module Int = Int
module Int32 = Int32
module Int64 = Int64
module Nativeint = Nativeint
module Float = Float
module Complex = Complex
*)

(**************************************************************************)
(* Composite data structures *)
(**************************************************************************)
(*
module Option = Option
module Result = Result
module Either = Either
*)

(**************************************************************************)
(* Containers *)
(**************************************************************************)
(*
module Seq = Seq
module List = List
module Set = Set
module Stack = Stack
module Map = Map
module Queue = Queue
module Hashtbl = Hashtbl
module Weak = Weak
*)

(**************************************************************************)
(* Arrays and buffers *)
(**************************************************************************)

(* less: could forbid the unsafe variants *)
(*
module Array = Array
module Bigarray = Bigarray
module Bytes = Bytes
module Buffer = Buffer
*)
(**************************************************************************)
(* Misc *)
(**************************************************************************)

(*
(* only unsafe is [usage()] printing on stdout, but not worth it for now *)
module Arg = Arg
module Lazy = Lazy
(* ?? *)
module Callback = Callback
module Digest = Digest
module Ephemeron = Ephemeron
module Fun = Fun
module Gc = Gc
*)

(**************************************************************************)
(* Deprecated modules *)
(**************************************************************************)
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

(* nobody use oo *)
module Oo = struct end

(*###########################################################################*)
(* Other module aliases (FORBIDDEN) *)
(*###########################################################################*)

module URandom = Random
module Random = struct end

(*###########################################################################*)
(* RESTRICTED modules *)
(*###########################################################################*)

(**************************************************************************)
(* Sys (RESTRICTED) *)
(**************************************************************************)

(* Unsafe (original) Sys *)
module USys = Sys

module Sys = struct
  let interactive = Sys.interactive

  type signal_behavior = Sys.signal_behavior =
    | Signal_default
    | Signal_ignore
    | Signal_handle of (int -> unit)

  (* LATER? create different capabilities for each signal? *)
  (* ref: https://faculty.cs.niu.edu/~hutchins/csci480/signals.htm *)
  let sighup = Sys.sighup
  let sigquit = Sys.sigquit

  (* Usually paging errors...  *)
  let sigbus = Sys.sigbus
  let sigpipe = Sys.sigpipe
  let sigterm = Sys.sigterm
  let sigsys = Sys.sigsys
  let sigalrm = Sys.sigalrm
  let sigxfsz = Sys.sigxfsz
  let sigint = Sys.sigint
  let sigkill = Sys.sigkill

  type backend_type = Sys.backend_type
  type extra_prefix = Sys.extra_prefix
  type extra_info = Sys.extra_info
  type ocaml_release_info = Sys.ocaml_release_info

  (* alt: keep them in USys *)
  let unix = Sys.unix
  let win32 = Sys.win32
  let cygwin = Sys.cygwin
  (* less: Immediate64 submodule *)
end

(**************************************************************************)
(* Unix (RESTRICTED) *)
(**************************************************************************)

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
  let stderr = ()
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

  (* not that used so not worth repeating their definitions *)
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

  (* FORBIDDEN:
   * - a lot
   *)
end

(*###########################################################################*)
(* Obj (FORBIDDEN) *)
(*###########################################################################*)

(* We absolutely need to forbid Obj, and especially Obj.magic which can
 * subvert the type system and allows for example code to forge any
 * types (including capabilities).
 *)
module Obj = struct end

(**************************************************************************)
(* Marshall (RESTRICTED) *)
(**************************************************************************)

module UMarshal = Marshal

module Marshal = struct
  type extern_flags = Marshal.extern_flags
  (* can't put def here because there is a change between 4.14 and 5.0
     =
      | No_sharing  (** Don't preserve sharing *)
      | Closures  (** Send function closures *)
      | Compat_32  (** Ensure 32-bit compatibility *)
  *)

  (* FORBIDDEN:
     - from_string
  *)
  let to_string = Marshal.to_string
end

(**************************************************************************)
(* Filename (RESTRICTED) *)
(**************************************************************************)

module UFilename = Filename

module Filename = struct
  let basename = Filename.basename
  let dirname = Filename.dirname
  let concat = Filename.concat
  let chop_extension = Filename.chop_extension
  let is_relative = Filename.is_relative
  let quote = Filename.quote

  (* FORBIDDEN:
     - current_dir_name, parent_dir_name
     - temp files stuff
     - ...
  *)
end

(**************************************************************************)
(* Reading (RESTRICTED) *)
(**************************************************************************)

module In_channel = struct
  (* most of the other functions are already in Stdlib so no need
   * provide an extra alias for them
   *)
  let input_all = In_channel.input_all
  (* FORBIDDEN:
   *)
end

module Lexing = struct
  type position = Lexing.position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

  type lexbuf = Lexing.lexbuf = {
    refill_buff : lexbuf -> unit;
    mutable lex_buffer : bytes;
    mutable lex_buffer_len : int;
    mutable lex_abs_pos : int;
    mutable lex_start_pos : int;
    mutable lex_curr_pos : int;
    mutable lex_last_pos : int;
    mutable lex_last_action : int;
    mutable lex_eof_reached : bool;
    mutable lex_mem : int array;
    mutable lex_start_p : position;
    mutable lex_curr_p : position;
  }

  let from_string = Lexing.from_string
  let from_channel = Lexing.from_channel

  (* FORBIDDEN:
*)
end

module UParsing = Parsing

module Parsing = struct
  (* FORBIDDEN:
*)
end

(* Forbidden, not worth it *)
module Scanf = struct end

(**************************************************************************)
(* Printing (RESTRICTED) *)
(**************************************************************************)

module Out_channel = struct
  (* TODO? *)
  (* FORBIDDEN:
   *)
end

module UPrintf = Printf

module Printf = struct
  let sprintf = Printf.sprintf
  let fprintf = Printf.fprintf
  let eprintf = Printf.eprintf
  let bprintf = Printf.bprintf

  (* FORBIDDEN:
     - printf
  *)
end

module UFormat = Format

module Format = struct
  (* already a capability *)
  type formatter = Format.formatter

  let formatter_of_buffer = Format.formatter_of_buffer
  let fprintf = Format.fprintf
  let sprintf = Format.sprintf
  let kfprintf = Format.kfprintf
  let kasprintf = Format.kasprintf
  let ikfprintf = Format.ikfprintf

  (* pp_xxx are safe *)
  let pp_print_int = Format.pp_print_int
  let pp_print_string = Format.pp_print_string
  let pp_print_list = Format.pp_print_list
  let pp_print_as = Format.pp_print_as
  let pp_print_flush = Format.pp_print_flush
  let pp_set_margin = Format.pp_set_margin
  let pp_open_box = Format.pp_open_box
  let pp_print_break = Format.pp_print_break

  (* FORBIDDEN:
      - all the print_xxx variant that use directly to stdout
      - std_formatter, err_formatter
      - printf, eprintf
      - stdbuf, str_formatter (unsafe in the end)
  *)
end

module Printexc = struct
  type raw_backtrace = Printexc.raw_backtrace

  let record_backtrace = Printexc.record_backtrace
  let register_printer = Printexc.register_printer
  let get_backtrace = Printexc.get_backtrace
  let get_raw_backtrace = Printexc.get_raw_backtrace
  let get_callstack = Printexc.get_callstack
  let raise_with_backtrace = Printexc.raise_with_backtrace
  let to_string = Printexc.to_string
  let raw_backtrace_to_string = Printexc.raw_backtrace_to_string

  (* FORBIDDEN:
     - print, print_backtrace
     - ...
  *)
end

(**************************************************************************)
(* Concurrency (RESTRICTED) *)
(**************************************************************************)

(*
module Atomic = Atomic
*)

(**************************************************************************)
(* Process/threads/domains (RESTRICTED) *)
(**************************************************************************)

(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* Commentary *)
(* Some helper code that should be in Sys.ml *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* See https://man7.org/linux/man-pages/man7/signal.7.html*)
type signal =
  | SIGHUP
  | SIGINT
  | SIGQUIT
  | SIGILL
  | SIGTRAP
  | SIGABRT (* alt: SIGIOT *)
  | SIGBUS
  | SIGFPE
  | SIGKILL
  | SIGUSR1
  | SIGSEGV
  | SIGUSR2
  | SIGPIPE
  | SIGALRM
  | SIGTERM
  | SIGSTKFLT
  | SIGCHLD
  | SIGCONT
  | SIGSTOP
  | SIGTSTP
  | SIGTTIN
  | SIGTTOU
  | SIGURG
  | SIGXCPU
  | SIGXFSZ
  | SIGVTALRM
  | SIGPROF
  | SIGWINCH
  | SIGPOLL (* alt: SIGIO *)
  | SIGPWR
  | SIGSYS (* alt: SIGUNUSED *)
  | UNKNOWN of int

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let signal_to_string = function
  | SIGHUP -> "SIGHUP"
  | SIGINT -> "SIGINT"
  | SIGQUIT -> "SIGQUIT"
  | SIGILL -> "SIGILL"
  | SIGTRAP -> "SIGTRAP"
  | SIGABRT -> "SIGABRT"
  | SIGBUS -> "SIGBUS"
  | SIGFPE -> "SIGFPE"
  | SIGKILL -> "SIGKILL"
  | SIGUSR1 -> "SIGUSR1"
  | SIGSEGV -> "SIGSEGV"
  | SIGUSR2 -> "SIGUSR2"
  | SIGPIPE -> "SIGPIPE"
  | SIGALRM -> "SIGALRM"
  | SIGTERM -> "SIGTERM"
  | SIGSTKFLT -> "SIGSTKFLT"
  | SIGCHLD -> "SIGCHLD"
  | SIGCONT -> "SIGCONT"
  | SIGSTOP -> "SIGSTOP"
  | SIGTSTP -> "SIGTSTP"
  | SIGTTIN -> "SIGTTIN"
  | SIGTTOU -> "SIGTTOU"
  | SIGURG -> "SIGURG"
  | SIGXCPU -> "SIGXCPU"
  | SIGXFSZ -> "SIGXFSZ"
  | SIGVTALRM -> "SIGVTALRM"
  | SIGPROF -> "SIGPROF"
  | SIGWINCH -> "SIGWINCH"
  | SIGPOLL -> "SIGPOLL"
  | SIGPWR -> "SIGPWR"
  | SIGSYS -> "SIGSYS"
  | UNKNOWN i -> Printf.sprintf "UNKNOWN(%d)" i

(* NOTE these are not portable to Alpha/SPARC/MIPS/PARISC *)
let signal_to_linux_signal = function
  | SIGHUP -> 1
  | SIGINT -> 2
  | SIGQUIT -> 3
  | SIGILL -> 4
  | SIGTRAP -> 5
  | SIGABRT -> 6
  | SIGBUS -> 7
  | SIGFPE -> 8
  | SIGKILL -> 9
  | SIGUSR1 -> 10
  | SIGSEGV -> 11
  | SIGUSR2 -> 12
  | SIGPIPE -> 13
  | SIGALRM -> 14
  | SIGTERM -> 15
  | SIGSTKFLT -> 16
  | SIGCHLD -> 17
  | SIGCONT -> 18
  | SIGSTOP -> 19
  | SIGTSTP -> 20
  | SIGTTIN -> 21
  | SIGTTOU -> 22
  | SIGURG -> 23
  | SIGXCPU -> 24
  | SIGXFSZ -> 25
  | SIGVTALRM -> 26
  | SIGPROF -> 27
  | SIGWINCH -> 28
  | SIGPOLL -> 29
  | SIGPWR -> 30
  | SIGSYS -> 31
  | UNKNOWN i -> i

(* This is what a lot of Linux programs do, it's best practice according to the
   advanced bash scripting guide

   See: https://tldp.org/LDP/abs/html/exitcodes.html *)
let signal_to_linux_exit_code signal = -128 - signal_to_linux_signal signal

(* OCaml does not number signals normally, i.e. -9 != sigkill so let's convert
   them to [signal] here. *)
let ocaml_signal_to_signal = function
  | s when s = USys.sigabrt -> SIGABRT
  | s when s = USys.sigalrm -> SIGALRM
  | s when s = USys.sigfpe -> SIGFPE
  | s when s = USys.sighup -> SIGHUP
  | s when s = USys.sigill -> SIGILL
  | s when s = USys.sigint -> SIGINT
  | s when s = USys.sigkill -> SIGKILL
  | s when s = USys.sigpipe -> SIGPIPE
  | s when s = USys.sigquit -> SIGQUIT
  | s when s = USys.sigsegv -> SIGSEGV
  | s when s = USys.sigterm -> SIGTERM
  | s when s = USys.sigusr1 -> SIGUSR1
  | s when s = USys.sigusr2 -> SIGUSR2
  | s when s = USys.sigchld -> SIGCHLD
  | s when s = USys.sigcont -> SIGCONT
  | s when s = USys.sigstop -> SIGSTOP
  | s when s = USys.sigtstp -> SIGTSTP
  | s when s = USys.sigttin -> SIGTTIN
  | s when s = USys.sigttou -> SIGTTOU
  | s when s = USys.sigvtalrm -> SIGVTALRM
  | s when s = USys.sigprof -> SIGPROF
  | s when s = USys.sigbus -> SIGBUS
  | s when s = USys.sigpoll -> SIGPOLL
  | s when s = USys.sigsys -> SIGSYS
  | s when s = USys.sigtrap -> SIGTRAP
  | s when s = USys.sigurg -> SIGURG
  | s when s = USys.sigxcpu -> SIGXCPU
  | s when s = USys.sigxfsz -> SIGXFSZ
  | other -> UNKNOWN other

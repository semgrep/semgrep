(** [signal] is a nice data type representing most linux/posix/unix signals *)
type signal =
  | SIGHUP
  | SIGINT
  | SIGQUIT
  | SIGILL
  | SIGTRAP
  | SIGABRT
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
  | SIGPOLL
  | SIGPWR
  | SIGSYS
  | UNKNOWN of int

val signal_to_string : signal -> string
(** Pretty print a [signal] *)

val signal_to_linux_signal : signal -> int
(** Convert a [signal] to its linux signal number *)

val signal_to_linux_exit_code : signal -> int
(** Convert a [signal] to its linux exit code. This is [-128 - (signal_to_linux_signal s)] *)

val ocaml_signal_to_signal : int -> signal
(** Convert an ocaml signal [Sys.sig*] to a [signal] *)

module Fpath : sig
  val exists : Fpath.t -> bool
  (** Checks whether the given path exists, after normalizing by removing
        trailing slashes (see
        https://sourceforge.net/p/mingw-w64/mailman/message/59154340/ .) *)

  val is_directory : Fpath.t -> bool
  (** Checks whether the given path exists and is a directory, after
        normalizing by removing trailing slashes (see
        https://sourceforge.net/p/mingw-w64/mailman/message/59154340/ .)
        Symbolic links are followed. *)

  val is_regular_file : Fpath.t -> bool
  (** Checks whether the given path exists and is a file, after normalizing
        by removing trailing slashes (see
        https://sourceforge.net/p/mingw-w64/mailman/message/59154340/ .)
        Symbolic links are followed. *)
end

val file_exists : string -> bool
(** Checks whether the given filename exists, after normalizing by removing
    trailing slashes (see https://sourceforge.net/p/mingw-w64/mailman/message/59154340/ .)
    Symbolic links are followed. *)

val is_directory : string -> bool
(** Checks whether the given path exists and is a directory, after normalizing by removing
    trailing slashes (see https://sourceforge.net/p/mingw-w64/mailman/message/59154340/ .)
    Symbolic links are followed. *)

val is_regular_file : string -> bool
(** Checks whether the given path exists and is a file, after normalizing by removing
    trailing slashes (see https://sourceforge.net/p/mingw-w64/mailman/message/59154340/ .)
    Symbolic links are followed. *)

(* Capabilities
 *
 * Note that most of the types here are abstract and there is no way
 * to build/forge them except with constructors that can be restricted by
 * Semgrep rules, or built in Cap.main() below and passed as initial
 * capabilities.
 *)

module Console : sig
  type stdin
  type stdout
  (* stderr and logs are an "ambient" authority *)
end

module FS : sig
  type root_read
  type root_write
  type cwd_read
  type cwd_write
  type tmp_read
  type tmp_write
end

module Exec : sig
  type t
end

module File : sig
  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
end

module Network : sig
  type t
end

type fs_powerbox = {
  root_read : FS.root_read;
  root_write : FS.root_write;
  cwd_read : FS.cwd_read;
  cwd_write : FS.cwd_write;
  tmp_read : FS.tmp_read;
  tmp_write : FS.tmp_write;
}

type powerbox = {
  stdin : Console.stdin;
  stdout : Console.stdout;
  fs : fs_powerbox;
  exec : Exec.t;
  network : Network.t;
}

type no_network = {
  stdin : Console.stdin;
  stdout : Console.stdout;
  fs : fs_powerbox;
  exec : Exec.t;
}

type no_exec = {
  stdin : Console.stdin;
  stdout : Console.stdout;
  fs : fs_powerbox;
}

type no_fs = { stdin : Console.stdin; stdout : Console.stdout }
type nocap

(* Only way to access a powerbox. This must be restricted to be called
 * only from a Main.ml (or Test.ml).
 *)
val main : (powerbox -> 'a) -> 'a

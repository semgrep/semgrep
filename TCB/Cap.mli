(* Capabilities implemented as simple abstract types and explicit
 * parameters ("Lambda the ultimate security tool").
 *
 * Note that most of the types below are on purpose [abstract] and there is
 * no way to build/forge them except by calling the restricted (statically
 * and dynamically) Cap.main() below which is passing all capabilities
 * to the entry point of your program.
 *)

module Console : sig
  type stdin
  type stdout
  (* stderr and logs are an "ambient" authority *)
end

module Process : sig
  type argv
  type env
  type signal

  (* See also the separate Exec.t *)
  type fork
  type domain
  type thread
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

module Misc : sig
  type time
  type random
end

(* The big one *)
type powerbox = {
  process : process_powerbox;
  fs : fs_powerbox;
  exec : Exec.t;
  network : Network.t;
  misc : misc_powerbox;
}

and process_powerbox = {
  stdin : Console.stdin;
  stdout : Console.stdout;
  argv : Process.argv;
  env : Process.env;
  signal : Process.signal;
  fork : Process.fork;
  domain : Process.domain;
  thread : Process.thread;
}

and fs_powerbox = {
  root_read : FS.root_read;
  root_write : FS.root_write;
  cwd_read : FS.cwd_read;
  cwd_write : FS.cwd_write;
  tmp_read : FS.tmp_read;
  tmp_write : FS.tmp_write;
}

and misc_powerbox = { time : Misc.time; random : Misc.random }

(* "subtypes" of powerbox *)
type no_network = {
  process : process_powerbox;
  fs : fs_powerbox;
  exec : Exec.t;
}

type no_exec = { process : process_powerbox; fs : fs_powerbox }
type no_fs = { process : process_powerbox }

type no_concurrency = {
  stdin : Console.stdin;
  stdout : Console.stdout;
  argv : Process.argv;
  env : Process.env;
}

(* you can also pass individual capabilities like just
 * stdout with 'Console.stdout'
 *)

(* pure computation, just cpu/ram *)
type nocap

(* Only way to access a powerbox. This must be restricted to be called
 * only from a Main.ml (or Test.ml).
 *)
val main : (powerbox -> 'a) -> 'a

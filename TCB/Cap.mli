(* Capabilities implemented as simple abstract types and explicit
 * parameters ("Lambda the ultimate security tool").
 *
 * Note that most of the types below are on purpose [abstract] and there is
 * no way to build/forge them except by calling the restricted (statically
 * and dynamically) Cap.main() below which is passing all capabilities
 * to the entry point of your program.
 *)

(**************************************************************************)
(* Standard capabilities *)
(**************************************************************************)

module Console : sig
  type stdin
  type stdout
  (* stderr and logs are an "ambient" authority *)
end

module Process : sig
  (* basic stuff *)
  type argv
  type env

  (* advanced stuff *)
  type signal
  type exit

  (* See also the separate Exec.t *)
  type fork
  type thread
  type domain
end

(* read/write on root/cwd/tmp *)
module FS : sig
  type root_r
  type root_w
  type cwd_r
  type cwd_w
  type tmp_r
  type tmp_w
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

(**************************************************************************)
(* Powerbox *)
(**************************************************************************)

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
  (* advanced stuff *)
  signal : Process.signal;
  fork : Process.fork;
  exit : Process.exit;
  domain : Process.domain;
  thread : Process.thread;
}

and fs_powerbox = {
  root_r : FS.root_r;
  root_w : FS.root_w;
  cwd_r : FS.cwd_r;
  cwd_w : FS.cwd_w;
  tmp_r : FS.tmp_r;
  tmp_w : FS.tmp_w;
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

(**************************************************************************)
(* Entry point *)
(**************************************************************************)

(* Only way to access a powerbox. This must be restricted to be called
 * only from a Main.ml (or Test.ml).
 *)
val main : (powerbox -> 'a) -> 'a

(* Capabilities implemented as simple abstract types and explicit
 * parameters ("Lambda the ultimate security tool").
 *
 * Note that most of the types below are on purpose abstract and there is
 * no way to build/forge them except by calling the restricted (statically
 * and dynamically) Cap.main() below which is passing all capabilities
 * to the entry point of your program. This entry point can then restrict
 * the set of capabilities to pass to other functions by using the :> cast
 * operator.
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

  (* advanced stuff (TODO? pid? kill? split signal?)  *)
  type signal
  type exit

  (* See also the separate Exec.t *)
  type fork
  type thread
  type domain
end

(* read/write on root/cwd/tmp *)
module FS : sig
  (* LATER: could restrict to root_but_no_proc_nor_sys_nor_etc *)
  type root_r
  type root_w
  type cwd_r
  type cwd_w
  type tmp
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

type root = < root_r : FS.root_r ; root_w : FS.root_w >
type cwd = < cwd_r : FS.cwd_r ; cwd_w : FS.cwd_w >
type tmp = < tmp : FS.tmp >
type fs = < root ; cwd ; tmp >
type stdin = < stdin : Console.stdin >
type stdout = < stdout : Console.stdout >
type console = < stdin ; stdout >
type fork = < fork : Process.fork >
type domain = < domain : Process.domain >
type thread = < thread : Process.thread >
type process_multi = < fork ; domain ; thread >
type signal = < signal : Process.signal >
type exit = < exit : Process.exit >
type process_single = < signal ; exit >
type argv = < argv : Process.argv >
type env = < env : Process.env >
type process = < console ; process_single ; process_multi ; argv ; env >
type exec = < exec : Exec.t >

(* TODO: extend *)
type network = < network : Network.t >
type time = < time : Misc.time >
type random = < random : Misc.random >
type misc = < time ; random >

(* alt: called "Stdenv.Base.env" in EIO *)
type all_caps =
  < process
  ; fs (* a mix of fs and process_multi as it requires both *)
  ; exec
  ; network
  ; misc >

(* you can also pass individual capabilities like just
 * stdout with 'Console.stdout'
 *)

(* pure computation, just cpu/ram *)
type no_cap

(**************************************************************************)
(* Temporary unsafe caps to help migration *)
(**************************************************************************)
val network_caps_UNSAFE : unit -> < network : Network.t >

(**************************************************************************)
(* Entry point *)
(**************************************************************************)

(* Only way to access capabilities. This must be restricted to be called
 * only from a Main.ml (or Test.ml).
 *)
val main : (all_caps -> 'a) -> 'a

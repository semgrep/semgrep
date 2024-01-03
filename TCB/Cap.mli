(* Capabilities implemented as simple abstract types and explicit
 * parameters ("Lambda the ultimate security tool").
 *
 * Note that most of the types below are on purpose abstract and there is
 * no way to build/forge them except by calling the restricted (statically
 * and dynamically) Cap.main() below. This function is passing all capabilities
 * to the entry point of your program; this entry point can then restrict
 * the set of capabilities to pass to other functions by using the :> cast
 * operator.
 *)

(**************************************************************************)
(* Standard capabilities *)
(**************************************************************************)

module Console : sig
  type stdin
  type stdout
  type stderr
  (* logs are an "ambient" authority though *)
end

module Process : sig
  (* basic stuff *)
  type argv
  type env

  (* advanced stuff *)
  type signal
  type exit
  type pid
  type kill

  (* See also the separate Exec.t *)
  type fork
  type thread
  type domain
end

(* read/write on root|cwd|tmp|~|~.xxx| (and files/dirs mentioned in argv) *)
module FS : sig
  type root_r
  type root_w

  (* this gives also access to /proc, /sys, etc. *)
  type root_all_r
  type root_all_w
  type cwd_r
  type cwd_w
  type home_r
  type home_w
  type dotfiles_r
  type dotfiles_w

  (* not worth differentiate between tmp_r/tmp_w, anyway /tmp has 't' bit *)
  type tmp

  (* files or directories mentioned in argv *)
  type files_argv_r
  type files_argv_w
end

module Exec : sig
  (* note that you can make your own exec capability (e.g., git_exec)
   * a subtype of this one by defining your own function
   * that takes Exec.t and gives the subcapability
   *)
  type t
end

(* See also commons/Chan.ml *)
module File : sig
  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
end

module Network : sig
  (* TODO? make specific host subcapability? like semgrep_url_capa ? *)
  type t
end

(* If your program does not use those capabilities, it has the nice property
 * of being deterministic.
 *)
module Misc : sig
  type random

  (* profiling functions are an "ambient" authority though *)
  type time
end

(**************************************************************************)
(* Powerbox *)
(**************************************************************************)

(* fs *)
type root = < root_r : FS.root_r ; root_w : FS.root_w >
type root_all = < root_all_r : FS.root_all_r ; root_all_w : FS.root_all_w >
type cwd = < cwd_r : FS.cwd_r ; cwd_w : FS.cwd_w >
type home = < home_r : FS.home_r ; home_w : FS.home_w >
type dotfiles = < dotfiles_r : FS.dotfiles_r ; dotfiles_w : FS.dotfiles_w >
type tmp = < tmp : FS.tmp >

type files_argv =
  < files_argv_r : FS.files_argv_r ; files_argv_w : FS.files_argv_w >

type fs = < root ; root_all ; cwd ; home ; dotfiles ; tmp ; files_argv >

(* console *)
type stdin = < stdin : Console.stdin >
type stdout = < stdout : Console.stdout >
type stderr = < stderr : Console.stderr >
type console = < stdin ; stdout ; stderr >

(* process *)
type argv = < argv : Process.argv >
type env = < env : Process.env >
type signal = < signal : Process.signal >
type exit = < exit : Process.exit >
type pid = < pid : Process.pid >
type kill = < kill : Process.kill >
type fork = < fork : Process.fork >
type domain = < domain : Process.domain >
type thread = < thread : Process.thread >
type process_multi = < pid ; kill ; fork ; domain ; thread >
type process_single = < signal ; exit >
type process = < argv ; env ; console ; process_single ; process_multi >

(* exec *)
type exec = < exec : Exec.t >

(* network *)
type network = < network : Network.t >

(* misc *)
type time = < time : Misc.time >
type random = < random : Misc.random >
type misc = < time ; random >

(* alt: called "Stdenv.Base.env" in EIO *)
type all_caps =
  < process
  ; fs
  ; exec (* exec is a mix of fs and process_multi as it requires both *)
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
(* !!DO NOT USE!! *)
val network_caps_UNSAFE : unit -> < network : Network.t >

(**************************************************************************)
(* Entry point *)
(**************************************************************************)

(* Only way to access capabilities. This must be restricted to be called
 * only from a Main.ml (or Test.ml). In any case, it can't be called
 * twice in your program (there's a dynamic check for it).
 *)
val main : (all_caps -> 'a) -> 'a

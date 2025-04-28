(**************************************************************************)
(* Prelude *)
(**************************************************************************)
(* Capabilities implemented as simple abstract types and explicit
 * arguments/parameters, "Lambda the ultimate security tool".
 *
 * references:
 *  - https://en.wikipedia.org/wiki/Capability-based_security
 *  - "Computer Systems Security Session 6: Capabilities" accessible at
 *    https://www.youtube.com/watch?v=TQhmua7Z2cY
 *    by Zeldovich and Mickens, Fall 2014. Good introduction.
 *  - https://roscidus.com/blog/blog/2023/04/26/lambda-capabilities/
 *
 * related work:
 *  - https://en.wikipedia.org/wiki/E_(programming_language) which
 *    itself led to Emily which was about adding capabilities in OCaml
 *    https://www.hpl.hp.com/techreports/2006/HPL-2006-116.html
 *  - EIO capabilities for network, fs, io, etc.
 *    see especially Eio_unix.Stdenv.base, (see the blog post above)
 *  - Android's permissions, iphone permissions; every mobile OS is
 *    using capabilities/permissions
 *  - Capability-based version of the Rust standard library
 *    https://github.com/bytecodealliance/cap-std
 *    I actually defined independently almost the same list of capabilities
 *    (tmp, random, fs, net) than they have
 *  - deno (a nodejs fork) sandboxed environment
 *  - TODO: "A Security Kernel Based on the Lambda-Calculus", Jonathan A. Rees,
 *    https://dspace.mit.edu/handle/1721.1/5944
 *  - TODO: "Effects, Capabilities, and Boxes"
 *    https://dl.acm.org/doi/pdf/10.1145/3527320
 *  - Effects as capabilities
 *    https://dl.acm.org/doi/10.1145/3428194
 *  - Effects in Scala
 *    https://dotty.epfl.ch/docs/reference/experimental/canthrow.html
 *  - ... lots of related work
 *
 * alt:
 *  - use an effect system, but not ready yet for OCaml
 *  - use semgrep rules, but this would be more of a blacklist approach
 *    whereas here it is more a whitelist approach
 *    update: we actually combine Cap, TCB with now semgrep rules, see
 *    the forbid_xxx.jsonnet rules in this directory.
 *
 * LATER:
 *  - exn (ability to thrown exn)
 *  - comparison? forbid polymorphic equal, forbid compare, force deriving
 *  - refs? (and globals)
 *
 * Assumed (ambient) capabilities:
 *  - The RAM (see Memory_limit.ml for some limits)
 *  - The CPU (see Time_limit.ml for some limits)
 *)

(**************************************************************************)
(* Core type *)
(**************************************************************************)

(* Note that it's important this type is not exported in Cap.mli!
 * Each capability must be seen as an independent abstract type
 *)
type cap = unit

(**************************************************************************)
(* Network *)
(**************************************************************************)

(* TODO: sub capabilities: host, url, ports, get vs post *)
module Network = struct
  type t = cap
end

(**************************************************************************)
(* FS *)
(**************************************************************************)

module FS = struct
  type readdir = cap
  type tmp = cap
end

(**************************************************************************)
(* Files *)
(**************************************************************************)

(**************************************************************************)
(* Exec *)
(**************************************************************************)

(* TODO: sub capabilities exec a particular program 'git_cmd Exec.t' *)
module Exec = struct
  type t = cap
end

(**************************************************************************)
(* Process *)
(**************************************************************************)

module Process = struct
  (* basic stuff *)
  type argv = cap

  (* advanced stuff *)
  type exit = cap
  type chdir = cap

  (* TODO: split signal? use subtypes to make alarm a subtype of signal?*)
  type signal = cap

  (* multi processes *)
  type fork = cap

  (* old: was alarm, but better rename to be consistent with memory_limit
   * See libs/process_limits/
   *)
  type time_limit = cap
  type memory_limit = cap
end

(**************************************************************************)
(* Console *)
(**************************************************************************)

(* alt: could be part of Process *)
module Console = struct
  type stdin = cap
  type stdout = cap
  type stderr = cap
end

(**************************************************************************)
(* Misc *)
(**************************************************************************)

module Misc = struct
  (* useful to be sure the program is deterministic and is not calling
   * any random generator functions.
   *)
  type random = cap
end

(**************************************************************************)
(* The powerbox *)
(**************************************************************************)
(* Entry point giving all the authories, a.k.a. the "Powerbox"
 *
 * references:
 *  - "How Emily Tamed the Caml"
 *     https://www.hpl.hp.com/techreports/2006/HPL-2006-116.html
 *
 * I was using plain records before, which was simple. However, objects,
 * which can be seen as extensible records, are nice because you can have
 * signatures like <network: Cap.Network.t; fs: Cap.FS.t; ..> without having
 * to name this type and without having to introduce yet another record
 * for the combination of those 2 capabilities.
 *
 * Objects are a bit to records what polymorphic variants are to variants,
 * that is [ taint | search ] allow to merge variants without introducing
 * an intermediate name. Polymorphic variants are extensible Sum types,
 * objects are extensible Product types!
 *)

(* fs *)
type readdir = < readdir : FS.readdir >
type tmp = < tmp : FS.tmp >
type fs = < readdir ; tmp >

(* console *)
type stdin = < stdin : Console.stdin >
type stdout = < stdout : Console.stdout >
type stderr = < stderr : Console.stderr >
type console = < stdin ; stdout ; stderr >

(* process *)
type argv = < argv : Process.argv >
type signal = < signal : Process.signal >
type time_limit = < time_limit : Process.time_limit >
type memory_limit = < memory_limit : Process.memory_limit >
type exit = < exit : Process.exit >
type chdir = < chdir : Process.chdir >
type fork = < fork : Process.fork >
type process_multi = < fork >
type process_single = < signal ; time_limit ; memory_limit ; exit ; chdir >
type process = < argv ; console ; process_single ; process_multi >

(* exec *)
type exec = < exec : Exec.t >

(* networl *)
type network = < network : Network.t >

(* misc *)
type random = < random : Misc.random >
type misc = < random >

(* alt: called "Stdenv.Base.env" in EIO *)
type all_caps =
  < process
  ; fs (* a mix of fs and process_multi as it requires both *)
  ; exec
  ; network
  ; misc >

type no_caps = < >

let no_caps : no_caps = object end

class powerbox =
  object
    (* fs *)
    method readdir = ()
    method tmp = ()

    (* console *)
    method stdin = ()
    method stdout = ()
    method stderr = ()

    (* process *)
    method argv = ()
    method chdir = ()
    method signal = ()
    method time_limit = ()
    method memory_limit = ()
    method fork = ()
    method exit = ()

    (* misc *)
    method random = ()

    (* dangerous stuff *)
    method exec = ()
    method network = ()
  end

(**************************************************************************)
(* Temporary unsafe caps to help migration *)
(**************************************************************************)

(* !!DO NOT USE!! *)
let network_caps_UNSAFE () =
  object
    method network = ()
  end

(* !!DO NOT USE!! *)
let tmp_caps_UNSAFE () =
  object
    method tmp = ()
  end

(* !!DO NOT USE!! *)
let stdout_caps_UNSAFE () =
  object
    method stdout = ()
  end

(* !!DO NOT USE!! *)
let caps_for_js_UNSAFE () =
  object
    method fork = ()
    method time_limit = ()
    method memory_limit = ()
    method readdir = ()
  end

(* !!DO NOT USE!! *)
let exec_and_tmp_caps_UNSAFE () =
  object
    method exec = ()
    method tmp = ()
  end

let readdir_UNSAFE () =
  object
    method readdir = ()
  end

(**************************************************************************)
(* Entry point *)
(**************************************************************************)

let already_called_main = ref false

(* TODO: in addition to the dynamic check below, we could also
 * write a semgrep rule to forbid any call to Cap.main() except
 * in Main.ml (via a nosemgrep or paths: exclude:)
 *)
let main (f : all_caps -> 'a) : 'a =
  Memtrace.trace_if_requested ();
  (* can't cheat :) can't nest them *)
  let powerbox = new powerbox in
  if !already_called_main then failwith "Cap.main() already called"
  else (
    already_called_main := true;
    f powerbox)

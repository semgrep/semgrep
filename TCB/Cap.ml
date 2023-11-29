(**************************************************************************)
(* Prelude *)
(**************************************************************************)
(* Capabilities implemented as simple abstract types and explicit
 * parameters ("Lambda the ultimate security tool").
 *
 * references:
 *  - https://en.wikipedia.org/wiki/Capability-based_security
 *  - "Computer Systems Security Session 6: Capabilities" accessible at
 *    https://www.youtube.com/watch?v=TQhmua7Z2cY
 *    by Zeldovich and Mickens, Fall 2014. Good introduction.
 *
 * related work:
 *  - EIO capabilities for network, fs, io, etc.
 *    see especially Eio_unix.Stdenv.base, and also Thomas's blog post
 *    https://roscidus.com/blog/blog/2023/04/26/lambda-capabilities/
 *  - TODO Android's permissions? iphone permissions?
 *  - TODO lots of related work
 *
 * alt:
 *  - effect system, but not ready yet for OCaml
 *  - semgrep rules, but this would be more of a blacklist approach whereas
 *    here it is more a whitelist approach
 *
 * LATER:
 *  - exn (ability to thrown exn)
 *  - comparison
 *  - refs
 *
 * Assumed capabilities:
 *  - use RAM (see Memory_limit.ml for some limits)
 *  - use CPU (see Time_limit.ml for some limits)
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

(* TODO: read vs write, specific dir (in_chan or out_chan of opened dir *)
module FS = struct
  type root_r = cap
  type root_w = cap
  type cwd_r = cap
  type cwd_w = cap
  type tmp_r = cap
  type tmp_w = cap
end

(**************************************************************************)
(* Files *)
(**************************************************************************)

module File = struct
  (* TODO: embed also the filename in it? useful for
   * error reporting.
   * TODO? inout_channel?
   *)
  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
end

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
  type env = cap

  (* advanced stuff
   * TODO: subtypes, like timeout signal very important
   *)
  type signal = cap
  type exit = cap
  type fork = cap
  type thread = cap
  type domain = cap
end

(**************************************************************************)
(* Console *)
(**************************************************************************)

(* alt: could be part of Process *)
module Console = struct
  type stdin = cap
  type stdout = cap
  (* no stderr, ambient authority *)
end

(**************************************************************************)
(* Misc *)
(**************************************************************************)

module Misc = struct
  (* supposedely important for side-channel attack *)
  type time = cap

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
 *  - "Lambda Capabilities"
 *     https://roscidus.com/blog/blog/2023/04/26/lambda-capabilities/
 *  - TODO: "A Security Kernel Based on the Lambda-Calculus", Jonathan A. Rees,
 *    https://dspace.mit.edu/handle/1721.1/5944
 *  - TODO: "Effects, Capabilities, and Boxes"
 *    https://dl.acm.org/doi/pdf/10.1145/3527320
 *
 * I was using plain records before, which is simple. However, objects,
 * which can be seen as extensible records, are nice because you can have
 * signatures like <network: Cap.Network.t; fs: Cap.FS.t; ..> without having
 * to name this type and without having to introduce yet another record
 * for the combination of those 2 capabilities.
 *
 * Objects are a bit to records what polymorphic variants are to variants,
 * that is [ taint | search ] allow to merge variants without introducing
 * an intermediate name. Polymorphic variants are extensible Sum types,
 * objects are extensible Product type!
 *)

type fs_powerbox =
  < root_r : FS.root_r
  ; root_w : FS.root_w
  ; cwd_r : FS.cwd_r
  ; cwd_w : FS.cwd_w
  ; tmp_r : FS.tmp_r
  ; tmp_w : FS.tmp_w >

type process_powerbox =
  < stdin : Console.stdin
  ; stdout : Console.stdout
  ; argv : Process.argv
  ; env : Process.env
  ; (* advanced stuff *)
  signal : Process.signal
  ; fork : Process.fork
  ; exit : Process.exit
  ; domain : Process.domain
  ; thread : Process.thread >

type misc_powerbox = < time : Misc.time ; random : Misc.random >

(* alt: called "Stdenv.Base.env" in EIO *)
type all_caps =
  < process_powerbox
  ; fs_powerbox
  ; exec : Exec.t
  ; network : Network.t
  ; misc_powerbox >

(*
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

*)

type no_cap = unit (* better than [type no_cap = cap] :) *)

let powerbox : all_caps =
  object
    (*let fs_powerbox : fs_powerbox = object *)
    method root_r = ()
    method root_w = ()
    method cwd_r = ()
    method cwd_w = ()
    method tmp_r = ()
    method tmp_w = ()
    (*end*)

    (*let process_powerbox : process_powerbox = object *)
    method stdin = ()
    method stdout = ()
    method argv = ()
    method env = ()
    method signal = ()
    method fork = ()
    method exit = ()
    method domain = ()
    method thread = ()
    (*  end *)

    (* let misc_powerbox : misc_powerbox = object  *)
    method time = ()
    method random = ()
    (*end *)

    method exec = ()
    method network = ()
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
  (* can't cheat :) can't nest them *)
  if !already_called_main then failwith "Cap.main() already called"
  else (
    already_called_main := true;
    f powerbox)

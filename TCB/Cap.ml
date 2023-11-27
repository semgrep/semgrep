(* Capabilities.
 *
 * references:
 *  - https://en.wikipedia.org/wiki/Capability-based_security
 *  - For a good introduction, see "Computer Systems Security
 *    Session 6: Capabilities", by Zeldovich and Mickens, Fall 2014
 *    https://www.youtube.com/watch?v=TQhmua7Z2cY
 *
 * related work:
 *  - EIO capabilities for network, fs, etc.
 *    see especially Eio_unix.Stdenv.base, and also
 *    https://roscidus.com/blog/blog/2023/04/26/lambda-capabilities/
 *  - a lot
 *
 * Assumed capabilities:
 *  - use RAM (see Memory_limit.ml for some limits)
 *  - use CPU (see Time_limit.ml for some limits)
 *)

(**************************************************************************)
(* Network *)
(**************************************************************************)

(* TODO: sub capabilities: host, url, ports, get vs post *)
module Network = struct
  type t = unit
end

(**************************************************************************)
(* FS *)
(**************************************************************************)

(* TODO: read vs write, specific dir (in_chan or out_chan of opened dir *)
module FS = struct
  type root_read = unit
  type root_write = unit
  type cwd_read = unit
  type cwd_write = unit
  type tmp_read = unit
  type tmp_write = unit
end

(**************************************************************************)
(* Files *)
(**************************************************************************)

module File = struct
  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
end

(**************************************************************************)
(* Exec *)
(**************************************************************************)

(* TODO: sub capabilities exec a particular program *)
module Exec = struct
  type t = unit
end

(**************************************************************************)
(* Console *)
(**************************************************************************)

module Console = struct
  type stdin = unit
  type stdout = unit
  (* no stderr *)
end

(**************************************************************************)
(* The powerbox *)
(**************************************************************************)
(* Entry point giving all the authories, a.k.a. the "Powerbox".
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
 *)

type fs_powerbox = {
  root_read : FS.root_read;
  root_write : FS.root_write;
  cwd_read : FS.cwd_read;
  cwd_write : FS.cwd_write;
  tmp_read : FS.tmp_read;
  tmp_write : FS.tmp_write;
}

(* called "env" in EIO *)
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
type nocap = unit

let fs_powerbox =
  {
    root_read = ();
    root_write = ();
    cwd_read = ();
    cwd_write = ();
    tmp_read = ();
    tmp_write = ();
  }

let powerbox =
  { stdin = (); stdout = (); fs = fs_powerbox; exec = (); network = () }

let main (f : powerbox -> 'a) : 'a = f powerbox

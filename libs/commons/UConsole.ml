(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)

let isatty () =
  !ANSITerminal.isatty Unix.stdout && !ANSITerminal.isatty Unix.stderr

let with_setup highlight_setting func =
  Console.with_highlight_setting ~isatty:(isatty ()) highlight_setting func

let mtx = Mutex.create ()
let print str = Mutex.protect mtx (fun () -> Printf.printf "%s\n%!" str)
let print_no_nl str = Mutex.protect mtx (fun () -> Printf.printf "%s%!" str)
let eprint str = Mutex.protect mtx (fun () -> Printf.eprintf "%s\n%!" str)

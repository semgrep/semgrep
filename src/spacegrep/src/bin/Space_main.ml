(*
   Copyright (c) 2020-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Select spacegrep or spacecat behavior depending on the command name.
   This allows us to build a single executable, which is more compact
   than two.

   Cmdliner would let us create a single 'space' executable that would be
   called as 'space grep', 'space cat', 'space-grep', 'spacegrep', etc.
   but it may be confusing for users. Instead, we default to spacegrep
   behavior if the command name is unrecognized and we don't show spacecat
   on the spacegrep/default help page.
*)

let dispatch () =
  Printexc.record_backtrace true;
  match Filename.basename Sys.argv.(0) with
  | "spacecat"
  | "space-cat" ->
      Spacecat_main.main ()
  | _ -> Spacegrep_main.main ()

let () = dispatch ()

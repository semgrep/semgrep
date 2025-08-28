(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let with_setenv envvar str f =
  let old = Sys.getenv_opt envvar in
  Unix.putenv envvar str;
  Common.finalize f (fun () ->
      match old with
      | Some str -> Unix.putenv envvar str
      (* ugly: Unix does not provide unsetenv,
       * see https://discuss.ocaml.org/t/unset-environment-variable/9025
       *)
      | None -> Unix.putenv envvar "")

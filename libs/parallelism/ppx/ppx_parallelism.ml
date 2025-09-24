(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* This ppx adds the ability to annotate functions with Concurrent.maybe_yield
   before the function is entered. This can be done via [@@maybe_yield]. If you
   want to annotate the top level functions of an entire module, you can use
   [@@@maybe_yield "auto"], and any functions following that will be annotated.
   To disable the auto annotation, you can use [@@@maybe_yield "auto-off"].
   Finally, if --auto is passed to the ppx in a dune file, all top level modules
   of the dune library/executable will be annotated. See Unit_PPX for examples *)

(* This ppx is a heavy modification of the landmarks ppx
   https://github.com/LexiFi/landmarks/tree/master/ppx *)

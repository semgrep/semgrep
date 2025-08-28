(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val visit_formula : (Rule.formula -> unit) -> Rule.formula -> unit

val visit_xpatterns :
  (Xpattern.t -> inside:bool -> unit) -> Rule.formula -> unit

val xpatterns_of_rule : Rule.t -> Xpattern.t list

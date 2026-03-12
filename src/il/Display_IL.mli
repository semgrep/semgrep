(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* use internally graphviz 'dot' and 'ghostview' on X11 or 'open' in mac *)
val pp_cfg : Format.formatter -> ?title:string -> IL.cfg -> unit
val display_cfg : ?title:string -> IL.cfg -> unit
val short_string_of_node_kind : IL.node_kind -> string
val short_string_of_node : IL.node -> string
val string_of_offset_list : IL.offset list -> string
val string_of_lval : IL.lval -> string
val string_of_exp : IL.exp -> string

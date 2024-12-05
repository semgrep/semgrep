(* use internally graphviz 'dot' and 'ghostview' on X11 or 'open' in mac *)
val display_cfg : < Cap.exec > -> IL.cfg -> unit
val short_string_of_node_kind : IL.node_kind -> string
val string_of_offset_list : IL.offset list -> string
val string_of_lval : IL.lval -> string
val string_of_exp : IL.exp -> string

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
type replacement_ctx

val of_bindings : Metavariable.bindings -> replacement_ctx
val of_out : Semgrep_output_v1_t.metavars -> replacement_ctx

val interpolate_metavars :
  ?fmt:(string -> string) -> string -> replacement_ctx -> string
(** [interpolate_metavars ?fmt msg ctx] will replace all metavars from in [msg]
    from [replacement_ctx]. Additionally, if [fmt] is supplied, it will be
    applied to the final metavar value content before it is substituted into
    [msg]. This can be used to do things like truncate the metavar content *)

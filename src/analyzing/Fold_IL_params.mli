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
(**
Fold over the variables bound by the parameters of a function definition.

Used e.g. to construct taint environments, see 'Taint_input_env'.
*)

val fold :
  ('acc ->
  AST_generic.ident ->
  AST_generic.id_info ->
  IL.param_default option (** default value *) ->
  'acc) ->
  'acc ->
  IL.param list ->
  'acc

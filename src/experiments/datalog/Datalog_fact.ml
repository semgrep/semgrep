(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml types to represent Datalog/DOOP facts.
 *
 * This was started from h_program-lang/datalog_code.ml.
 * See Datalog.io.ml to read/write those facts on disk.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* for locals, but also right now for fields, globals, constants, enum, ... *)
type var = string
type func = string
type fld = string

(* _cst_xxx, _str_line_xxx, _malloc_in_xxx_line, ... *)
type heap = string

(* _in_xxx_line_xxx_col_xxx *)
type callsite = string

(* mimics ../datalog_code.dtl top comment repeated here for convenience:

   # Abstract memory locations (also called heap objects), are mostly qualified
   # symbols (e.g 'main__foo', 'ret_main', '_cst_line2_'):
   # - each globals, functions, constants
   # - each malloc (context insensitively). will do sensitively later for
   #   malloc wrappers or maybe each malloc with certain type. (e.g. any Proc)
   #   so have some form of type sensitivty at least
   # - each locals (context insensitively first), when their addresses are taken
   # - each fields (field-based, see sep08.pdf lecture, so *.f, not x.* )
   # - array element (array insensitive, aggregation)
   # Invocations: line in the file (e.g. '_in_main_line_14_')

   assign0(dest:V, source:V) inputtuples
   assign_address (dest:V, source:V) inputtuples
   assign_deref(dest:V, source:V) inputtuples
   assign_content(dest:V, source:V) inputtuples

   parameter(f:N, z:Z, v:V) inputtuples
   return(f:N, v:V) inputtuples
   argument(i:I, z:Z, v:V) inputtuples
   call_direct(i:I, f:N) inputtuples
   call_indirect(i:I, v:V) inputtuples
   call_ret(i:I, v:V) inputtuples
   # typing!
   var_to_func(v:V, f:N) inputtuples

   assign_array_elt(dest:V, source:V) inputtuples
   assign_array_element_address(dest:V, source:V) inputtuples
   assign_array_deref(a:V, v:V) inputtuples

   assign_load_field(dest:V, source:V, fld:F) inputtuples
   assign_store_field(dest:V, fld:F, source:V) inputtuples
   assign_field_address(dest:V, source:V, fld:F) inputtuples
   # typing!
   field_to_var(fld:F, v:V) inputtuples
   #field_point_to?? hmm maybe once we differentiate objects heap
   # and not do just *.f

   point_to0(v:V, h:V) inputtuples

   point_to(v:V, h:V) outputtuples
   call_edge(i:I, f:N) outputtuples
   assign(dest:V, source:V)

   # the data we really care to export
   PointingData (v:V, h:V) outputtuples
   CallingData (i:I, f:N) outputtuples
*)
type fact =
  | PointTo of var * heap
  | Assign of var * var
  | AssignContent of var * var
  | AssignAddress of var * var
  | AssignDeref of var * var
  | AssignLoadField of var * var * fld
  | AssignStoreField of var * fld * var
  | AssignFieldAddress of var * var * fld
  | AssignArrayElt of var * var
  | AssignArrayDeref of var * var
  | AssignArrayElementAddress of var * var
  | Parameter of func * int * var
  | Return of func * var (* ret_xxx convention *)
  | Argument of callsite * int * var
  | ReturnValue of callsite * var
  | CallDirect of callsite * func
  | CallIndirect of callsite * var

(* alias *)
type t = fact
type facts = fact list

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

(* see datalog_code.dl domain *)
type value = V of var | F of fld | N of func | I of callsite | Z of int

let string_of_value = function
  | V x
  | F x
  | N x
  | I x ->
      x
  | Z _ -> raise Impossible

type _rule = string
type _meta_fact = string * value list

let meta_fact = function
  | PointTo (a, b) -> ("point_to", [ V a; V b ])
  | Assign (a, b) -> ("assign", [ V a; V b ])
  | AssignContent (a, b) -> ("assign_content", [ V a; V b ])
  | AssignAddress (a, b) -> ("assign_address", [ V a; V b ])
  | AssignDeref (a, b) -> ("assign_deref", [ V a; V b ])
  | AssignLoadField (a, b, c) -> ("assign_load_field", [ V a; V b; F c ])
  | AssignStoreField (a, b, c) -> ("assign_store_field", [ V a; F b; V c ])
  | AssignFieldAddress (a, b, c) -> ("assign_field_address", [ V a; V b; F c ])
  | AssignArrayElt (a, b) -> ("assign_array_elt", [ V a; V b ])
  | AssignArrayDeref (a, b) -> ("assign_array_deref", [ V a; V b ])
  | AssignArrayElementAddress (a, b) ->
      ("assign_array_element_address", [ V a; V b ])
  | Parameter (a, b, c) -> ("parameter", [ N a; Z b; V c ])
  | Return (a, b) -> ("return", [ N a; V b ])
  | Argument (a, b, c) -> ("argument", [ I a; Z b; V c ])
  | ReturnValue (a, b) -> ("call_ret", [ I a; V b ])
  | CallDirect (a, b) -> ("call_direct", [ I a; N b ])
  | CallIndirect (a, b) -> ("call_indirect", [ I a; V b ])

(*****************************************************************************)
(* Toy datalog *)
(*****************************************************************************)

let string_of_fact fact =
  let str, xs = meta_fact fact in
  spf "%s(%s)" str
    (xs
    |> List_.map (function
         | V x
         | F x
         | N x
         | I x ->
             spf "'%s'" x
         | Z i -> spf "%d" i)
    |> String.concat ", ")

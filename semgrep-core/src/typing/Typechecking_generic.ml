(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open AST_generic
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Poor's man typechecker on literals (for now).
 *
 * todo:
 *  - local type inference on AST generic? good coverage?
 *  - we could allow metavars on the type itself, as in
 *    foo($X: $T) ... $T x; ...
 *    which would require to transform the code in the generic_vs_generic
 *    style as typechecking could also bind metavariables in the process
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* very Python specific for now where the type is currently an OT_Expr
 * TODO: fill AST_generic.expr_to_type at least.
 *)
let compatible_type t e =
  match (t.G.t, e.G.e) with
  | TyExpr { e = N (Id (("int", _tok), _idinfo)); _ }, L (Int _) -> true
  | TyExpr { e = N (Id (("float", _tok), _idinfo)); _ }, L (Float _) -> true
  | TyExpr { e = N (Id (("str", _tok), _idinfo)); _ }, L (String _) -> true
  | TyBuiltin (t1, _), N (Id (_, { id_type; _ })) -> (
      match !id_type with
      | Some { t = TyBuiltin (t2, _); _ } -> t1 = t2
      | _ -> false)
  | TyN (Id ((t1, _), _)), N (Id (_, { id_type; _ })) -> (
      match !id_type with
      | Some { t = TyN (Id ((t2, _), _)); _ } -> t1 = t2
      | _ -> false)
  | TyArray (_, { t = TyN (Id ((t1, _), _)); _ }), N (Id (_, { id_type; _ }))
    -> (
      match !id_type with
      | Some { t = TyArray (_, { t = TyN (Id ((t2, _), _)); _ }); _ } -> t1 = t2
      | _ -> false)
  | _ -> false

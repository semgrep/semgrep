(* Copyright (C) 2019-2023 Semgrep Inc.
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
module G = AST_generic

let wrap_type_expr lang str =
  match lang with
  (* `x` is a placeholder and won't be used during unwrapping. *)
  | Lang.Java -> Some (spf "(%s x)" str)
  | Lang.Python -> Some (spf "x: %s" str)
  | Lang.Go -> Some (spf "var x %s" str)
  | Lang.Kotlin -> Some (spf "x as %s" str)
  | Lang.Scala -> Some (spf "x.asInstanceOf[%s]" str)
  (* for php, casting expression only allows primitive types so we use func def instead. *)
  | Lang.Php -> Some (spf "function foo(%s $x) {}" str)
  | Lang.Ts -> Some (spf "x as %s" str)
  | Lang.Csharp -> Some (spf "x as %s" str)
  | Lang.Rust -> Some (spf "x as %s" str)
  | Lang.Move_on_sui -> Some (spf "(x : %s)" str)
  | Lang.Move_on_aptos -> Some (spf "(x : %s)" str)
  | Lang.Julia -> Some (spf "x :: %s" str)
  | Lang.Cpp -> Some (spf "(%s) x" str)
  | Lang.C -> Some (spf "(%s) x" str)
  | _ -> None

let unwrap_type_expr lang expr =
  match (lang, expr) with
  | Lang.Java, G.E { e = G.TypedMetavar (_, _, t); _ } -> Some t
  | Lang.Python, G.S { s = G.DefStmt (_, VarDef { vtype = Some t; _ }); _ } ->
      Some t
  | Lang.Go, G.S { s = G.DefStmt (_, VarDef { vtype = Some t; _ }); _ } ->
      Some t
  | Lang.Kotlin, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.Scala, G.E { e = OtherExpr (_, _ :: T t :: _); _ } -> Some t
  | ( Lang.Php,
      G.S
        {
          s =
            G.DefStmt
              ( _,
                FuncDef { fparams = _, Param { ptype = Some t; _ } :: [], _; _ }
              );
          _;
        } ) ->
      Some t
  | Lang.Ts, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.Csharp, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.Rust, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.Move_on_sui, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.Move_on_aptos, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.Julia, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.Cpp, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | Lang.C, G.E { e = G.Cast (t, _, _); _ } -> Some t
  | _ -> None

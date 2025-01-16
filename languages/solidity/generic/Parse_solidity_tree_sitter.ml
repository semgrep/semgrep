(* Yoann Padioleau
 *
 * Copyright (c) 2021-2022 Semgrep Inc.
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
open Either_
open Fpath_.Operators
module CST = Tree_sitter_solidity.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Solidity parser using tree-sitter-lang/semgrep-solidity and converting
 * directly to AST_generic.ml
 *
 * related work:
 *  - https://github.com/OCamlPro/ocaml-solidity which contains not only a parser
 *    but also a typechecker!
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket
let fake s = Tok.unsafe_fake_tok s

let map_trailing_comma env v =
  match v with
  | Some tok -> (* "," *) token env tok |> ignore
  | None -> ()

let tuple_hole_expr _env tok = OtherExpr (("TupleHole", tok), []) |> G.e
let tuple_hole_pat _env tok = PatWildcard tok |> G.p

let stmt_of_def_or_dir = function
  | Either_.Left3 def -> DefStmt def |> G.s
  | Either_.Right3 dir -> DirectiveStmt dir |> G.s
  | Either_.Middle3 ellipsis_tok -> G.exprstmt (G.e (Ellipsis ellipsis_tok))

(* TODO: we should also adjust the pos for 't' *)
let left_strip_space (s, t) =
  if s =~ "^ +\\(.*\\)$" then (Common.matched1 s, t) else (s, t)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-solidity/Boilerplate.ml *)

let map_uint (env : env) (x : CST.uint) =
  match x with
  | `Uint tok -> (* "uint" *) str env tok
  | `Uint8 tok -> (* "uint8" *) str env tok
  | `Uint16 tok -> (* "uint16" *) str env tok
  | `Uint24 tok -> (* "uint24" *) str env tok
  | `Uint32 tok -> (* "uint32" *) str env tok
  | `Uint40 tok -> (* "uint40" *) str env tok
  | `Uint48 tok -> (* "uint48" *) str env tok
  | `Uint56 tok -> (* "uint56" *) str env tok
  | `Uint64 tok -> (* "uint64" *) str env tok
  | `Uint72 tok -> (* "uint72" *) str env tok
  | `Uint80 tok -> (* "uint80" *) str env tok
  | `Uint88 tok -> (* "uint88" *) str env tok
  | `Uint96 tok -> (* "uint96" *) str env tok
  | `Uint104 tok -> (* "uint104" *) str env tok
  | `Uint112 tok -> (* "uint112" *) str env tok
  | `Uint120 tok -> (* "uint120" *) str env tok
  | `Uint128 tok -> (* "uint128" *) str env tok
  | `Uint136 tok -> (* "uint136" *) str env tok
  | `Uint144 tok -> (* "uint144" *) str env tok
  | `Uint152 tok -> (* "uint152" *) str env tok
  | `Uint160 tok -> (* "uint160" *) str env tok
  | `Uint168 tok -> (* "uint168" *) str env tok
  | `Uint176 tok -> (* "uint176" *) str env tok
  | `Uint184 tok -> (* "uint184" *) str env tok
  | `Uint192 tok -> (* "uint192" *) str env tok
  | `Uint200 tok -> (* "uint200" *) str env tok
  | `Uint208 tok -> (* "uint208" *) str env tok
  | `Uint216 tok -> (* "uint216" *) str env tok
  | `Uint224 tok -> (* "uint224" *) str env tok
  | `Uint232 tok -> (* "uint232" *) str env tok
  | `Uint240 tok -> (* "uint240" *) str env tok
  | `Uint248 tok -> (* "uint248" *) str env tok
  | `Uint256 tok -> (* "uint256" *) str env tok

let map_number_unit (env : env) (x : CST.number_unit) =
  match x with
  | `Wei tok -> (* "wei" *) str env tok
  | `Szabo tok -> (* "szabo" *) str env tok
  | `Finney tok -> (* "finney" *) str env tok
  | `Gwei tok -> (* "gwei" *) str env tok
  | `Ether tok -> (* "ether" *) str env tok
  | `Seconds tok -> (* "seconds" *) str env tok
  | `Minutes tok -> (* "minutes" *) str env tok
  | `Hours tok -> (* "hours" *) str env tok
  | `Days tok -> (* "days" *) str env tok
  | `Weeks tok -> (* "weeks" *) str env tok
  | `Years tok -> (* "years" *) str env tok

let map_state_mutability (env : env) (x : CST.state_mutability) : attribute =
  match x with
  (* less: Const? *)
  | `Pure tok -> (* "pure" *) str env tok |> G.unhandled_keywordattr
  | `View tok -> (* "view" *) str env tok |> G.unhandled_keywordattr
  | `Paya tok -> (* "payable" *) str env tok |> G.unhandled_keywordattr

let map_yul_or_literal_boolean (env : env) x : bool wrap =
  match x with
  | `True tok -> (* "true" *) (true, token env tok)
  | `False tok -> (* "false" *) (false, token env tok)

let map_solidity_version_comparison_operator (env : env)
    (x : CST.solidity_version_comparison_operator) : operator wrap =
  match x with
  | `LTEQ tok -> (* "<=" *) (LtE, token env tok)
  | `LT tok -> (* "<" *) (Lt, token env tok)
  | `HAT tok -> (* "^" *) (BitXor, token env tok)
  | `GT tok -> (* ">" *) (Gt, token env tok)
  | `GTEQ tok -> (* ">=" *) (GtE, token env tok)
  | `TILDE tok -> (* "~" *) (BitNot, token env tok)
  | `EQ tok -> (* "=" *) (Eq, token env tok)

let map_storage_location (env : env) (x : CST.storage_location) : attribute =
  match x with
  | `Memory tok -> (* "memory" *) str env tok |> G.unhandled_keywordattr
  | `Stor tok -> (* "storage" *) str env tok |> G.unhandled_keywordattr
  | `Call tok -> (* "calldata" *) str env tok |> G.unhandled_keywordattr

let map_visibility (env : env) (x : CST.visibility) : attribute =
  match x with
  | `Public tok ->
      let x = (* "public" *) token env tok in
      G.attr Public x
  | `Inte tok ->
      let x = (* "internal" *) token env tok in
      G.attr Static x
  | `Priv tok ->
      let x = (* "private" *) token env tok in
      G.attr Private x
  | `Exte tok ->
      let x = (* "external" *) token env tok in
      G.attr Extern x

let map_yul_evm_builtin (env : env) (x : CST.yul_evm_builtin) =
  match x with
  | `Stop tok -> (* "stop" *) str env tok
  | `Add tok -> (* "add" *) str env tok
  | `Sub tok -> (* "sub" *) str env tok
  | `Mul tok -> (* "mul" *) str env tok
  | `Div tok -> (* "div" *) str env tok
  | `Sdiv tok -> (* "sdiv" *) str env tok
  | `Mod tok -> (* "mod" *) str env tok
  | `Smod tok -> (* "smod" *) str env tok
  | `Exp tok -> (* "exp" *) str env tok
  | `Not tok -> (* "not" *) str env tok
  | `Lt tok -> (* "lt" *) str env tok
  | `Gt tok -> (* "gt" *) str env tok
  | `Slt tok -> (* "slt" *) str env tok
  | `Sgt tok -> (* "sgt" *) str env tok
  | `Eq tok -> (* "eq" *) str env tok
  | `Iszero tok -> (* "iszero" *) str env tok
  | `And tok -> (* "and" *) str env tok
  | `Or tok -> (* "or" *) str env tok
  | `Xor tok -> (* "xor" *) str env tok
  | `Byte tok -> (* "byte" *) str env tok
  | `Shl tok -> (* "shl" *) str env tok
  | `Shr tok -> (* "shr" *) str env tok
  | `Sar tok -> (* "sar" *) str env tok
  | `Addmod tok -> (* "addmod" *) str env tok
  | `Mulmod tok -> (* "mulmod" *) str env tok
  | `Sign tok -> (* "signextend" *) str env tok
  | `Keccak256 tok -> (* "keccak256" *) str env tok
  | `Pop tok -> (* "pop" *) str env tok
  | `Mload tok -> (* "mload" *) str env tok
  | `Mstore tok -> (* "mstore" *) str env tok
  | `Mstore8 tok -> (* "mstore8" *) str env tok
  | `Sload tok -> (* "sload" *) str env tok
  | `Sstore tok -> (* "sstore" *) str env tok
  | `Msize tok -> (* "msize" *) str env tok
  | `Gas tok -> (* "gas" *) str env tok
  | `Addr tok -> (* "address" *) str env tok
  | `Bala tok -> (* "balance" *) str env tok
  | `Self_e34af40 tok -> (* "selfbalance" *) str env tok
  | `Caller tok -> (* "caller" *) str env tok
  | `Call_17bffc7 tok -> (* "callvalue" *) str env tok
  | `Call_b766e35 tok -> (* "calldataload" *) str env tok
  | `Call_ee2b8b2 tok -> (* "calldatasize" *) str env tok
  | `Call_9211e8b tok -> (* "calldatacopy" *) str env tok
  | `Extc_8cf31ff tok -> (* "extcodesize" *) str env tok
  | `Extc_097e5c5 tok -> (* "extcodecopy" *) str env tok
  | `Retu_6316777 tok -> (* "returndatasize" *) str env tok
  | `Retu_0c570b4 tok -> (* "returndatacopy" *) str env tok
  | `Extc_d7340e7 tok -> (* "extcodehash" *) str env tok
  | `Create tok -> (* "create" *) str env tok
  | `Create2 tok -> (* "create2" *) str env tok
  | `Call_53b9e96 tok -> (* "call" *) str env tok
  | `Call_bebd5bc tok -> (* "callcode" *) str env tok
  | `Dele tok -> (* "delegatecall" *) str env tok
  | `Stat tok -> (* "staticcall" *) str env tok
  | `Ret tok -> (* "return" *) str env tok
  | `Revert tok -> (* "revert" *) str env tok
  | `Self_482b767 tok -> (* "selfdestruct" *) str env tok
  | `Inva tok -> (* "invalid" *) str env tok
  | `Log0 tok -> (* "log0" *) str env tok
  | `Log1 tok -> (* "log1" *) str env tok
  | `Log2 tok -> (* "log2" *) str env tok
  | `Log3 tok -> (* "log3" *) str env tok
  | `Log4 tok -> (* "log4" *) str env tok
  | `Chai tok -> (* "chainid" *) str env tok
  | `Origin tok -> (* "origin" *) str env tok
  | `Gasp tok -> (* "gasprice" *) str env tok
  | `Bloc tok -> (* "blockhash" *) str env tok
  | `Coin tok -> (* "coinbase" *) str env tok
  | `Time tok -> (* "timestamp" *) str env tok
  | `Num tok -> (* "number" *) str env tok
  | `Diff tok -> (* "difficulty" *) str env tok
  | `Gasl tok -> (* "gaslimit" *) str env tok

let map_bytes_ (env : env) (x : CST.bytes_) =
  match x with
  | `Byte tok -> (* "byte" *) str env tok
  | `Bytes tok -> (* "bytes" *) str env tok
  | `Bytes1 tok -> (* "bytes1" *) str env tok
  | `Bytes2 tok -> (* "bytes2" *) str env tok
  | `Bytes3 tok -> (* "bytes3" *) str env tok
  | `Bytes4 tok -> (* "bytes4" *) str env tok
  | `Bytes5 tok -> (* "bytes5" *) str env tok
  | `Bytes6 tok -> (* "bytes6" *) str env tok
  | `Bytes7 tok -> (* "bytes7" *) str env tok
  | `Bytes8 tok -> (* "bytes8" *) str env tok
  | `Bytes9 tok -> (* "bytes9" *) str env tok
  | `Bytes10 tok -> (* "bytes10" *) str env tok
  | `Bytes11 tok -> (* "bytes11" *) str env tok
  | `Bytes12 tok -> (* "bytes12" *) str env tok
  | `Bytes13 tok -> (* "bytes13" *) str env tok
  | `Bytes14 tok -> (* "bytes14" *) str env tok
  | `Bytes15 tok -> (* "bytes15" *) str env tok
  | `Bytes16 tok -> (* "bytes16" *) str env tok
  | `Bytes17 tok -> (* "bytes17" *) str env tok
  | `Bytes18 tok -> (* "bytes18" *) str env tok
  | `Bytes19 tok -> (* "bytes19" *) str env tok
  | `Bytes20 tok -> (* "bytes20" *) str env tok
  | `Bytes21 tok -> (* "bytes21" *) str env tok
  | `Bytes22 tok -> (* "bytes22" *) str env tok
  | `Bytes23 tok -> (* "bytes23" *) str env tok
  | `Bytes24 tok -> (* "bytes24" *) str env tok
  | `Bytes25 tok -> (* "bytes25" *) str env tok
  | `Bytes26 tok -> (* "bytes26" *) str env tok
  | `Bytes27 tok -> (* "bytes27" *) str env tok
  | `Bytes28 tok -> (* "bytes28" *) str env tok
  | `Bytes29 tok -> (* "bytes29" *) str env tok
  | `Bytes30 tok -> (* "bytes30" *) str env tok
  | `Bytes31 tok -> (* "bytes31" *) str env tok
  | `Bytes32 tok -> (* "bytes32" *) str env tok

let map_anon_choice_PLUSPLUS_e498e28 (env : env)
    (x : CST.anon_choice_PLUSPLUS_e498e28) =
  match x with
  | `PLUSPLUS tok -> (* "++" *) (Incr, token env tok)
  | `DASHDASH tok -> (* "--" *) (Decr, token env tok)

let map_int_ (env : env) (x : CST.int_) =
  match x with
  | `Int tok -> (* "int" *) str env tok
  | `Int8 tok -> (* "int8" *) str env tok
  | `Int16 tok -> (* "int16" *) str env tok
  | `Int24 tok -> (* "int24" *) str env tok
  | `Int32 tok -> (* "int32" *) str env tok
  | `Int40 tok -> (* "int40" *) str env tok
  | `Int48 tok -> (* "int48" *) str env tok
  | `Int56 tok -> (* "int56" *) str env tok
  | `Int64 tok -> (* "int64" *) str env tok
  | `Int72 tok -> (* "int72" *) str env tok
  | `Int80 tok -> (* "int80" *) str env tok
  | `Int88 tok -> (* "int88" *) str env tok
  | `Int96 tok -> (* "int96" *) str env tok
  | `Int104 tok -> (* "int104" *) str env tok
  | `Int112 tok -> (* "int112" *) str env tok
  | `Int120 tok -> (* "int120" *) str env tok
  | `Int128 tok -> (* "int128" *) str env tok
  | `Int136 tok -> (* "int136" *) str env tok
  | `Int144 tok -> (* "int144" *) str env tok
  | `Int152 tok -> (* "int152" *) str env tok
  | `Int160 tok -> (* "int160" *) str env tok
  | `Int168 tok -> (* "int168" *) str env tok
  | `Int176 tok -> (* "int176" *) str env tok
  | `Int184 tok -> (* "int184" *) str env tok
  | `Int192 tok -> (* "int192" *) str env tok
  | `Int200 tok -> (* "int200" *) str env tok
  | `Int208 tok -> (* "int208" *) str env tok
  | `Int216 tok -> (* "int216" *) str env tok
  | `Int224 tok -> (* "int224" *) str env tok
  | `Int232 tok -> (* "int232" *) str env tok
  | `Int240 tok -> (* "int240" *) str env tok
  | `Int248 tok -> (* "int248" *) str env tok
  | `Int256 tok -> (* "int256" *) str env tok

let map_identifier_path (env : env) ((v1, v2) : CST.identifier_path) : name =
  let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "." *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        v2)
      v2
  in
  H2.name_of_ids (v1 :: v2)

let map_yul_path (env : env) ((v1, v2) : CST.yul_path) : name =
  let v1 = (* pattern [a-zA-Z$_]+ *) str env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "." *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
        v2)
      v2
  in
  H2.name_of_ids (v1 :: v2)

let map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e (env : env)
    ((v1, v2, v3) : CST.anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e) :
    ident list =
  let v1 = (* pattern [a-zA-Z$_]+ *) str env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
        v2)
      v2
  in
  let _v3 = map_trailing_comma env v3 in
  v1 :: v2

(* TODO: in the grammar there is a PREC.USER_TYPE which makes every
 * primary expression to switch to this instead of a simple identifier,
 * so take care!
 *)
let map_user_defined_type (env : env) ((v1, v2) : CST.user_defined_type) : name
    =
  let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "." *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        v2)
      v2
  in
  let ids = v1 :: v2 in
  let n = H2.name_of_ids ids in
  (* actually not always a type, so better to just return the name instead
   * of TyN n |> G.t
   *)
  n

let map_import_declaration (env : env) ((v1, v2) : CST.import_declaration) =
  let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = (* "as" *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        Some v2
    | None -> None
  in
  (v1, v2)

let map_decimal_number (env : env) (x : CST.decimal_number) =
  match x with
  | `Pat_585ba4d tok ->
      let s, t = (* pattern \d+(\.\d+)?([eE](-)?\d+)? *) str env tok in
      (float_of_string_opt s, t)
  | `Pat_ac20a0c tok ->
      let s, t = (* pattern \.\d+([eE](-)?\d+)? *) str env tok in
      (float_of_string_opt s, t)

let map_pragma_version_constraint (env : env)
    (v : CST.pragma_version_constraint) : expr =
  match v with
  | `Opt_soli_vers_comp_op_soli_vers (v1, v2) -> (
      let ver =
        (* pattern \d+(.\d+(.\d+)?)? *) str env v2 |> left_strip_space
      in
      let e = G.L (G.String (fb ver)) |> G.e in
      match v1 with
      | Some x ->
          let op, t = map_solidity_version_comparison_operator env x in
          G.opcall (op, t) [ e ]
      | None -> e)
  | `Opt_soli_vers_comp_op_id (v1, v2) -> (
      (* TODO? not sure why we need those left_strip_space here; the grammar seems
       * ok but in practice the identifier has a leading space
       *)
      let id = str env v2 |> left_strip_space in
      let e = G.N (H2.name_of_id id) |> G.e in
      match v1 with
      | Some x ->
          let op, t = map_solidity_version_comparison_operator env x in
          G.opcall (op, t) [ e ]
      | None -> e)

let map_fixed (env : env) (x : CST.fixed) =
  match x with
  | `Fixed tok -> (* "fixed" *) str env tok
  | `Pat_f2662db tok -> (* pattern fixed([0-9]+)x([0-9]+) *) str env tok

let map_anon_rep_opt___hex_digit_c87bea1 (env : env)
    (xs : CST.anon_rep_opt___hex_digit_c87bea1) : string wrap list =
  List_.map
    (fun (v1, v2) ->
      let _v1 =
        match v1 with
        | Some tok -> (* "_" *) [ token env tok ]
        | None -> []
      in
      let v2 = (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) str env v2 in
      v2)
    xs

let map_ufixed (env : env) (x : CST.ufixed) =
  match x with
  | `Ufixed tok -> (* "ufixed" *) str env tok
  | `Pat_accdbe2 tok -> (* pattern ufixed([0-9]+)x([0-9]+) *) str env tok

let map_double_quoted_unicode_char (env : env)
    (x : CST.double_quoted_unicode_char) =
  str env x

let map_single_quoted_unicode_char (env : env)
    (x : CST.single_quoted_unicode_char) =
  str env x

let map_string_ (env : env) (x : CST.string_) : string wrap bracket =
  match x with
  | `DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT (v1, v2, v3) ->
      let l = (* "\"" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_double_quote tok ->
                (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "\"" *) token env v3 in
      G.string_ (l, xs, r)
  | `SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT (v1, v2, v3) ->
      let l = (* "'" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_quote tok ->
                (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "'" *) token env v3 in
      G.string_ (l, xs, r)

let map_primitive_type (env : env) (x : CST.primitive_type) : type_ =
  match x with
  | `Addr_opt_paya (v1, v2) ->
      let v1 = (* "address" *) str env v1 in
      let v2 =
        match v2 with
        | Some tok -> (* "payable" *) [ str env tok ]
        | None -> []
      in
      let n = H2.name_of_ids (v1 :: v2) in
      G.TyN n |> G.t
  | `Bool tok ->
      let x = (* "bool" *) str env tok in
      G.ty_builtin x
  | `Str tok ->
      let x = (* "string" *) str env tok in
      G.ty_builtin x
  | `Var tok ->
      let x = (* "var" *) str env tok in
      G.ty_builtin x
  | `Int x ->
      let x = map_int_ env x in
      G.ty_builtin x
  | `Uint x ->
      let x = map_uint env x in
      G.ty_builtin x
  | `Bytes x ->
      let x = map_bytes_ env x in
      G.ty_builtin x
  | `Fixed x ->
      let x = map_fixed env x in
      G.ty_builtin x
  | `Ufixed x ->
      let x = map_ufixed env x in
      G.ty_builtin x

let map_user_defined_type_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.user_defined_type_definition) : definition =
  let _ttype = (* "type" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let _tis = (* "is" *) token env v3 in
  let ty = map_primitive_type env v4 in
  let _sc = (* ";" *) token env v5 in
  let ent = G.basic_entity id in
  let def = { tbody = AliasType (* or NewType? *) ty } in
  (ent, TypeDef def)

let map_enum_member (env : env) (x : CST.enum_member) : or_type_element =
  match x with
  | `Id tok ->
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
      OrEnum (id, None)
  | `Ellips tok ->
      let tk = (* "..." *) token env tok in
      OrEllipsis tk

let map_enum_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.enum_declaration) : definition =
  let _enumkwd = (* "enum" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let _lb = (* "{" *) token env v3 in
  let or_elems =
    match v4 with
    | Some (v1, v2, v3) ->
        let x = map_enum_member env v1 in
        let xs =
          v2
          |> List_.map (fun (v1, v2) ->
                 let _comma = token env v1 in
                 map_enum_member env v2)
        in
        let _ = map_trailing_comma env v3 in
        x :: xs
    | None -> []
  in
  let _rb = (* "}" *) token env v5 in
  let ent = G.basic_entity id in
  let def = { tbody = OrType or_elems } in
  (ent, TypeDef def)

let map_override_specifier (env : env) ((v1, v2) : CST.override_specifier) =
  let toverride = (* "override" *) token env v1 in
  let names =
    match v2 with
    | Some (v1, v2, v3, v4, v5) ->
        let lp = (* "(" *) token env v1 in
        let n = map_user_defined_type env v2 in
        let xs =
          List_.map
            (fun (v1, v2) ->
              let _tcomma = (* "," *) token env v1 in
              let n = map_user_defined_type env v2 in
              n)
            v3
        in
        let _v4 = map_trailing_comma env v4 in
        let rp = (* ")" *) token env v5 in
        Some (lp, n :: xs, rp)
    | None -> None
  in
  match names with
  | None -> G.attr Override toverride
  | Some (_l, xs, _r) ->
      OtherAttribute
        ( ("OverrideWithNames", toverride),
          xs |> List_.map (fun x -> E (N x |> G.e)) )

let map_hex_number (env : env) (x : CST.hex_number) =
  let s, t = str env x in
  Parsed_int.parse (s, t)

let map_hex_string_literal (env : env) (xs : CST.hex_string_literal) :
    (tok * string wrap bracket) list =
  List_.map
    (fun (v1, v2) ->
      let v1 = (* "hex" *) token env v1 in
      let v2 =
        match v2 with
        | `DQUOT_opt_hex_digit_rep_opt___hex_digit_DQUOT (v1, v2, v3) ->
            let l = (* "\"" *) token env v1 in
            let xs =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) str env v1 in
                  let v2 = map_anon_rep_opt___hex_digit_c87bea1 env v2 in
                  v1 :: v2
              | None -> []
            in
            let r = (* "\"" *) token env v3 in
            G.string_ (l, xs, r)
        | `SQUOT_opt_hex_digit_rep_opt___hex_digit_SQUOT (v1, v2, v3) ->
            let l = (* "'" *) token env v1 in
            let xs =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) str env v1 in
                  let v2 = map_anon_rep_opt___hex_digit_c87bea1 env v2 in
                  v1 :: v2
              | None -> []
            in
            let r = (* "'" *) token env v3 in
            G.string_ (l, xs, r)
      in
      (v1, v2))
    xs

let map_unicode_string_literal (env : env) (xs : CST.unicode_string_literal) :
    (tok * string wrap bracket) list =
  List_.map
    (fun (v1, v2) ->
      let v1 = (* "unicode" *) token env v1 in
      let v2 =
        match v2 with
        | `DQUOT_rep_double_quoted_unic_char_DQUOT (v1, v2, v3) ->
            let l = (* "\"" *) token env v1 in
            let xs = List_.map (map_double_quoted_unicode_char env) v2 in
            let r = (* "\"" *) token env v3 in
            G.string_ (l, xs, r)
        | `SQUOT_rep_single_quoted_unic_char_SQUOT (v1, v2, v3) ->
            let l = (* "'" *) token env v1 in
            let xs = List_.map (map_single_quoted_unicode_char env) v2 in
            let r = (* "'" *) token env v3 in
            G.string_ (l, xs, r)
      in
      (v1, v2))
    xs

let map_yul_string_literal (env : env) (x : CST.yul_string_literal) =
  map_string_ env x

let map_import_clause (env : env) (x : CST.import_clause) =
  match x with
  | `Single_import (v1, v2) -> (
      let v1 =
        match v1 with
        | `STAR tok -> Left ((* "*" *) token env tok)
        | `Id tok -> Right ((* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok)
      in
      let alias_opt =
        Option.map
          (fun (v1, v2) ->
            let _v1 = (* "as" *) token env v1 in
            let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
            id)
          v2
      in
      fun timport modname ->
        match (v1, alias_opt) with
        | Left tstar, None -> [ ImportAll (timport, modname, tstar) |> G.d ]
        | Left _tstar, Some alias ->
            [
              ImportAs (timport, modname, Some (alias, G.empty_id_info ()))
              |> G.d;
            ]
        | Right id, alias_opt ->
            [
              ImportFrom
                (timport, modname, [ H2.mk_import_from_kind id alias_opt ])
              |> G.d;
            ])
  | `Mult_import (v1, v2, v3) ->
      let _lb = (* "{" *) token env v1 in
      let xs =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_import_declaration env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_import_declaration env v2 in
                  v2)
                v2
            in
            let _v3 = map_trailing_comma env v3 in
            v1 :: v2
        | None -> []
      in
      let _rb = (* "}" *) token env v3 in
      fun timport modname ->
        xs
        |> List_.map (fun (id, aliasopt) ->
               ImportFrom
                 (timport, modname, [ H2.mk_import_from_kind id aliasopt ])
               |> G.d)

let map_mapping_key (env : env) (x : CST.mapping_key) : type_ =
  match x with
  | `Prim_type x -> map_primitive_type env x
  | `User_defi_type x ->
      let n = map_user_defined_type env x in
      TyN n |> G.t

let map_yul_literal (env : env) (x : CST.yul_literal) : literal =
  match x with
  | `Yul_deci_num tok ->
      (* pattern 0|([1-9][0-9]*\
         ) *)
      let s, t = str env tok in
      Int (Parsed_int.parse_c_octal (s, t))
  | `Yul_str_lit x ->
      let x = map_yul_string_literal env x in
      String x
  | `Yul_hex_num tok ->
      let s, t = (* pattern 0x[0-9A-Fa-f]* *) str env tok in
      Int (Parsed_int.parse (s, t))
  | `Yul_bool x ->
      let b = map_yul_or_literal_boolean env x in
      Bool b

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) :
    string wrap bracket =
  let _v1 = (* "from" *) token env v1 in
  let v2 = map_yul_string_literal env v2 in
  v2

let map_source_import (env : env) ((v1, v2) : CST.source_import) =
  let v1 = map_yul_string_literal env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = (* "as" *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        Some (v2, G.empty_id_info ())
    | None -> None
  in
  (v1, v2)

let map_string_literal (env : env) (xs : CST.string_literal) =
  List_.map (map_yul_string_literal env) xs

let rec map_yul_expression (env : env) (x : CST.yul_expression) : expr =
  match x with
  | `Yul_path x ->
      let n = map_yul_path env x in
      N n |> G.e
  | `Yul_func_call x -> map_yul_function_call env x
  | `Yul_lit x ->
      let x = map_yul_literal env x in
      L x |> G.e

and map_yul_function_call (env : env) (x : CST.yul_function_call) =
  match x with
  | `Choice_yul_id_LPAR_opt_yul_exp_rep_COMMA_yul_exp_opt_COMMA_RPAR
      (v1, v2, v3, v4) ->
      let operand =
        match v1 with
        | `Yul_id tok ->
            let id = (* pattern [a-zA-Z$_]+ *) str env tok in
            N (H2.name_of_id id) |> G.e
        | `Yul_evm_buil x ->
            let id = map_yul_evm_builtin env x in
            (* TODO: IdSpecial (Builtin ?) *)
            N (H2.name_of_id id) |> G.e
      in
      let lp = (* "(" *) token env v2 in
      let args =
        match v3 with
        | Some (v1, v2, v3) ->
            let v1 = map_yul_expression env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_yul_expression env v2 in
                  v2)
                v2
            in
            let _v3 = map_trailing_comma env v3 in
            v1 :: v2
        | None -> []
      in
      let rp = (* ")" *) token env v4 in
      let args = args |> List_.map G.arg in
      Call (operand, (lp, args, rp)) |> G.e
  | `Yul_evm_buil x ->
      let id = map_yul_evm_builtin env x in
      N (H2.name_of_id id) |> G.e

let map_literal (env : env) (x : CST.literal) : expr =
  match x with
  | `Str_lit x -> (
      let xs = map_string_literal env x in
      match xs with
      | [] -> raise Impossible
      | [ x ] -> L (String x) |> G.e
      (* TODO: concat them in a single String? *)
      | xs ->
          (* like in c_to_generic.ml *)
          let operand =
            G.IdSpecial (G.ConcatString G.SequenceConcat, fake " ") |> G.e
          in
          G.Call
            ( operand,
              fb (xs |> List_.map (fun x -> G.Arg (G.L (G.String x) |> G.e))) )
          |> G.e)
  | `Num_lit (v1, v2) ->
      let lit =
        match v1 with
        | `Deci_num x ->
            let fopt, t = map_decimal_number env x in
            Float (fopt, t)
        | `Hex_num x ->
            let pi = map_hex_number env x in
            Int pi
      in
      let res = L lit |> G.e in
      let res =
        match v2 with
        | Some x ->
            let s, t = map_number_unit env x in
            OtherExpr (("UnitLiteral", t), [ Str (fb (s, t)); E res ]) |> G.e
        | None -> res
      in
      res
  | `Bool_lit x ->
      let x = map_yul_or_literal_boolean env x in
      L (Bool x) |> G.e
  | `Hex_str_lit x -> (
      let xs = map_hex_string_literal env x in
      match xs with
      | [] -> raise Impossible
      | (tok_hex, _) :: _ ->
          OtherExpr
            ( ("HexString", tok_hex),
              xs
              |> List_.map (fun (thex, str) -> [ Tk thex; Str str ])
              |> List_.flatten )
          |> G.e)
  | `Unic_str_lit x -> (
      let xs = map_unicode_string_literal env x in
      match xs with
      | [] -> raise Impossible
      | (tok_unicode, _) :: _ ->
          OtherExpr
            ( ("UnicodeString", tok_unicode),
              xs
              |> List_.map (fun (tk, str) -> [ Tk tk; Str str ])
              |> List_.flatten )
          |> G.e)

let map_yul_variable_declaration (env : env) (x : CST.yul_variable_declaration)
    : definition =
  match x with
  | `Let_yul_id_opt_COLONEQ_yul_exp (v1, v2, v3) ->
      let _letkwd = (* "let" *) token env v1 in
      let id = (* pattern [a-zA-Z$_]+ *) str env v2 in
      let eopt =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = (* ":=" *) token env v1 in
            let v2 = map_yul_expression env v2 in
            Some v2
        | None -> None
      in
      let ent = G.basic_entity id in
      let vdef = { vinit = eopt; vtype = None; vtok = None } in
      (ent, VarDef vdef)
  | `Let_choice_yul_id_rep_COMMA_yul_id_opt_COMMA_opt_COLONEQ_yul_func_call
      (v1, v2, v3) ->
      let _letkwd = (* "let" *) token env v1 in
      let lp, ids, rp =
        match v2 with
        | `Yul_id_rep_COMMA_yul_id_opt_COMMA x ->
            fb (map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e env x)
        | `LPAR_yul_id_rep_COMMA_yul_id_opt_COMMA_RPAR (v1, v2, v3, v4, v5) ->
            let lp = (* "(" *) token env v1 in
            let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
            let v3 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
                  v2)
                v3
            in
            let _v4 = map_trailing_comma env v4 in
            let rp = (* ")" *) token env v5 in
            (lp, v2 :: v3, rp)
      in
      let eopt =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = (* ":=" *) token env v1 in
            let v2 = map_yul_function_call env v2 in
            Some v2
        | None -> None
      in
      (* TODO: if eopt is None we could return a list of defs *)
      let pat =
        PatTuple
          (lp, ids |> List_.map (fun id -> PatId (id, G.empty_id_info ())), rp)
        |> G.p
      in
      let ent = { name = EPattern pat; attrs = []; tparams = None } in
      let def = { vinit = eopt; vtype = None; vtok = G.no_sc } in
      (ent, VarDef def)

let map_yul_assignment_operator (env : env) (x : CST.yul_assignment_operator) =
  match x with
  | `COLONEQ tok -> (* ":=" *) token env tok
  | `COLON_EQ (v1, v2) ->
      let v1 = (* ":" *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      Tok.combine_toks v1 [ v2 ]

let map_yul_assignment (env : env) (x : CST.yul_assignment) : expr =
  match x with
  | `Yul_path_yul_assign_op_yul_exp (v1, v2, v3) ->
      let n = map_yul_path env v1 in
      let teq = map_yul_assignment_operator env v2 in
      let e = map_yul_expression env v3 in
      Assign (N n |> G.e, teq, e) |> G.e
  | `Yul_path_rep_COMMA_yul_path_opt_COMMA_opt_yul_assign_op_yul_func_call
      (v1, v2, v3, v4) ->
      let v1 = map_yul_path env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_yul_path env v2 in
            v2)
          v2
      in
      let _v3 = map_trailing_comma env v3 in
      let names = v1 :: v2 in
      let single_or_tuple =
        match names with
        | [] -> raise Impossible
        | [ x ] -> N x |> G.e
        | xs ->
            Container (Tuple, fb (xs |> List_.map (fun n -> N n |> G.e))) |> G.e
      in
      let res =
        match v4 with
        | Some (v1, v2) ->
            let teq = map_yul_assignment_operator env v1 in
            let e = map_yul_function_call env v2 in
            Assign (single_or_tuple, teq, e) |> G.e
        | None ->
            (* TODO: what is 'a, b' alone? a tuple? *)
            single_or_tuple
      in
      res

let map_solidity_pragma_token (env : env) ((v1, v2) : CST.solidity_pragma_token)
    : ident * any list =
  let idsol = (* "solidity" *) str env v1 in
  let anys =
    v2
    |> List_.map (fun (v1, v2) ->
           let e = map_pragma_version_constraint env v1 in
           (* TODO: maybe we should generate a big boolean expression
            * with 'solidity' id instead of an any list
            *)
           let v2 =
             match v2 with
             | Some x -> (
                 match x with
                 | `BARBAR tok -> (* "||" *) [ Tk (token env tok) ]
                 | `DASH tok -> (* "-" *) [ Tk (token env tok) ])
             | None -> []
           in
           [ G.E e ] @ v2)
  in
  (idsol, List_.flatten anys)

let map_pragma_value (env : env) (x : CST.pragma_value) =
  let t = token (* pattern [^;]+ *) env x in
  [ Tk t ]

let map_any_pragma_token (env : env) ((v1, v2) : CST.any_pragma_token) :
    ident * any list =
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let anys = map_pragma_value env v2 in
  (id, anys)

let map_directive (env : env) (x : CST.directive) : directive list =
  match x with
  | `Pragma_dire (v1, v2, v3) ->
      let tpragma = (* "pragma" *) token env v1 in
      let id, anys =
        match v2 with
        | `Soli_pragma_tok x -> map_solidity_pragma_token env x
        | `Any_pragma_tok x -> map_any_pragma_token env x
      in
      let sc = (* ";" *) token env v3 in
      [ Pragma (id, [ Tk tpragma ] @ anys @ [ Tk sc ]) |> G.d ]
  | `Import_dire (v1, v2, v3) ->
      let timport = (* "import" *) token env v1 in
      let res =
        match v2 with
        | `Source_import x ->
            let (_, str, _), aliasopt = map_source_import env x in
            [ ImportAs (timport, FileName str, aliasopt) |> G.d ]
        | `Import_clause_from_clause (v1, v2) ->
            let f = map_import_clause env v1 in
            let _, str, _ = map_from_clause env v2 in
            f timport (FileName str)
      in
      let _sc = (* ";" *) token env v3 in
      res

let rec map_anon_choice_exp_5650be1 (env : env)
    (x : CST.anon_choice_exp_5650be1) : argument =
  match x with
  | `Exp x ->
      map_expression env x |> G.arg
      (* TODO: what is this argument? a list of keyword args?  *)
  | `LCURL_opt_id_COLON_exp_rep_COMMA_id_COLON_exp_opt_COMMA_RCURL (v1, v2, v3)
    ->
      let lb = (* "{" *) token env v1 in
      let xs =
        match v2 with
        | Some x ->
            map_anon_yul_id_COLON_exp_rep_COMMA_yul_id_COLON_exp_opt_COMMA_c2b7c35
              env x
        | None -> []
      in
      let _rb = (* "}" *) token env v3 in
      OtherArg
        ( ("ArgIds", lb),
          xs
          |> List_.map (fun (id, tcol, e) -> [ I id; Tk tcol; E e ])
          |> List_.flatten )

and map_anon_yul_id_COLON_exp_rep_COMMA_yul_id_COLON_exp_opt_COMMA_c2b7c35
    (env : env)
    ((v1, v2, v3, v4, v5) :
      CST.anon_yul_id_COLON_exp_rep_COMMA_yul_id_COLON_exp_opt_COMMA_c2b7c35) =
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let tcol = (* ":" *) token env v2 in
  let e = map_expression env v3 in
  let rest =
    List_.map
      (fun (v1, v2, v3, v4) ->
        let _v1 = (* "," *) token env v1 in
        let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        let tcol = (* ":" *) token env v3 in
        let e = map_expression env v4 in
        (id, tcol, e))
      v4
  in
  let _v5 = map_trailing_comma env v5 in
  (id, tcol, e) :: rest

and map_array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) : expr =
  let e = map_expression env v1 in
  let lb = (* "[" *) token env v2 in
  let idx_opt = Option.map (map_expression env) v3 in
  let rb = (* "]" *) token env v4 in
  match idx_opt with
  | Some idx -> ArrayAccess (e, (lb, idx, rb)) |> G.e
  | None -> OtherExpr (("ArrayAccessEmpty", lb), [ E e; Tk lb; Tk rb ]) |> G.e

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (And, v2) [ v1; v3 ]
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Or, v2) [ v1; v3 ]
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (LSR, v2) [ v1; v3 ]
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (ASR, v2) [ v1; v3 ]
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (LSL, v2) [ v1; v3 ]
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (BitAnd, v2) [ v1; v3 ]
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (BitXor, v2) [ v1; v3 ]
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (BitOr, v2) [ v1; v3 ]
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Plus, v2) [ v1; v3 ]
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Minus, v2) [ v1; v3 ]
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Mult, v2) [ v1; v3 ]
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Div, v2) [ v1; v3 ]
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Mod, v2) [ v1; v3 ]
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Pow, v2) [ v1; v3 ]
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Lt, v2) [ v1; v3 ]
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (LtE, v2) [ v1; v3 ]
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      (* TODO: or PhysEq? *)
      G.opcall (Eq, v2) [ v1; v3 ]
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (NotEq, v2) [ v1; v3 ]
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (NotPhysEq, v2) [ v1; v3 ]
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (GtE, v2) [ v1; v3 ]
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Gt, v2) [ v1; v3 ]

and map_call_arguments (env : env) ((v1, v2, v3) : CST.call_arguments) :
    arguments =
  let lp = (* "(" *) token env v1 in
  let args =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_anon_choice_exp_5650be1 env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_exp_5650be1 env v2 in
              v2)
            v2
        in
        let _v3 = map_trailing_comma env v3 in
        v1 :: v2
    | None -> []
  in
  let rp = (* ")" *) token env v3 in
  (lp, args, rp)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Choice_bin_exp x -> (
      match x with
      | `Bin_exp x -> map_binary_expression env x
      | `Un_exp x -> map_unary_expression env x
      | `Update_exp x -> map_update_expression env x
      | `Call_exp (v1, v2) ->
          let e = map_expression env v1 in
          let args = map_call_arguments env v2 in
          Call (e, args) |> G.e
      | `Paya_conv_exp (v1, v2) ->
          (* TODO: add to special? *)
          let id = (* "payable" *) str env v1 in
          let args = map_call_arguments env v2 in
          let n = N (H2.name_of_id id) |> G.e in
          Call (n, args) |> G.e
      | `Meta_type_exp (v1, v2, v3, v4) ->
          let ttype = (* "type" *) token env v1 in
          let lp = (* "(" *) token env v2 in
          let t = map_type_name env v3 in
          let rp = (* ")" *) token env v4 in
          let arg = ArgType t in
          let op = IdSpecial (Typeof, ttype) |> G.e in
          Call (op, (lp, [ arg ], rp)) |> G.e
      | `Prim_exp x -> map_primary_expression env x
      | `Struct_exp (v1, v2, v3, v4) ->
          let e = map_expression env v1 in
          let lb = (* "{" *) token env v2 in
          let flds =
            match v3 with
            | Some (v1, v2, v3, v4, v5) ->
                let fld_id =
                  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1
                in
                let _tcolon = (* ":" *) token env v2 in
                let e = map_expression env v3 in
                let xs =
                  List_.map
                    (fun (v1, v2, v3, v4) ->
                      let _v1 = (* "," *) token env v1 in
                      let fld_id =
                        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2
                      in
                      let _tcolon = (* ":" *) token env v3 in
                      let e = map_expression env v4 in
                      (fld_id, e))
                    v4
                in
                let _v5 = map_trailing_comma env v5 in
                (fld_id, e) :: xs
            | None -> []
          in
          let rb = (* "}" *) token env v4 in
          (* TODO? kind of New? or a With? *)
          let flds_any =
            flds
            |> List_.map (fun (fld_id, e) -> [ I fld_id; E e ])
            |> List_.flatten
          in
          OtherExpr (("StructExpr", lb), [ E e; Tk lb ] @ flds_any @ [ Tk rb ])
          |> G.e
      | `Tern_exp (v1, v2, v3, v4, v5) ->
          let e1 = map_expression env v1 in
          let _question = (* "?" *) token env v2 in
          let e2 = map_expression env v3 in
          let _colon = (* ":" *) token env v4 in
          let e3 = map_expression env v5 in
          Conditional (e1, e2, e3) |> G.e
      | `Type_cast_exp (v1, v2, v3, v4) ->
          let t = map_primitive_type env v1 in
          let lp = (* "(" *) token env v2 in
          let e = map_expression env v3 in
          let _rp = (* ")" *) token env v4 in
          Cast (t, lp, e) |> G.e)
  | `Ellips tok -> Ellipsis ((* "..." *) token env tok) |> G.e
  | `Deep_ellips (v1, v2, v3) ->
      let l = (* "<..." *) token env v1 in
      let e = map_expression env v2 in
      let r = (* "...>" *) token env v3 in
      DeepEllipsis (l, e, r) |> G.e
  | `Member_ellips_exp (v1, v2, v3) ->
      let e = map_anon_choice_exp_97f816a env v1 in
      let _tdot = (* "." *) token env v2 in
      let tdots = (* "..." *) token env v3 in
      DotAccessEllipsis (e, tdots) |> G.e

and map_anon_choice_exp_97f816a (env : env) (x : CST.anon_choice_exp_97f816a) =
  match x with
  | `Exp x -> map_expression env x
  | `Id tok ->
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
      N (H2.name_of_id id) |> G.e

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) :
    expr =
  let e = map_anon_choice_exp_97f816a env v1 in
  let tdot = (* "." *) token env v2 in
  let fld = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
  DotAccess (e, tdot, FN (H2.name_of_id fld)) |> G.e

and map_nameless_parameter (env : env) ((v1, v2) : CST.nameless_parameter) =
  let t = map_type_name env v1 in
  let _attrsTODO =
    match v2 with
    | Some x -> [ map_storage_location env x ]
    | None -> []
  in
  t

and map_parameter (env : env) (x : CST.parameter) : parameter =
  match x with
  | `Type_name_opt_stor_loca_opt_id (v1, v2, v3) ->
      let t = map_type_name env v1 in
      let pattrs =
        match v2 with
        | Some x -> [ map_storage_location env x ]
        | None -> []
      in
      let pname =
        match v3 with
        | Some tok -> (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) Some (str env tok)
        | None -> None
      in
      Param (G.param_of_type ~pattrs ?pname t)
  | `Ellips tok -> ParamEllipsis ((* "..." *) token env tok)

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameters =
  let lp = (* "(" *) token env v1 in
  let params =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_parameter env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_parameter env v2 in
              v2)
            v2
        in
        let _v3 = map_trailing_comma env v3 in
        v1 :: v2
    | None -> []
  in
  let rp = (* ")" *) token env v3 in
  (lp, params, rp)

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : expr =
  let _lp = (* "(" *) token env v1 in
  let e = map_expression env v2 in
  let _rp = (* ")" *) token env v3 in
  (* alt: ParenExpr (lp, e, rp) |> G.e *)
  e

and map_primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Member_exp x -> map_member_expression env x
  | `Array_access x -> map_array_access env x
  | `Slice_access (v1, v2, v3, v4, v5, v6) ->
      let e = map_expression env v1 in
      let lb = (* "[" *) token env v2 in
      let lower_opt = Option.map (map_expression env) v3 in
      let _tcolon = (* ":" *) token env v4 in
      let upper_opt = Option.map (map_expression env) v5 in
      let rb = (* "]" *) token env v6 in
      SliceAccess (e, (lb, (lower_opt, upper_opt, None), rb)) |> G.e
  (* TODO: what is that? *)
  | `Prim_type x ->
      let t = map_primitive_type env x in
      OtherExpr (("TypeExpr", fake ""), [ T t ]) |> G.e
  | `Assign_exp (v1, v2, v3) ->
      let lhs = map_expression env v1 in
      let teq = (* "=" *) token env v2 in
      let rhs = map_expression env v3 in
      Assign (lhs, teq, rhs) |> G.e
  | `Augm_assign_exp (v1, v2, v3) ->
      let lhs = map_expression env v1 in
      let op =
        match v2 with
        | `PLUSEQ tok -> (* "+=" *) (Plus, token env tok)
        | `DASHEQ tok -> (* "-=" *) (Minus, token env tok)
        | `STAREQ tok -> (* "*=" *) (Mult, token env tok)
        | `SLASHEQ tok -> (* "/=" *) (Div, token env tok)
        | `PERCEQ tok -> (* "%=" *) (Mod, token env tok)
        | `HATEQ tok -> (* "^=" *) (BitXor, token env tok)
        | `AMPEQ tok -> (* "&=" *) (BitAnd, token env tok)
        | `BAREQ tok -> (* "|=" *) (BitOr, token env tok)
        | `GTGTEQ tok -> (* ">>=" *) (LSR, token env tok)
        | `GTGTGTEQ tok -> (* ">>>=" *) (ASR, token env tok)
        | `LTLTEQ tok -> (* "<<=" *) (LSL, token env tok)
      in
      let rhs = map_expression env v3 in
      AssignOp (lhs, op, rhs) |> G.e
  (* TODO: this is actually not a type, but in grammar.js maybe because
   * of ambiguities it uses this instead of a 'path_identifier' *)
  | `User_defi_type x ->
      let n = map_user_defined_type env x in
      N n |> G.e
  | `Tuple_exp x -> map_tuple_expression env x
  | `Inline_array_exp (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let es =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_expression env v2 in
                  v2)
                v2
            in
            let _v3 = map_trailing_comma env v3 in
            v1 :: v2
        | None -> []
      in
      let rb = (* "]" *) token env v3 in
      OtherExpr
        ( ("InlineArray", lb),
          [ Tk lb ] @ (es |> List_.map (fun e -> E e)) @ [ Tk rb ] )
      |> G.e
  | `Id tok ->
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
      N (H2.name_of_id id) |> G.e
  | `Lit x -> map_literal env x
  | `New_exp (v1, v2, v3) -> (
      let tnew = (* "new" *) token env v1 in
      let t = map_type_name env v2 in
      let argsopt =
        match v3 with
        | Some x -> Some (map_call_arguments env x)
        | None -> None
      in
      match argsopt with
      | None -> New (tnew, t, empty_id_info (), fb []) |> G.e
      | Some (lp, es, rp) ->
          New (tnew, t, empty_id_info (), (lp, es, rp)) |> G.e)

and map_return_parameters (env : env)
    ((v0, v1, v2, v3, v4, v5) : CST.return_parameters) : type_ =
  let _tret = (* returns *) token env v0 in
  let lp = (* "(" *) token env v1 in
  let v2 = map_nameless_parameter env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_nameless_parameter env v2 in
        v2)
      v3
  in
  let _v4 = map_trailing_comma env v4 in
  let rp = (* ")" *) token env v5 in
  TyTuple (lp, v2 :: v3, rp) |> G.t

and map_tuple_expression (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) :
    expr =
  let lp = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_expression env x
    | None -> tuple_hole_expr env lp
  in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let tcomma = (* "," *) token env v1 in
        let e =
          match v2 with
          | Some x -> map_expression env x
          | None -> tuple_hole_expr env tcomma
        in
        e)
      v3
  in
  let rp = (* ")" *) token env v4 in
  Container (Tuple, (lp, v2 :: v3, rp)) |> G.e

and map_type_name (env : env) (x : CST.type_name) : type_ =
  match x with
  | `Prim_type x -> map_primitive_type env x
  | `User_defi_type x ->
      let n = map_user_defined_type env x in
      TyN n |> G.t
  | `Mapp (v1, v2, v3, v4, v5, v6) ->
      let idmap = (* "mapping" *) str env v1 in
      let lp = (* "(" *) token env v2 in
      let tkey = map_mapping_key env v3 in
      let _tarrow = (* "=>" *) token env v4 in
      let tval = map_type_name env v5 in
      let rp = (* ")" *) token env v6 in
      let n = H2.name_of_id idmap in
      let targs = [ tkey; tval ] |> List_.map (fun t -> TA t) in
      TyApply (TyN n |> G.t, (lp, targs, rp)) |> G.t
  | `Array_type (v1, v2, v3, v4) ->
      let t = map_type_name env v1 in
      let lb = (* "[" *) token env v2 in
      let eopt =
        match v3 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let rb = (* "]" *) token env v4 in
      TyArray ((lb, eopt, rb), t) |> G.t
  | `Func_type (v1, v2, v3, v4) ->
      let tfunc = (* "function" *) token env v1 in
      let _, params, _ = map_parameter_list env v2 in
      let _v3TODO =
        List_.map
          (fun x ->
            match x with
            | `Visi x -> map_visibility env x
            | `State_muta x -> map_state_mutability env x)
          v3
      in
      let tret =
        match v4 with
        | Some x -> map_return_parameters env x
        | None -> G.ty_builtin ("void", tfunc)
      in
      TyFun (params, tret) |> G.t

and map_unary_expression (env : env) (x : CST.unary_expression) : expr =
  match x with
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (Not, v1) [ v2 ]
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (BitNot, v1) [ v2 ]
  | `DASH_exp (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (Minus, v1) [ v2 ]
  | `PLUS_exp (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (Plus, v1) [ v2 ]
  | `Delete_exp (v1, v2) ->
      let v1 = (* "delete" *) token env v1 in
      let v2 = map_expression env v2 in
      OtherExpr (("Delete", v1), [ E v2 ]) |> G.e

and map_update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let e = map_expression env v1 in
      let op, t = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      G.special (IncrDecr (op, Postfix), t) [ e ]
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let op, t = map_anon_choice_PLUSPLUS_e498e28 env v1 in
      let e = map_expression env v2 in
      G.special (IncrDecr (op, Prefix), t) [ e ]

let rec map_yul_block (env : env) ((v1, v2, v3) : CST.yul_block) =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_yul_statement env) v2 in
  let rb = (* "}" *) token env v3 in
  Block (lb, xs, rb) |> G.s

and map_yul_statement (env : env) (x : CST.yul_statement) : stmt =
  match x with
  | `Yul_blk x -> map_yul_block env x
  | `Yul_var_decl x ->
      let def = map_yul_variable_declaration env x in
      DefStmt def |> G.s
  | `Yul_assign x ->
      let e = map_yul_assignment env x in
      G.exprstmt e
  | `Yul_func_call x ->
      let e = map_yul_function_call env x in
      G.exprstmt e
  | `Yul_if_stmt (v1, v2, v3) ->
      let tif = (* "if" *) token env v1 in
      let cond = map_yul_expression env v2 in
      let then_ = map_yul_block env v3 in
      If (tif, Cond cond, then_, None) |> G.s
  | `Yul_for_stmt (v1, v2, v3, v4, v5) ->
      let tfor = (* "for" *) token env v1 in
      let init = map_yul_block env v2 in
      let cond = map_yul_expression env v3 in
      let post_iter = map_yul_block env v4 in
      let body = map_yul_block env v5 in
      (* TODO: change AST_generic.for_header? ugly to use stmt_to_expr *)
      let init = [ ForInitExpr (G.stmt_to_expr init) ] in
      let post = G.stmt_to_expr post_iter in
      For (tfor, ForClassic (init, Some cond, Some post), body) |> G.s
  | `Yul_switch_stmt (v1, v2, v3) ->
      let tswitch = (* "switch" *) token env v1 in
      let cond = map_yul_expression env v2 in
      let cases_and_body =
        match v3 with
        | `Defa_yul_blk (v1, v2) ->
            let tdefault = (* "default" *) token env v1 in
            let st = map_yul_block env v2 in
            [ CasesAndBody ([ Default tdefault ], st) ]
        | `Rep1_case_yul_lit_yul_blk_opt_defa_yul_blk (v1, v2) ->
            let v1 =
              List_.map
                (fun (v1, v2, v3) ->
                  let tcase = (* "case" *) token env v1 in
                  let lit = map_yul_literal env v2 in
                  let st = map_yul_block env v3 in
                  CasesAndBody ([ Case (tcase, PatLiteral lit) ], st))
                v1
            in
            let v2 =
              match v2 with
              | Some (v1, v2) ->
                  let tdefault = (* "default" *) token env v1 in
                  let st = map_yul_block env v2 in
                  [ CasesAndBody ([ Default tdefault ], st) ]
              | None -> []
            in
            v1 @ v2
      in
      Switch (tswitch, Some (Cond cond), cases_and_body) |> G.s
  | `Yul_leave tok ->
      let x = (* "leave" *) token env tok in
      OtherStmt (OS_Todo, [ G.TodoK ("Leave", x) ]) |> G.s
  | `Yul_brk tok ->
      let x = (* "break" *) token env tok in
      Break (x, LNone, G.sc) |> G.s
  | `Yul_cont tok ->
      let x = (* "continue" *) token env tok in
      Continue (x, LNone, G.sc) |> G.s
  | `Yul_func_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let tfunc = (* "function" *) token env v1 in
      let id = (* pattern [a-zA-Z$_]+ *) str env v2 in
      let _lp = (* "(" *) token env v3 in
      let ids =
        match v4 with
        | Some x -> map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e env x
        | None -> []
      in
      let _rp = (* ")" *) token env v5 in
      let tret =
        match v6 with
        | Some (v1, v2, v3, v4) -> (
            let _tarrow = (* "->" *) token env v1 in
            let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
            let v3 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
                  v2)
                v3
            in
            let _v4 = map_trailing_comma env v4 in
            match v2 :: v3 with
            | [] -> raise Impossible
            | [ id ] -> Some (TyN (H2.name_of_id id) |> G.t)
            | xs ->
                Some
                  (TyTuple
                     (fb
                        (xs
                        |> List_.map (fun id -> TyN (H2.name_of_id id) |> G.t)))
                  |> G.t))
        | None -> None
      in
      let body = map_yul_block env v7 in
      let ent = G.basic_entity id in
      let params = ids |> List_.map (fun id -> Param (G.param_of_id id)) in
      let def =
        {
          fkind = (Function, tfunc);
          fparams = fb params;
          frettype = tret;
          fbody = FBStmt body;
        }
      in
      DefStmt (ent, FuncDef def) |> G.s
  | `Yul_label (v1, v2) ->
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
      let tcol = (* ":" *) token env v2 in
      (* TODO: change in Label? but following stmt? There is a goto? *)
      OtherStmt (OS_Todo, [ TodoK ("Label", tcol); I id ]) |> G.s
  | `Yul_lit x ->
      let x = map_yul_literal env x in
      G.exprstmt (L x |> G.e)

let map_state_variable_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.state_variable_declaration) : definition =
  let ty = map_type_name env v1 in
  let attrs =
    List_.map
      (fun x ->
        match x with
        | `Visi x -> map_visibility env x
        | `Cst tok ->
            let x = (* "constant" *) token env tok in
            G.attr Const x
        | `Over_spec x ->
            let x = map_override_specifier env x in
            x
        (* TODO: difference between constant and immutable? *)
        | `Immu tok -> (* "immutable" *) str env tok |> G.unhandled_keywordattr)
      v2
  in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
  let vinit =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        Some v2
    | None -> None
  in
  let sc = (* ";" *) token env v5 in
  let ent = G.basic_entity ~attrs id in
  let def = { vinit; vtype = Some ty; vtok = Some sc } in
  (ent, VarDef def)

let map_modifier_invocation (env : env) ((v1, v2) : CST.modifier_invocation) :
    attribute =
  let name = map_identifier_path env v1 in
  let args =
    match v2 with
    | Some x -> map_call_arguments env x
    | None -> fb []
  in
  NamedAttr (fake "@", name, args)

let map_expression_statement (env : env) (x : CST.expression_statement) =
  match x with
  | `Exp_semi (v1, v2) ->
      let e = map_expression env v1 in
      let sc = (* ";" *) token env v2 in
      (e, sc)
  | `Ellips_SEMI (v1, v2) ->
      let tdots = (* "..." *) token env v1 in
      let sc = (* ";" *) token env v2 in
      (Ellipsis tdots |> G.e, sc)
  | `Ellips tok ->
      let tdots = (* "..." *) token env tok in
      (Ellipsis tdots |> G.e, G.sc)

let map_struct_member (env : env) (x : CST.struct_member) : field =
  match x with
  | `Type_name_id_semi (v1, v2, v3) ->
      let ty = map_type_name env v1 in
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
      let _sc = (* ";" *) token env v3 in
      G.basic_field id None (Some ty)
  | `Ellips tok -> G.field_ellipsis ((* "..." *) token env tok)

let map_inheritance_specifier (env : env) (v1 : CST.inheritance_specifier) :
    class_parent =
  match v1 with
  | `Ellips tok -> (G.TyEllipsis (token env tok) |> G.t, None)
  | `User_defi_type_opt_call_args (v1, v2) ->
      let n = map_user_defined_type env v1 in
      let ty = TyN n |> G.t in
      let argsopt =
        match v2 with
        | Some x -> Some (map_call_arguments env x)
        | None -> None
      in
      (ty, argsopt)

let map_event_paramater (env : env) (x : CST.event_paramater) : parameter =
  match x with
  | `Type_name_opt_inde_opt_id (v1, v2, v3) ->
      let ty = map_type_name env v1 in
      let pattrs =
        match v2 with
        | Some tok -> [ G.unhandled_keywordattr (* "indexed" *) (str env tok) ]
        | None -> []
      in
      let idopt =
        match v3 with
        | Some tok -> (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) Some (str env tok)
        | None -> None
      in
      G.Param (G.param_of_type ~pattrs ?pname:idopt ty)
  | `Ellips v ->
      let tk = token env v in
      G.ParamEllipsis tk

let map_return_type_definition (env : env)
    ((v1, v2) : CST.return_type_definition) =
  let tret = (* "returns" *) token env v1 in
  let params = map_parameter_list env v2 in
  (tret, params)

let map_variable_declaration (env : env)
    ((v1, v2, v3) : CST.variable_declaration) =
  let ty = map_type_name env v1 in
  let attrs =
    match v2 with
    | Some x -> [ map_storage_location env x ]
    | None -> []
  in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
  (ty, attrs, id)

let map_using_directive (env : env) ((v1, v2, v3, v4, v5) : CST.using_directive)
    : directive =
  (* TODO: a bit similar to an Import *)
  let tusing = (* "using" *) token env v1 in
  let n1 = map_user_defined_type env v2 in
  let ty1 = TyN n1 |> G.t in
  (* ?? some kind of mixins? *)
  let tfor = (* "for" *) token env v3 in
  let ty2 =
    match v4 with
    | `Any_source_type tok ->
        let star = (* "*" *) str env tok in
        TyN (H2.name_of_id star) |> G.t
    | `Type_name x -> map_type_name env x
  in
  let sc = (* ";" *) token env v5 in
  OtherDirective (("Using", tusing), [ T ty1; Tk tfor; T ty2; Tk sc ]) |> G.d

let map_struct_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.struct_declaration) : definition =
  let tstruct = (* "struct" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let lb = (* "{" *) token env v3 in
  let flds = List_.map (map_struct_member env) v4 in
  let rb = (* "}" *) token env v5 in
  let ent = G.basic_entity id in
  let def =
    {
      ckind = (Class, tstruct);
      cextends = [];
      cimplements = [];
      cmixins = [];
      cparams = fb [];
      cbody = (lb, flds, rb);
    }
  in
  (ent, ClassDef def)

let map_class_heritage (env : env) ((v1, v2, v3, v4) : CST.class_heritage) :
    class_parent list =
  let _tis = (* "is" *) token env v1 in
  let v2 = map_inheritance_specifier env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_inheritance_specifier env v2 in
        v2)
      v3
  in
  let _v4 = map_trailing_comma env v4 in
  v2 :: v3

let map_event_parameter_list (env : env)
    ((v1, v2, v3) : CST.event_parameter_list) : parameters =
  let lb = (* "(" *) token env v1 in
  let xs =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_event_paramater env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_event_paramater env v2 in
              v2)
            v2
        in
        let _v3 = map_trailing_comma env v3 in
        v1 :: v2
    | None -> []
  in
  let rb = (* ")" *) token env v3 in
  (lb, xs, rb)

let map_variable_declaration_tuple (env : env)
    (x : CST.variable_declaration_tuple) : pattern =
  match x with
  | `LPAR_opt_opt_var_decl_rep_COMMA_opt_var_decl_opt_COMMA_RPAR (v1, v2, v3) ->
      let lp = (* "(" *) token env v1 in
      let xs =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = Option.map (map_variable_declaration env) v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = Option.map (map_variable_declaration env) v2 in
                  v2)
                v2
            in
            let _v3 = map_trailing_comma env v3 in
            v1 :: v2
            (* TODO: should generate hole pattern when using (x,,y) *)
            |> List_.filter_map (fun x -> x)
            |> List_.map (fun (ty, _attrsTODO, id) ->
                   PatTyped (PatId (id, G.empty_id_info ()) |> G.p, ty) |> G.p)
        | None -> []
      in
      let rp = (* ")" *) token env v3 in
      PatTuple (lp, xs, rp) |> G.p
  | `Var_LPAR_opt_id_rep_COMMA_opt_id_RPAR (v1, v2, v3, v4, v5) ->
      let _tvar = (* "var" *) token env v1 in
      let lp = (* "(" *) token env v2 in
      let p =
        match v3 with
        | Some tok ->
            let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
            PatId (id, G.empty_id_info ()) |> G.p
        | None -> tuple_hole_pat env lp
      in
      let ps =
        List_.map
          (fun (v1, v2) ->
            let tcomma = (* "," *) token env v1 in
            let pat =
              match v2 with
              | Some tok ->
                  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
                  PatId (id, G.empty_id_info ()) |> G.p
              | None -> tuple_hole_pat env tcomma
            in
            pat)
          v4
      in
      let rp = (* ")" *) token env v5 in
      PatTuple (lp, p :: ps, rp) |> G.p

let map_event_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.event_definition) : definition =
  let tevent = (* "event" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let fparams = map_event_parameter_list env v3 in
  let attrs =
    match v4 with
    | Some tok -> (* "anonymous" *) [ str env tok |> G.unhandled_keywordattr ]
    | None -> []
  in
  let sc = (* ";" *) token env v5 in
  let ent = G.basic_entity id ~attrs in
  (* TODO? make a new fkind? *)
  let fdef =
    { fkind = (Function, tevent); fparams; frettype = None; fbody = FBDecl sc }
  in
  (ent, FuncDef fdef)

let map_variable_declaration_statement (env : env)
    ((v1, v2) : CST.variable_declaration_statement) :
    entity * variable_definition =
  let def =
    match v1 with
    | `Var_decl_opt_EQ_exp (v1, v2) ->
        let ty, attrs, id = map_variable_declaration env v1 in
        let vinit =
          match v2 with
          | Some (v1, v2) ->
              let _teq = (* "=" *) token env v1 in
              let e = map_expression env v2 in
              Some e
          | None -> None
        in
        let ent = G.basic_entity id ~attrs in
        let vdef = { vinit; vtype = Some ty; vtok = G.no_sc } in
        (ent, vdef)
    | `Var_decl_tuple_EQ_exp (v1, v2, v3) ->
        let pat = map_variable_declaration_tuple env v1 in
        let _teq = (* "=" *) token env v2 in
        let e = map_expression env v3 in
        let ent = { name = EPattern pat; attrs = []; tparams = None } in
        let vdef = { vinit = Some e; vtype = None; vtok = G.no_sc } in
        (ent, vdef)
  in
  let _sc = (* ";" *) token env v2 in
  def

let rec map_block_statement (env : env) ((v0, v1, v2, v3) : CST.block_statement)
    =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_statement env) v2 in
  let rb = (* "}" *) token env v3 in
  let stmt = Block (lb, xs, rb) |> G.s in
  match v0 with
  | Some tok ->
      let t = (* "unchecked" *) token env tok in
      OtherStmtWithStmt (OSWS_Block ("Unchecked", t), [], stmt) |> G.s
  | None -> stmt

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) : catch =
  let tcatch = (* "catch" *) token env v1 in
  let catch_exn : catch_exn =
    match v2 with
    | Some (v1, v2) ->
        let idopt =
          match v1 with
          | Some tok ->
              (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) [ I (str env tok) ]
          | None -> []
        in
        let _l, params, _r = map_parameter_list env v2 in
        let anys = idopt @ (params |> List_.map (fun p -> Pa p)) in
        OtherCatch (("CatchParams", tcatch), anys)
    | None -> OtherCatch (("CatchEmpty", tcatch), [])
  in
  let st = map_block_statement env v3 in
  (tcatch, catch_exn, st)

and map_statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `Blk_stmt x -> map_block_statement env x
  | `Exp_stmt x ->
      let e, sc = map_expression_statement env x in
      ExprStmt (e, sc) |> G.s
  | `Var_decl_stmt x ->
      let ent, vdef = map_variable_declaration_statement env x in
      DefStmt (ent, VarDef vdef) |> G.s
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let tif = (* "if" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _rp = (* ")" *) token env v4 in
      let then_ = map_statement env v5 in
      let else_opt =
        match v6 with
        | Some (v1, v2) ->
            let _telse = (* "else" *) token env v1 in
            let st = map_statement env v2 in
            Some st
        | None -> None
      in
      If (tif, Cond cond, then_, else_opt) |> G.s
  | `For_stmt v1 -> map_for_statement env v1
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let twhile = (* "while" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _rp = (* ")" *) token env v4 in
      let st = map_statement env v5 in
      While (twhile, Cond cond, st) |> G.s
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let tdo = (* "do" *) token env v1 in
      let st = map_statement env v2 in
      let _twhile = (* "while" *) token env v3 in
      let _lp = (* "(" *) token env v4 in
      let cond = map_expression env v5 in
      let _rp = (* ")" *) token env v6 in
      let _sc = (* ";" *) token env v7 in
      DoWhile (tdo, st, cond) |> G.s
  | `Cont_stmt (v1, v2) ->
      let tcont = (* "continue" *) token env v1 in
      let sc = (* ";" *) token env v2 in
      Continue (tcont, LNone, sc) |> G.s
  | `Brk_stmt (v1, v2) ->
      let tbreak = (* "break" *) token env v1 in
      let sc = (* ";" *) token env v2 in
      Continue (tbreak, LNone, sc) |> G.s
  | `Try_stmt (v1, v2, v3, v4, v5) ->
      let ttry = (* "try" *) token env v1 in
      (* Solidity try statement is a bit unusual, see for example:
       *   try foo() (Param p1, Param p2) { return p1; } catch { return 1; }
       * We internally convert part of it in a Lambda below, for the example:
       *   try ((Param p1, Param p2) -> { return p; })(foo()) catch {...}.
       * There is no Lambda construct in Solidity so we don't risk
       * a collision with other constructs.
       *)
      (* must be an external funcall or contract creation according to doc *)
      let e = map_expression env v2 in
      let params =
        match v3 with
        | Some x ->
            let _treturn, params = map_return_type_definition env x in
            params
        | None -> fb []
      in
      let st = map_block_statement env v4 in
      let fun_ =
        {
          fkind = (LambdaKind, ttry);
          fparams = params;
          frettype = None;
          fbody = FBStmt st;
        }
      in
      let lambda = Lambda fun_ |> G.e in
      let call = Call (lambda, fb [ Arg e ]) |> G.e in
      let try_stmt = G.exprstmt call in
      let catches = List_.map (map_catch_clause env) v5 in
      Try (ttry, try_stmt, catches, None, None) |> G.s
  | `Ret_stmt (v1, v2, v3) ->
      let tret = (* "return" *) token env v1 in
      let eopt =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let sc = (* ";" *) token env v3 in
      Return (tret, eopt, sc) |> G.s
  | `Emit_stmt (v1, v2, v3, v4) ->
      let temit = (* "emit" *) token env v1 in
      let e = map_expression env v2 in
      let _lp, args, _rp = map_call_arguments env v3 in
      let sc = (* ";" *) token env v4 in
      OtherStmt (OS_Todo, [ TodoK ("Emit", temit); E e; Args args; Tk sc ])
      |> G.s
  | `Asse_stmt (v1, v2, v3, v4, v5) ->
      let tassembly = (* "assembly" *) token env v1 in
      let _evmasm =
        match v2 with
        | Some tok -> (* "\"evmasm\"" *) Some (token env tok)
        | None -> None
      in
      let lb = (* "{" *) token env v3 in
      let xs = List_.map (map_yul_statement env) v4 in
      let rb = (* "}" *) token env v5 in
      let st = Block (lb, xs, rb) |> G.s in
      OtherStmtWithStmt (OSWS_Block ("Assembly", tassembly), [], st) |> G.s
  | `Revert_stmt (v1, v2, v3) ->
      let revert = (* "revert" *) str env v1 in
      (* less: could be a OtherExpr or OtherStmt *)
      let name = H2.name_of_id revert in
      let e = N name |> G.e in
      let e =
        match v2 with
        | Some (v1, v2) ->
            let op =
              match v1 with
              | Some x ->
                  let e1 = map_expression env x in
                  Call (e, fb [ Arg e1 ]) |> G.e
              | None -> e
            in
            let v2 = map_call_arguments env v2 in
            Call (op, v2) |> G.e
        | None -> e
      in
      let sc = (* ";" *) token env v3 in
      ExprStmt (e, sc) |> G.s

and map_for_statement env v =
  match v with
  | `For_LPAR_choice_var_decl_stmt_choice_exp_stmt_opt_exp_RPAR_stmt
      (v1, v2, v3, v4, v5, v6, v7) ->
      let tfor = (* "for" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let init =
        match v3 with
        | `Var_decl_stmt x ->
            let ent, vdef = map_variable_declaration_statement env x in
            [ ForInitVar (ent, vdef) ]
        | `Exp_stmt x ->
            let e, _sc = map_expression_statement env x in
            [ ForInitExpr e ]
        | `Semi tok ->
            (* ";" *)
            let _sc = token env tok in
            []
      in
      let cond =
        match v4 with
        | `Exp_stmt x ->
            let e, _sc = map_expression_statement env x in
            Some e
        | `Semi tok ->
            let _sc = (* ";" *) token env tok in
            None
      in
      let post =
        match v5 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let _rp = (* ")" *) token env v6 in
      let st = map_statement env v7 in
      For (tfor, ForClassic (init, cond, post), st) |> G.s
  | `For_LPAR_ellips_RPAR_stmt (v1, v2, v3, v4, v5) ->
      let tfor = (* "for" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let tellipsis = (* "..." *) token env v3 in
      let _rp = (* ")" *) token env v4 in
      let st = map_statement env v5 in
      For (tfor, ForEllipsis tellipsis, st) |> G.s

let map_function_body (env : env) ((v1, v2, v3) : CST.function_body) :
    function_body =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_statement env) v2 in
  let rb = (* "}" *) token env v3 in
  FBStmt (Block (lb, xs, rb) |> G.s)

let map_constructor_definition (env : env)
    ((v1, v2, v3, v4) : CST.constructor_definition) =
  let tctor = (* "constructor" *) token env v1 in
  let params = map_parameter_list env v2 in
  let attrs =
    List_.map
      (fun x ->
        match x with
        | `Modi_invo x -> map_modifier_invocation env x
        | `Paya tok -> (* "payable" *) str env tok |> G.unhandled_keywordattr
        | `Choice_inte x -> (
            match x with
            | `Inte tok ->
                (* "internal" *) str env tok |> G.unhandled_keywordattr
            | `Public tok -> G.attr Public (* "public" *) (token env tok)))
      v3
  in
  let fbody = map_function_body env v4 in
  let ctor_attr = G.attr Ctor tctor in
  let attrs = ctor_attr :: attrs in
  let ent = G.basic_entity ("constructor", tctor) ~attrs in
  let def =
    { fkind = (Method, tctor); fparams = params; frettype = None; fbody }
  in
  (ent, FuncDef def)

let map_anon_choice_semi_f2fe6be (env : env) (x : CST.anon_choice_semi_f2fe6be)
    : function_body =
  match x with
  | `Semi tok ->
      let sc = (* ";" *) token env tok in
      FBDecl sc
  | `Func_body x ->
      let x = map_function_body env x in
      x

let visi_and_co env x : attribute =
  match x with
  | `Visi x -> map_visibility env x
  | `Modi_invo x -> map_modifier_invocation env x
  | `State_muta x -> map_state_mutability env x
  | `Virt tok ->
      let x = (* "virtual" *) token env tok in
      G.attr Abstract x
  | `Over_spec x -> map_override_specifier env x

let map_fallback_receive_definition (env : env)
    ((v1, v2, v3, v4) : CST.fallback_receive_definition) =
  let id =
    match v1 with
    (* If it has the function kwd before, then it's a regular func.
     * Without the function kwd it's a "special" function.
     * Joran defined both in the grammar for backward compatibility.
     *)
    | `Choice_fall v1 -> (
        match v1 with
        (* TODO: or use OtherDef to treat those specially? or via
         * a CTor like attribute? *)
        | `Fall tok -> (* "fallback" *) str env tok
        | `Rece tok -> (* "receive" *) str env tok
        | `Func tok -> (* "function" *) str env tok)
    (* This is the old syntax for fallback function, just
     * function() public { ... } according to Joran Honig *)
    | `Func tok -> (* "function" *) str env tok
  in
  let fparams = map_parameter_list env v2 in
  let attrs = List_.map (fun x -> visi_and_co env x) v3 in
  let ent = G.basic_entity ~attrs id in
  let fbody = map_anon_choice_semi_f2fe6be env v4 in
  let def = { fkind = (Function, snd id); fparams; frettype = None; fbody } in
  (ent, FuncDef def)

let map_function_definition (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.function_definition) : definition =
  let tfunc = (* "function" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let params = map_parameter_list env v3 in
  let attrs = List_.map (fun x -> visi_and_co env x) v4 in
  let _ret_type_TODO =
    match v5 with
    | Some x -> Some (map_return_type_definition env x)
    | None -> None
  in
  let fbody = map_anon_choice_semi_f2fe6be env v6 in
  let ent = G.basic_entity id ~attrs in
  let def =
    { fkind = (Function, tfunc); fparams = params; frettype = None; fbody }
  in
  (ent, FuncDef def)

let map_modifier_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.modifier_definition) : definition =
  let tmodif = (* "modifier" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let params =
    match v3 with
    | Some x -> map_parameter_list env x
    | None -> fb []
  in
  let attrs =
    List_.map
      (fun x ->
        match x with
        | `Virt tok ->
            let t = (* "virtual" *) token env tok in
            G.attr Abstract t
        | `Over_spec x -> map_override_specifier env x)
      v4
  in
  let attrs = G.unhandled_keywordattr ("modifier", tmodif) :: attrs in
  let fbody = map_anon_choice_semi_f2fe6be env v5 in
  let ent = G.basic_entity id ~attrs in
  let def =
    {
      (* TODO? add OtherFunc? *)
      fkind = (Function, tmodif);
      fparams = params;
      frettype = None;
      fbody;
    }
  in
  (ent, FuncDef def)

(* less: could return a G.parameter *)
let map_error_parameter (env : env) ((v1, v2) : CST.error_parameter) : type_ =
  let ty = map_type_name env v1 in
  let _idTODO =
    match v2 with
    | Some tok -> Some ((* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok)
    | None -> None
  in
  ty

let map_error_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.error_declaration) : definition =
  let _terror = (* "error" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let _lp = (* "(" *) token env v3 in
  let params =
    match v4 with
    | Some (v1, v2, v3) ->
        let v1 = map_error_parameter env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_error_parameter env v2 in
              v2)
            v2
        in
        let _v3 = map_trailing_comma env v3 in
        v1 :: v2
    | None -> []
  in
  let _rp = (* ")" *) token env v5 in
  let _sc = (* ";" *) token env v6 in
  let ent = G.basic_entity id in
  let def = { tbody = Exception (id, params) } in
  (ent, TypeDef def)

let map_contract_member (env : env) (x : CST.contract_member) =
  match x with
  | `Choice_func_defi x -> (
      match x with
      | `Func_defi x -> Left3 (map_function_definition env x)
      | `Modi_defi x -> Left3 (map_modifier_definition env x)
      | `State_var_decl x -> Left3 (map_state_variable_declaration env x)
      | `Struct_decl x -> Left3 (map_struct_declaration env x)
      | `Enum_decl x -> Left3 (map_enum_declaration env x)
      | `Event_defi x -> Left3 (map_event_definition env x)
      | `Using_dire x -> Right3 (map_using_directive env x)
      | `Cons_defi x -> Left3 (map_constructor_definition env x)
      | `Fall_rece_defi x -> Left3 (map_fallback_receive_definition env x)
      | `Error_decl x -> Left3 (map_error_declaration env x)
      | `User_defi_type_defi x -> Left3 (map_user_defined_type_definition env x)
      )
  | `Ellips tok ->
      let t = (* "..." *) token env tok in
      Middle3 t

let map_contract_body (env : env) ((v1, v2, v3) : CST.contract_body) :
    (definition, tok, directive) either3 list bracket =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_contract_member env) v2 in
  let rb = (* "}" *) token env v3 in
  (lb, xs, rb)

let map_declaration (env : env) (x : CST.declaration) : definition =
  match x with
  | `Cont_decl (v1, v2, v3, v4, v5) ->
      let attrs =
        match v1 with
        | Some tok -> [ G.attr Abstract (* "abstract" *) (token env tok) ]
        | None -> []
      in
      let tcontract = (* "contract" *) token env v2 in
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
      let parents =
        match v4 with
        | Some x -> map_class_heritage env x
        | None -> []
      in
      let l, defs_or_dirs, r = map_contract_body env v5 in
      let flds =
        defs_or_dirs |> List_.map (fun x -> F (stmt_of_def_or_dir x))
      in
      let ent = G.basic_entity id ~attrs in
      let def =
        {
          (* ugly: but Class is used for solidity struct, so we need to
           * choose something else *)
          ckind = (Object, tcontract);
          cextends = parents;
          cmixins = [];
          cimplements = [];
          cparams = fb [];
          cbody = (l, flds, r);
        }
      in
      (ent, ClassDef def)
  | `Inte_decl (v1, v2, v3, v4) ->
      let tinter = (* "interface" *) token env v1 in
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
      let parents =
        match v3 with
        | Some x -> map_class_heritage env x
        | None -> []
      in
      let l, defs_or_dirs, r = map_contract_body env v4 in
      let flds =
        defs_or_dirs |> List_.map (fun x -> F (stmt_of_def_or_dir x))
      in
      let ent = G.basic_entity id in
      let def =
        {
          ckind = (Interface, tinter);
          cextends = parents;
          cmixins = [];
          cimplements = [];
          cparams = fb [];
          cbody = (l, flds, r);
        }
      in
      (ent, ClassDef def)
  | `Libr_decl (v1, v2, v3) ->
      let _tlib = (* "library" *) token env v1 in
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
      let _l, defs_or_dirs, _r = map_contract_body env v3 in
      let ent = G.basic_entity id in
      let items = defs_or_dirs |> List_.map stmt_of_def_or_dir in
      (* TODO: kinda of namespace/module? *)
      let def = { mbody = ModuleStruct (Some [ id ], items) } in
      (ent, ModuleDef def)
  | `Struct_decl x -> map_struct_declaration env x
  | `Enum_decl x -> map_enum_declaration env x
  | `Func_defi x -> map_function_definition env x
  | `Cst_var_decl (v1, v2, v3, v4, v5, v6) ->
      let ty = map_type_name env v1 in
      let tconst = (* "constant" *) token env v2 in
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
      let _teq = (* "=" *) token env v4 in
      let e = map_expression env v5 in
      let sc = (* ";" *) token env v6 in
      let attr = G.attr Const tconst in
      let ent = G.basic_entity id ~attrs:[ attr ] in
      let def = { vtype = Some ty; vinit = Some e; vtok = Some sc } in
      (ent, VarDef def)
  | `User_defi_type_defi x -> map_user_defined_type_definition env x
  | `Error_decl x -> map_error_declaration env x

let map_source_unit (env : env) (x : CST.source_unit) : item list =
  match x with
  | `Dire x ->
      let xs = map_directive env x in
      xs |> List_.map (fun dir -> DirectiveStmt dir |> G.s)
  | `Decl x ->
      let def = map_declaration env x in
      [ DefStmt def |> G.s ]

let map_source_file (env : env) (x : CST.source_file) : any =
  match x with
  | `Rep_source_unit v1 ->
      let xxs = List_.map (map_source_unit env) v1 in
      Pr (List_.flatten xxs)
  | `Rep1_stmt xs ->
      let xs = List_.map (map_statement env) xs in
      Ss xs
  | `Exp x ->
      let e = map_expression env x in
      E e
  | `Modi_defi x ->
      let x = map_modifier_definition env x in
      S (DefStmt x |> G.s)
  | `Cons_defi x ->
      let x = map_constructor_definition env x in
      S (DefStmt x |> G.s)
  | `Event_defi x ->
      let x = map_event_definition env x in
      S (DefStmt x |> G.s)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_solidity.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_source_file env cst with
      | G.Pr xs
      | G.Ss xs ->
          xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_solidity.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      map_source_file env cst)

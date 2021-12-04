(* Yoann Padioleau
 *
 * Copyright (c) 2021 R2C
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
module CST = Tree_sitter_solidity.CST
module PI = Parse_info
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
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token

let str = H.str

let _fb = G.fake_bracket

let todo (_env : env) _ = failwith "not implemented"

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-solidity/Boilerplate.ml *)

let map_trailing_comma env v =
  match v with
  | Some tok -> (* "," *) token env tok |> ignore
  | None -> ()

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

let map_state_mutability (env : env) (x : CST.state_mutability) =
  match x with
  | `Pure tok -> (* "pure" *) token env tok
  | `View tok -> (* "view" *) token env tok
  | `Paya tok -> (* "payable" *) token env tok

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

let map_storage_location (env : env) (x : CST.storage_location) =
  match x with
  | `Memory tok -> (* "memory" *) token env tok
  | `Stor tok -> (* "storage" *) token env tok
  | `Call tok -> (* "calldata" *) token env tok

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
  | `PLUSPLUS tok -> (* "++" *) token env tok
  | `DASHDASH tok -> (* "--" *) token env tok

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

let map_yul_path (env : env) ((v1, v2) : CST.yul_path) : dotted_ident =
  let v1 = (* pattern [a-zA-Z$_]+ *) str env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "." *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e (env : env)
    ((v1, v2, v3) : CST.anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e) :
    ident list =
  let v1 = (* pattern [a-zA-Z$_]+ *) str env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
        v2)
      v2
  in
  let _v3 = map_trailing_comma env v3 in
  v1 :: v2

let map_anon_id_rep_COMMA_id_opt_COMMA_e9ba3f8 (env : env)
    ((v1, v2, v3) : CST.anon_id_rep_COMMA_id_opt_COMMA_e9ba3f8) : ident list =
  let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        v2)
      v2
  in
  let _v3 = map_trailing_comma env v3 in
  v1 :: v2

let map_user_defined_type (env : env) ((v1, v2) : CST.user_defined_type) : type_
    =
  let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "." *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        v2)
      v2
  in
  let ids = v1 :: v2 in
  let name = H2.name_of_ids ids in
  TyN name |> G.t

let map_import_declaration (env : env) ((v1, v2) : CST.import_declaration) =
  let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = (* "as" *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
        Some (v2, G.empty_id_info ())
    | None -> None
  in
  (v1, v2)

let map_decimal_number (env : env) (x : CST.decimal_number) =
  match x with
  | `Pat_6c32705 tok ->
      let s, t = (* pattern \d+(\.\d+)?([eE](-)?\d+)? *) str env tok in
      (float_of_string_opt s, t)
  | `Pat_0468c4a tok ->
      let s, t = (* pattern \.\d+([eE](-)?\d+)? *) str env tok in
      (float_of_string_opt s, t)

let map_pragma_version_constraint (env : env)
    ((v1, v2) : CST.pragma_version_constraint) : any list =
  let v1 =
    match v1 with
    | Some x ->
        let op, t = map_solidity_version_comparison_operator env x in
        [ E (IdSpecial (Op op, t) |> G.e) ]
    | None -> []
  in
  let v2 = (* pattern \d+(.\d+(.\d+)?)? *) str env v2 in
  v1 @ [ Str v2 ]

let map_fixed (env : env) (x : CST.fixed) =
  match x with
  | `Fixed tok -> (* "fixed" *) str env tok
  | `Pat_f2662db tok -> (* pattern fixed([0-9]+)x([0-9]+) *) str env tok

let map_anon_rep_opt___hex_digit_c87bea1 (env : env)
    (xs : CST.anon_rep_opt___hex_digit_c87bea1) : string wrap list =
  List.map
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
  match x with
  | `Pat_0c477de tok -> (* pattern "[^\"\\r\\n\\\\]" *) str env tok
  | `Esc_seq tok -> (* escape_sequence *) str env tok

let map_single_quoted_unicode_char (env : env)
    (x : CST.single_quoted_unicode_char) =
  match x with
  | `Pat_a096c41 tok -> (* pattern "[^'\\r\\n\\\\]" *) str env tok
  | `Esc_seq tok -> (* escape_sequence *) str env tok

let map_string_ (env : env) (x : CST.string_) =
  match x with
  | `DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_double_quote tok ->
                (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let v3 = (* "\"" *) token env v3 in
      let str = v2 |> List.map fst |> String.concat "" in
      let toks = (v2 |> List.map snd) @ [ v3 ] in
      (str, PI.combine_infos v1 toks)
  | `SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT (v1, v2, v3) ->
      let v1 = (* "'" *) token env v1 in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_quote tok ->
                (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let v3 = (* "'" *) token env v3 in
      let str = v2 |> List.map fst |> String.concat "" in
      let toks = (v2 |> List.map snd) @ [ v3 ] in
      (str, PI.combine_infos v1 toks)

let map_enum_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.enum_declaration) : definition =
  let _enumkwd = (* "enum" *) token env v1 in
  let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let _lb = (* "{" *) token env v3 in
  let elems =
    match v4 with
    | Some x -> map_anon_id_rep_COMMA_id_opt_COMMA_e9ba3f8 env x
    | None -> []
  in
  let _rb = (* "}" *) token env v5 in
  let ent = G.basic_entity id in
  let ors = elems |> List.map (fun id -> OrEnum (id, None)) in
  let def = { tbody = OrType ors } in
  (ent, TypeDef def)

let map_override_specifier (env : env) ((v1, v2) : CST.override_specifier) =
  let v1 = (* "override" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4, v5) ->
        let v1 = (* "(" *) token env v1 in
        let v2 = map_user_defined_type env v2 in
        let v3 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_user_defined_type env v2 in
              v2)
            v3
        in
        let _v4 = map_trailing_comma env v4 in
        let v5 = (* ")" *) token env v5 in
        Some (v1, v2 :: v3, v5)
    | None -> None
  in
  match v2 with
  | None -> G.attr Override v1
  | Some (_l, xs, _r) ->
      OtherAttribute (("OverrideWithTypes", v1), xs |> List.map (fun t -> T t))

let map_hex_number (env : env) ((v1, v2) : CST.hex_number) =
  let start, t1 = (* pattern 0[xX] *) str env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) str env v1 in
        let v2 = map_anon_rep_opt___hex_digit_c87bea1 env v2 in
        v1 :: v2
    | None -> []
  in
  let str = start ^ (v2 |> List.map fst |> String.concat "") in
  let toks = v2 |> List.map snd in
  (int_of_string_opt str, PI.combine_infos t1 toks)

let map_hex_string_literal (env : env) (xs : CST.hex_string_literal) =
  List.map
    (fun (v1, v2) ->
      let v1 = (* "hex" *) token env v1 in
      let v2 =
        match v2 with
        | `DQUOT_opt_hex_digit_rep_opt___hex_digit_DQUOT (v1, v2, v3) ->
            let v1 = (* "\"" *) token env v1 in
            let v2 =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) str env v1 in
                  let v2 = map_anon_rep_opt___hex_digit_c87bea1 env v2 in
                  v1 :: v2
              | None -> []
            in
            let v3 = (* "\"" *) token env v3 in
            let str = v2 |> List.map fst |> String.concat "" in
            let toks = (v2 |> List.map snd) @ [ v3 ] in
            (str, PI.combine_infos v1 toks)
        | `SQUOT_opt_hex_digit_rep_opt___hex_digit_SQUOT (v1, v2, v3) ->
            let v1 = (* "'" *) token env v1 in
            let v2 =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) str env v1 in
                  let v2 = map_anon_rep_opt___hex_digit_c87bea1 env v2 in
                  v1 :: v2
              | None -> []
            in
            let v3 = (* "'" *) token env v3 in
            let str = v2 |> List.map fst |> String.concat "" in
            let toks = (v2 |> List.map snd) @ [ v3 ] in
            (str, PI.combine_infos v1 toks)
      in
      (v1, v2))
    xs

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

let map_unicode_string_literal (env : env) (xs : CST.unicode_string_literal) =
  List.map
    (fun (v1, v2) ->
      let v1 = (* "unicode" *) token env v1 in
      let v2 =
        match v2 with
        | `DQUOT_rep_double_quoted_unic_char_DQUOT (v1, v2, v3) ->
            let v1 = (* "\"" *) token env v1 in
            let v2 = List.map (map_double_quoted_unicode_char env) v2 in
            let v3 = (* "\"" *) token env v3 in
            let str = v2 |> List.map fst |> String.concat "" in
            let toks = (v2 |> List.map snd) @ [ v3 ] in
            (str, PI.combine_infos v1 toks)
        | `SQUOT_rep_single_quoted_unic_char_SQUOT (v1, v2, v3) ->
            let v1 = (* "'" *) token env v1 in
            let v2 = List.map (map_single_quoted_unicode_char env) v2 in
            let v3 = (* "'" *) token env v3 in
            let str = v2 |> List.map fst |> String.concat "" in
            let toks = (v2 |> List.map snd) @ [ v3 ] in
            (str, PI.combine_infos v1 toks)
      in
      (v1, v2))
    xs

let map_yul_string_literal (env : env) (x : CST.yul_string_literal) =
  map_string_ env x

let map_import_clause (env : env) (x : CST.import_clause) =
  match x with
  | `Single_import (v1, v2) ->
      let tstar = (* "*" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = (* "as" *) token env v1 in
            let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
            fun timport modname ->
              [
                ImportAs (timport, modname, Some (id, G.empty_id_info ()))
                |> G.d;
              ]
        | None ->
            fun timport modname ->
              [ ImportAll (timport, modname, tstar) |> G.d ]
      in
      v2
  | `Mult_import (v1, v2, v3) ->
      let _lb = (* "{" *) token env v1 in
      let xs =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_import_declaration env v1 in
            let v2 =
              List.map
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
        |> List.map (fun (id, aliasopt) ->
               ImportFrom (timport, modname, id, aliasopt) |> G.d)

let map_mapping_key (env : env) (x : CST.mapping_key) : type_ =
  match x with
  | `Prim_type x -> map_primitive_type env x
  | `User_defi_type x -> map_user_defined_type env x

let map_yul_literal (env : env) (x : CST.yul_literal) : literal =
  match x with
  | `Yul_deci_num tok ->
      (* pattern 0|([1-9][0-9]*\
         ) *)
      let s, t = str env tok in
      Int (Common2.int_of_string_c_octal_opt s, t)
  | `Yul_str_lit x ->
      let x = map_yul_string_literal env x in
      String x
  | `Yul_hex_num tok ->
      let s, t = (* pattern 0x[0-9A-Fa-f]* *) str env tok in
      Int (int_of_string_opt s, t)
  | `Yul_bool x ->
      let b = map_yul_or_literal_boolean env x in
      Bool b

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = (* "from" *) token env v1 in
  let v2 = map_yul_string_literal env v2 in
  todo env (v1, v2)

let map_source_import (env : env) ((v1, v2) : CST.source_import) =
  let v1 = map_yul_string_literal env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2)

let map_string_literal (env : env) (xs : CST.string_literal) =
  List.map (map_yul_string_literal env) xs

let rec map_yul_expression (env : env) (x : CST.yul_expression) : expr =
  match x with
  | `Yul_path x ->
      let ids = map_yul_path env x in
      todo env ids
  | `Yul_func_call x -> map_yul_function_call env x
  | `Yul_lit x ->
      let x = map_yul_literal env x in
      todo env x

and map_yul_function_call (env : env) ((v1, v2, v3, v4) : CST.yul_function_call)
    =
  let v1 =
    match v1 with
    | `Yul_id tok ->
        let x = (* pattern [a-zA-Z$_]+ *) token env tok in
        todo env x
    | `Yul_evm_buil x ->
        let x = map_yul_evm_builtin env x in
        todo env x
  in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2, v3) ->
        let v1 = map_yul_expression env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_yul_expression env v2 in
              v2)
            v2
        in
        let _v3 = map_trailing_comma env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_literal (env : env) (x : CST.literal) : expr =
  match x with
  | `Str_lit x ->
      let x = map_string_literal env x in
      todo env x
  | `Num_lit (v1, v2) ->
      let v1 =
        match v1 with
        | `Deci_num x ->
            let fopt, t = map_decimal_number env x in
            Float (fopt, t)
        | `Hex_num x ->
            let iopt, t = map_hex_number env x in
            Int (iopt, t)
      in
      let v2 =
        match v2 with
        | Some x ->
            let unit = map_number_unit env x in
            todo env unit
        | None -> L v1 |> G.e
      in
      v2
  | `Bool_lit x ->
      let x = map_yul_or_literal_boolean env x in
      L (Bool x) |> G.e
  | `Hex_str_lit x ->
      let xs = map_hex_string_literal env x in
      todo env xs
  | `Unic_str_lit x ->
      let xs = map_unicode_string_literal env x in
      todo env xs

let map_yul_variable_declaration (env : env) (x : CST.yul_variable_declaration)
    =
  match x with
  | `Let_yul_id_opt_COLONEQ_yul_exp (v1, v2, v3) ->
      let v1 = (* "let" *) token env v1 in
      let v2 = (* pattern [a-zA-Z$_]+ *) token env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = (* ":=" *) token env v1 in
            let v2 = map_yul_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Let_choice_yul_id_rep_COMMA_yul_id_opt_COMMA_opt_COLONEQ_yul_func_call
      (v1, v2, v3) ->
      let v1 = (* "let" *) token env v1 in
      let v2 =
        match v2 with
        | `Yul_id_rep_COMMA_yul_id_opt_COMMA x ->
            map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e env x
        | `LPAR_yul_id_rep_COMMA_yul_id_opt_COMMA_RPAR (v1, v2, v3, v4, v5) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = (* pattern [a-zA-Z$_]+ *) token env v2 in
            let v3 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
                  v2)
                v3
            in
            let _v4 = map_trailing_comma env v4 in
            let v5 = (* ")" *) token env v5 in
            todo env (v1, v2, v3, v4, v5)
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = (* ":=" *) token env v1 in
            let v2 = map_yul_function_call env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

let map_yul_assignment (env : env) (x : CST.yul_assignment) =
  match x with
  | `Yul_path_COLONEQ_yul_exp (v1, v2, v3) ->
      let v1 = map_yul_path env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_yul_expression env v3 in
      todo env (v1, v2, v3)
  | `Yul_path_rep_COMMA_yul_path_opt_COMMA_opt_COLONEQ_yul_func_call
      (v1, v2, v3, v4) ->
      let v1 = map_yul_path env v1 in
      let v2 =
        List.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_yul_path env v2 in
            v2)
          v2
      in
      let v3 = map_trailing_comma env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = (* ":=" *) token env v1 in
            let v2 = map_yul_function_call env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4)

let map_directive (env : env) (x : CST.directive) : directive =
  match x with
  | `Pragma_dire (v1, v2, v3, v4) ->
      let v1 = (* "pragma" *) token env v1 in
      let v2 = (* "solidity" *) str env v2 in
      let v3 = List.map (map_pragma_version_constraint env) v3 in
      let v4 = (* ";" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Import_dire (v1, v2, v3) ->
      let v1 = (* "import" *) token env v1 in
      let v2 =
        match v2 with
        | `Source_import x -> map_source_import env x
        | `Import_clause_from_clause (v1, v2) ->
            let v1 = map_import_clause env v1 in
            let v2 = map_from_clause env v2 in
            todo env (v1, v2)
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)

let rec map_anon_choice_exp_e762ef6 (env : env)
    (x : CST.anon_choice_exp_e762ef6) =
  match x with
  | `Exp x -> map_expression env x
  | `LCURL_opt_id_rep_COMMA_id_opt_COMMA_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_anon_id_rep_COMMA_id_opt_COMMA_e9ba3f8 env x
        | None -> todo env ()
      in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)

and map_array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) : expr =
  let v1 = map_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "]" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_call_arguments (env : env) ((v1, v2, v3) : CST.call_arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_anon_choice_exp_e762ef6 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_exp_e762ef6 env v2 in
              v2)
            v2
        in
        let v3 = map_trailing_comma env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Bin_exp x -> map_binary_expression env x
  | `Un_exp x -> map_unary_expression env x
  | `Update_exp x -> map_update_expression env x
  | `Call_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_call_arguments env v2 in
      todo env (v1, v2)
  | `Paya_conv_exp (v1, v2) ->
      let v1 = (* "payable" *) token env v1 in
      let v2 = map_call_arguments env v2 in
      todo env (v1, v2)
  | `Meta_type_exp (v1, v2, v3, v4) ->
      let v1 = (* "type" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_name env v3 in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Prim_exp x -> map_primary_expression env x
  | `Struct_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2, v3, v4, v5) ->
            let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1 in
            let v2 = (* ":" *) token env v2 in
            let v3 = map_expression env v3 in
            let v4 =
              List.map
                (fun (v1, v2, v3, v4) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
                  let v3 = (* ":" *) token env v3 in
                  let v4 = map_expression env v4 in
                  todo env (v1, v2, v3, v4))
                v4
            in
            let v5 = map_trailing_comma env v5 in
            todo env (v1, v2, v3, v4, v5)
        | None -> todo env ()
      in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Type_cast_exp (v1, v2, v3, v4) ->
      let v1 = map_primitive_type env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)

and map_lhs_expression (env : env) (x : CST.lhs_expression) : expr =
  match x with
  | `Member_exp x -> map_member_expression env x
  | `Array_access x -> map_array_access env x
  | `Id tok ->
      let x = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
      todo env x
  | `Tuple_exp x -> map_tuple_expression env x

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) :
    expr =
  let v1 =
    match v1 with
    | `Exp x -> map_expression env x
    | `Id tok ->
        let x = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok in
        todo env x
  in
  let v2 = (* "." *) token env v2 in
  let v3 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3 in
  todo env (v1, v2, v3)

and map_nameless_parameter (env : env) ((v1, v2) : CST.nameless_parameter) =
  let v1 = map_type_name env v1 in
  let v2 =
    match v2 with
    | Some x -> map_storage_location env x
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_parameter (env : env) ((v1, v2, v3) : CST.parameter) : parameter =
  let v1 = map_type_name env v1 in
  let v2 =
    match v2 with
    | Some x -> map_storage_location env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some tok -> (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_parameter env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_parameter env v2 in
              v2)
            v2
        in
        let v3 = map_trailing_comma env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : expr =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Member_exp x -> map_member_expression env x
  | `Array_access x -> map_array_access env x
  | `Slice_access (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* "]" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Prim_type x ->
      let x = map_primitive_type env x in
      todo env x
  | `Assign_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Paren_exp x -> map_parenthesized_expression env x
        | `Lhs_exp x -> map_lhs_expression env x
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let v1 = map_lhs_expression env v1 in
      let v2 =
        match v2 with
        | `PLUSEQ tok -> (* "+=" *) token env tok
        | `DASHEQ tok -> (* "-=" *) token env tok
        | `STAREQ tok -> (* "*=" *) token env tok
        | `SLASHEQ tok -> (* "/=" *) token env tok
        | `PERCEQ tok -> (* "%=" *) token env tok
        | `HATEQ tok -> (* "^=" *) token env tok
        | `AMPEQ tok -> (* "&=" *) token env tok
        | `BAREQ tok -> (* "|=" *) token env tok
        | `GTGTEQ tok -> (* ">>=" *) token env tok
        | `GTGTGTEQ tok -> (* ">>>=" *) token env tok
        | `LTLTEQ tok -> (* "<<=" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `User_defi_type x ->
      let x = map_user_defined_type env x in
      todo env x
  | `Tuple_exp x -> map_tuple_expression env x
  | `Inline_array_exp (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_expression env v2 in
                  v2)
                v2
            in
            let _v3 = map_trailing_comma env v3 in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `Id tok ->
      let x = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok in
      todo env x
  | `Lit x ->
      let x = map_literal env x in
      todo env x
  | `New_exp (v1, v2, v3) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = map_type_name env v2 in
      let v3 =
        match v3 with
        | Some x -> map_call_arguments env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_return_parameters (env : env)
    ((v1, v2, v3, v4, v5) : CST.return_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_nameless_parameter env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_nameless_parameter env v2 in
        v2)
      v3
  in
  let v4 = map_trailing_comma env v4 in
  let v5 = (* ")" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_tuple_expression (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_expression env x
    | None -> todo env ()
  in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_expression env x
          | None -> todo env ()
        in
        todo env (v1, v2))
      v3
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_type_name (env : env) (x : CST.type_name) : type_ =
  match x with
  | `Prim_type x -> map_primitive_type env x
  | `User_defi_type x ->
      let x = map_user_defined_type env x in
      todo env x
  | `Mapp (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "mapping" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_mapping_key env v3 in
      let v4 = (* "=>" *) token env v4 in
      let v5 = map_type_name env v5 in
      let v6 = (* ")" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Array_type (v1, v2, v3, v4) ->
      let v1 = map_type_name env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_expression env x
        | None -> todo env ()
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_type (v1, v2, v3) ->
      let v1 = (* "function" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        match v3 with
        | Some x -> map_return_parameters env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_unary_expression (env : env) (x : CST.unary_expression) : expr =
  match x with
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASH_exp (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `PLUS_exp (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Delete_exp (v1, v2) ->
      let v1 = (* "delete" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

and map_update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      todo env (v1, v2)
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let v1 = map_anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

let rec map_yul_block (env : env) ((v1, v2, v3) : CST.yul_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_yul_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_yul_statement (env : env) (x : CST.yul_statement) : stmt =
  match x with
  | `Yul_blk x -> map_yul_block env x
  | `Yul_var_decl x ->
      let x = map_yul_variable_declaration env x in
      todo env x
  | `Yul_assign x ->
      let x = map_yul_assignment env x in
      todo env x
  | `Yul_func_call x ->
      let x = map_yul_function_call env x in
      todo env x
  | `Yul_if_stmt (v1, v2, v3) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_yul_expression env v2 in
      let v3 = map_yul_block env v3 in
      todo env (v1, v2, v3)
  | `Yul_for_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = map_yul_block env v2 in
      let v3 = map_yul_expression env v3 in
      let v4 = map_yul_block env v4 in
      let v5 = map_yul_block env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Yul_switch_stmt (v1, v2, v3) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_yul_expression env v2 in
      let v3 =
        match v3 with
        | `Defa_yul_blk (v1, v2) ->
            let v1 = (* "default" *) token env v1 in
            let v2 = map_yul_block env v2 in
            todo env (v1, v2)
        | `Rep1_case_yul_lit_yul_blk_opt_defa_yul_blk (v1, v2) ->
            let v1 =
              List.map
                (fun (v1, v2, v3) ->
                  let v1 = (* "case" *) token env v1 in
                  let v2 = map_yul_literal env v2 in
                  let v3 = map_yul_block env v3 in
                  todo env (v1, v2, v3))
                v1
            in
            let v2 =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = (* "default" *) token env v1 in
                  let v2 = map_yul_block env v2 in
                  todo env (v1, v2)
              | None -> todo env ()
            in
            todo env (v1, v2)
      in
      todo env (v1, v2, v3)
  | `Yul_leave tok ->
      let x = (* "leave" *) token env tok in
      todo env x
  | `Yul_brk tok ->
      let x = (* "break" *) token env tok in
      todo env x
  | `Yul_cont tok ->
      let x = (* "continue" *) token env tok in
      todo env x
  | `Yul_func_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "function" *) token env v1 in
      let v2 = (* pattern [a-zA-Z$_]+ *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        match v4 with
        | Some x -> map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e env x
        | None -> todo env ()
      in
      let v5 = (* ")" *) token env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2, v3, v4) ->
            let v1 = (* "->" *) token env v1 in
            let v2 = (* pattern [a-zA-Z$_]+ *) token env v2 in
            let v3 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = (* pattern [a-zA-Z$_]+ *) str env v2 in
                  v2)
                v3
            in
            let _v4 = map_trailing_comma env v4 in
            todo env (v1, v2, v3, v4)
        | None -> todo env ()
      in
      let v7 = map_yul_block env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)

let map_state_variable_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.state_variable_declaration) : definition =
  let v1 = map_type_name env v1 in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Visi x ->
            let x = map_visibility env x in
            x
        | `Cst tok ->
            let x = (* "constant" *) token env tok in
            G.attr Const x
        | `Over_spec x ->
            let x = map_override_specifier env x in
            x
        (* TODO: difference between constant and immutable? *)
        | `Immu tok ->
            let x = (* "immutable" *) str env tok in
            G.unhandled_keywordattr x)
      v2
  in
  let v3 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v5 = (* ";" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_modifier_invocation (env : env) ((v1, v2) : CST.modifier_invocation) =
  let v1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments env x
    | None -> todo env ()
  in
  todo env (v1, v2)

let map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expression env v1 in
  let v2 = (* ";" *) token env v2 in
  todo env (v1, v2)

let map_struct_member (env : env) ((v1, v2, v3) : CST.struct_member) =
  let v1 = map_type_name env v1 in
  let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
  let v3 = (* ";" *) token env v3 in
  todo env (v1, v2, v3)

let map_inheritance_specifier (env : env) ((v1, v2) : CST.inheritance_specifier)
    =
  let v1 = map_user_defined_type env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments env x
    | None -> todo env ()
  in
  todo env (v1, v2)

let map_event_paramater (env : env) ((v1, v2, v3) : CST.event_paramater) =
  let v1 = map_type_name env v1 in
  let v2 =
    match v2 with
    | Some tok -> (* "indexed" *) token env tok
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some tok -> (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

let map_return_type_definition (env : env)
    ((v1, v2) : CST.return_type_definition) =
  let v1 = (* "returns" *) token env v1 in
  let v2 = map_parameter_list env v2 in
  todo env (v1, v2)

let map_variable_declaration (env : env)
    ((v1, v2, v3) : CST.variable_declaration) =
  let v1 = map_type_name env v1 in
  let v2 =
    match v2 with
    | Some x -> map_storage_location env x
    | None -> todo env ()
  in
  let v3 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
  todo env (v1, v2, v3)

let map_using_directive (env : env) ((v1, v2, v3, v4, v5) : CST.using_directive)
    =
  let v1 = (* "using" *) token env v1 in
  let v2 = map_user_defined_type env v2 in
  let v3 = (* "for" *) token env v3 in
  let v4 =
    match v4 with
    | `Any_source_type tok ->
        let x = (* "*" *) token env tok in
        todo env x
    | `Type_name x ->
        let x = map_type_name env x in
        todo env x
  in
  let v5 = (* ";" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_struct_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.struct_declaration) : definition =
  let v1 = (* "struct" *) token env v1 in
  let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 = List.map (map_struct_member env) v4 in
  let v5 = (* "}" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_class_heritage (env : env) ((v1, v2, v3, v4) : CST.class_heritage) =
  let v1 = (* "is" *) token env v1 in
  let v2 = map_inheritance_specifier env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_inheritance_specifier env v2 in
        v2)
      v3
  in
  let _v4 = map_trailing_comma env v4 in
  todo env (v1, v2, v3, v4)

let map_event_parameter_list (env : env)
    ((v1, v2, v3) : CST.event_parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_event_paramater env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_event_paramater env v2 in
              v2)
            v2
        in
        let _v3 = map_trailing_comma env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

let map_variable_declaration_tuple (env : env)
    (x : CST.variable_declaration_tuple) =
  match x with
  | `LPAR_opt_var_decl_rep_COMMA_var_decl_opt_COMMA_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_variable_declaration env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_variable_declaration env v2 in
                  v2)
                v2
            in
            let _v3 = map_trailing_comma env v3 in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Var_LPAR_opt_id_rep_COMMA_opt_id_RPAR (v1, v2, v3, v4, v5) ->
      let v1 = (* "var" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some tok -> (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
        | None -> todo env ()
      in
      let v4 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              match v2 with
              | Some tok -> (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
              | None -> todo env ()
            in
            todo env (v1, v2))
          v4
      in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)

let map_event_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.event_definition) : definition =
  let v1 = (* "event" *) token env v1 in
  let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let v3 = map_event_parameter_list env v3 in
  let v4 =
    match v4 with
    | Some tok -> (* "anonymous" *) token env tok
    | None -> todo env ()
  in
  let v5 = (* ";" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_variable_declaration_statement (env : env)
    ((v1, v2) : CST.variable_declaration_statement) =
  let v1 =
    match v1 with
    | `Var_decl_opt_EQ_exp (v1, v2) ->
        let v1 = map_variable_declaration env v1 in
        let v2 =
          match v2 with
          | Some (v1, v2) ->
              let v1 = (* "=" *) token env v1 in
              let v2 = map_expression env v2 in
              todo env (v1, v2)
          | None -> todo env ()
        in
        todo env (v1, v2)
    | `Var_decl_tuple_EQ_exp (v1, v2, v3) ->
        let v1 = map_variable_declaration_tuple env v1 in
        let v2 = (* "=" *) token env v2 in
        let v3 = map_expression env v3 in
        todo env (v1, v2, v3)
  in
  let v2 = (* ";" *) token env v2 in
  todo env (v1, v2)

let rec map_block_statement (env : env) ((v1, v2, v3) : CST.block_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some tok -> (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
          | None -> todo env ()
        in
        let v2 = map_parameter_list env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 = map_block_statement env v3 in
  todo env (v1, v2, v3)

and map_statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `Blk_stmt x -> map_block_statement env x
  | `Exp_stmt x -> map_expression_statement env x
  | `Var_decl_stmt x -> map_variable_declaration_statement env x
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = (* "else" *) token env v1 in
            let v2 = map_statement env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | `Var_decl_stmt x -> map_variable_declaration_statement env x
        | `Exp_stmt x -> map_expression_statement env x
        | `Semi tok -> (* ";" *) token env tok
      in
      let v4 =
        match v4 with
        | `Exp_stmt x -> map_expression_statement env x
        | `Semi tok -> (* ";" *) token env tok
      in
      let v5 =
        match v5 with
        | Some x -> map_expression env x
        | None -> todo env ()
      in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_statement env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = (* "(" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Cont_stmt (v1, v2) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      todo env (v1, v2)
  | `Brk_stmt (v1, v2) ->
      let v1 = (* "break" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      todo env (v1, v2)
  | `Try_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        match v3 with
        | Some x -> map_return_type_definition env x
        | None -> todo env ()
      in
      let v4 = map_block_statement env v4 in
      let v5 = List.map (map_catch_clause env) v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_expression env x
        | None -> todo env ()
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Emit_stmt (v1, v2, v3, v4) ->
      let v1 = (* "emit" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_call_arguments env v3 in
      let v4 = (* ";" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Asse_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "assembly" *) token env v1 in
      let v2 =
        match v2 with
        | Some tok -> (* "\"evmasm\"" *) token env tok
        | None -> todo env ()
      in
      let v3 = (* "{" *) token env v3 in
      let v4 = List.map (map_yul_statement env) v4 in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)

let map_function_body (env : env) ((v1, v2, v3) : CST.function_body) :
    function_body =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

let map_constructor_definition (env : env)
    ((v1, v2, v3, v4) : CST.constructor_definition) =
  let v1 = (* "constructor" *) token env v1 in
  let v2 = map_parameter_list env v2 in
  let v3 =
    List.map
      (fun x ->
        match x with
        | `Modi_invo x -> map_modifier_invocation env x
        | `Paya tok -> (* "payable" *) token env tok
        | `Choice_inte x -> (
            match x with
            | `Inte tok -> (* "internal" *) token env tok
            | `Public tok -> (* "public" *) token env tok))
      v3
  in
  let v4 = map_function_body env v4 in
  todo env (v1, v2, v3, v4)

let map_anon_choice_semi_f2fe6be (env : env) (x : CST.anon_choice_semi_f2fe6be)
    =
  match x with
  | `Semi tok ->
      let x = (* ";" *) token env tok in
      todo env x
  | `Func_body x ->
      let x = map_function_body env x in
      todo env x

let visi_and_co env x : attribute =
  match x with
  | `Visi x ->
      let x = map_visibility env x in
      todo env x
  | `Modi_invo x ->
      let x = map_modifier_invocation env x in
      todo env x
  | `State_muta x ->
      let x = map_state_mutability env x in
      todo env x
  | `Virt tok ->
      let x = (* "virtual" *) token env tok in
      todo env x
  | `Over_spec x ->
      let x = map_override_specifier env x in
      todo env x

let map_fallback_receive_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.fallback_receive_definition) =
  let v1 =
    match v1 with
    | `Opt_func_choice_fall (v1, v2) ->
        let v1 =
          match v1 with
          | Some tok -> (* "function" *) token env tok
          | None -> todo env ()
        in
        let v2 =
          match v2 with
          | `Fall tok -> (* "fallback" *) token env tok
          | `Rece tok -> (* "receive" *) token env tok
        in
        todo env (v1, v2)
    | `Func tok -> (* "function" *) token env tok
  in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 = List.map (fun x -> visi_and_co env x) v4 in
  let v5 = map_anon_choice_semi_f2fe6be env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_function_definition (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.function_definition) : definition =
  let v1 = (* "function" *) token env v1 in
  let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let v3 = map_parameter_list env v3 in
  let v4 = List.map (fun x -> visi_and_co env x) v4 in
  let v5 =
    match v5 with
    | Some x -> map_return_type_definition env x
    | None -> todo env ()
  in
  let v6 = map_anon_choice_semi_f2fe6be env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let map_modifier_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.modifier_definition) =
  let v1 = (* "modifier" *) token env v1 in
  let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_parameter_list env x
    | None -> todo env ()
  in
  let v4 =
    List.map
      (fun x ->
        match x with
        | `Virt tok ->
            let t = (* "virtual" *) token env tok in
            G.attr Abstract t
        | `Over_spec x -> map_override_specifier env x)
      v4
  in
  let v5 = map_anon_choice_semi_f2fe6be env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_contract_body (env : env) ((v1, v2, v3) : CST.contract_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Func_defi x -> map_function_definition env x
        | `Modi_defi x -> map_modifier_definition env x
        | `State_var_decl x -> map_state_variable_declaration env x
        | `Struct_decl x -> map_struct_declaration env x
        | `Enum_decl x -> map_enum_declaration env x
        | `Event_defi x -> map_event_definition env x
        | `Using_dire x -> map_using_directive env x
        | `Cons_defi x -> map_constructor_definition env x
        | `Fall_rece_defi x -> map_fallback_receive_definition env x)
      v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

let map_declaration (env : env) (x : CST.declaration) : definition =
  match x with
  | `Cont_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some tok -> (* "abstract" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "contract" *) token env v2 in
      let v3 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3 in
      let v4 =
        match v4 with
        | Some x -> map_class_heritage env x
        | None -> todo env ()
      in
      let v5 = map_contract_body env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Inte_decl (v1, v2, v3, v4) ->
      let v1 = (* "interface" *) token env v1 in
      let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_class_heritage env x
        | None -> todo env ()
      in
      let v4 = map_contract_body env v4 in
      todo env (v1, v2, v3, v4)
  | `Libr_decl (v1, v2, v3) ->
      let v1 = (* "library" *) token env v1 in
      let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
      let v3 = map_contract_body env v3 in
      todo env (v1, v2, v3)
  | `Struct_decl x -> map_struct_declaration env x
  | `Enum_decl x -> map_enum_declaration env x
  | `Func_defi x -> map_function_definition env x
  | `Cst_var_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_type_name env v1 in
      let v2 = (* "constant" *) token env v2 in
      let v3 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ";" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)

let map_source_unit (env : env) (x : CST.source_unit) : item =
  match x with
  | `Dire x ->
      let x = map_directive env x in
      todo env x
  | `Decl x ->
      let x = map_declaration env x in
      todo env x

let map_source_file (env : env) (v1 : CST.source_file) =
  let xs = List.map (map_source_unit env) v1 in
  G.Pr xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_solidity.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern _str = failwith "TODO"

(*
let parse_expression_or_source_file str =
  let res = Tree_sitter_kotlin.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_kotlin.Parse.string expr_str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match source_file env cst with
      | G.Pr [ x ] -> G.S x
      | G.Pr xs -> G.Ss xs
      | x -> x)
 *)

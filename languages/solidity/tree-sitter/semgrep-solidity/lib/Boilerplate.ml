(**
   Boilerplate to be used as a template when mapping the solidity CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_double_quoted_unicode_char (env : env) (tok : CST.double_quoted_unicode_char) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_uint (env : env) (x : CST.uint) =
  (match x with
  | `Uint tok -> R.Case ("Uint",
      (* "uint" *) token env tok
    )
  | `Uint8 tok -> R.Case ("Uint8",
      (* "uint8" *) token env tok
    )
  | `Uint16 tok -> R.Case ("Uint16",
      (* "uint16" *) token env tok
    )
  | `Uint24 tok -> R.Case ("Uint24",
      (* "uint24" *) token env tok
    )
  | `Uint32 tok -> R.Case ("Uint32",
      (* "uint32" *) token env tok
    )
  | `Uint40 tok -> R.Case ("Uint40",
      (* "uint40" *) token env tok
    )
  | `Uint48 tok -> R.Case ("Uint48",
      (* "uint48" *) token env tok
    )
  | `Uint56 tok -> R.Case ("Uint56",
      (* "uint56" *) token env tok
    )
  | `Uint64 tok -> R.Case ("Uint64",
      (* "uint64" *) token env tok
    )
  | `Uint72 tok -> R.Case ("Uint72",
      (* "uint72" *) token env tok
    )
  | `Uint80 tok -> R.Case ("Uint80",
      (* "uint80" *) token env tok
    )
  | `Uint88 tok -> R.Case ("Uint88",
      (* "uint88" *) token env tok
    )
  | `Uint96 tok -> R.Case ("Uint96",
      (* "uint96" *) token env tok
    )
  | `Uint104 tok -> R.Case ("Uint104",
      (* "uint104" *) token env tok
    )
  | `Uint112 tok -> R.Case ("Uint112",
      (* "uint112" *) token env tok
    )
  | `Uint120 tok -> R.Case ("Uint120",
      (* "uint120" *) token env tok
    )
  | `Uint128 tok -> R.Case ("Uint128",
      (* "uint128" *) token env tok
    )
  | `Uint136 tok -> R.Case ("Uint136",
      (* "uint136" *) token env tok
    )
  | `Uint144 tok -> R.Case ("Uint144",
      (* "uint144" *) token env tok
    )
  | `Uint152 tok -> R.Case ("Uint152",
      (* "uint152" *) token env tok
    )
  | `Uint160 tok -> R.Case ("Uint160",
      (* "uint160" *) token env tok
    )
  | `Uint168 tok -> R.Case ("Uint168",
      (* "uint168" *) token env tok
    )
  | `Uint176 tok -> R.Case ("Uint176",
      (* "uint176" *) token env tok
    )
  | `Uint184 tok -> R.Case ("Uint184",
      (* "uint184" *) token env tok
    )
  | `Uint192 tok -> R.Case ("Uint192",
      (* "uint192" *) token env tok
    )
  | `Uint200 tok -> R.Case ("Uint200",
      (* "uint200" *) token env tok
    )
  | `Uint208 tok -> R.Case ("Uint208",
      (* "uint208" *) token env tok
    )
  | `Uint216 tok -> R.Case ("Uint216",
      (* "uint216" *) token env tok
    )
  | `Uint224 tok -> R.Case ("Uint224",
      (* "uint224" *) token env tok
    )
  | `Uint232 tok -> R.Case ("Uint232",
      (* "uint232" *) token env tok
    )
  | `Uint240 tok -> R.Case ("Uint240",
      (* "uint240" *) token env tok
    )
  | `Uint248 tok -> R.Case ("Uint248",
      (* "uint248" *) token env tok
    )
  | `Uint256 tok -> R.Case ("Uint256",
      (* "uint256" *) token env tok
    )
  )

let map_user_definable_operator (env : env) (x : CST.user_definable_operator) =
  (match x with
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `SLASH tok -> R.Case ("SLASH",
      (* "/" *) token env tok
    )
  | `PERC tok -> R.Case ("PERC",
      (* "%" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  )

let map_single_quoted_unicode_char (env : env) (tok : CST.single_quoted_unicode_char) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_pat_ac20a0c (env : env) (tok : CST.pat_ac20a0c) =
  (* pattern \.(\d|_)+([eE](-)?(\d|_)+)? *) token env tok

let map_string_immediate_elt_inside_quote (env : env) (tok : CST.string_immediate_elt_inside_quote) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_int_ (env : env) (x : CST.int_) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* "int" *) token env tok
    )
  | `Int8 tok -> R.Case ("Int8",
      (* "int8" *) token env tok
    )
  | `Int16 tok -> R.Case ("Int16",
      (* "int16" *) token env tok
    )
  | `Int24 tok -> R.Case ("Int24",
      (* "int24" *) token env tok
    )
  | `Int32 tok -> R.Case ("Int32",
      (* "int32" *) token env tok
    )
  | `Int40 tok -> R.Case ("Int40",
      (* "int40" *) token env tok
    )
  | `Int48 tok -> R.Case ("Int48",
      (* "int48" *) token env tok
    )
  | `Int56 tok -> R.Case ("Int56",
      (* "int56" *) token env tok
    )
  | `Int64 tok -> R.Case ("Int64",
      (* "int64" *) token env tok
    )
  | `Int72 tok -> R.Case ("Int72",
      (* "int72" *) token env tok
    )
  | `Int80 tok -> R.Case ("Int80",
      (* "int80" *) token env tok
    )
  | `Int88 tok -> R.Case ("Int88",
      (* "int88" *) token env tok
    )
  | `Int96 tok -> R.Case ("Int96",
      (* "int96" *) token env tok
    )
  | `Int104 tok -> R.Case ("Int104",
      (* "int104" *) token env tok
    )
  | `Int112 tok -> R.Case ("Int112",
      (* "int112" *) token env tok
    )
  | `Int120 tok -> R.Case ("Int120",
      (* "int120" *) token env tok
    )
  | `Int128 tok -> R.Case ("Int128",
      (* "int128" *) token env tok
    )
  | `Int136 tok -> R.Case ("Int136",
      (* "int136" *) token env tok
    )
  | `Int144 tok -> R.Case ("Int144",
      (* "int144" *) token env tok
    )
  | `Int152 tok -> R.Case ("Int152",
      (* "int152" *) token env tok
    )
  | `Int160 tok -> R.Case ("Int160",
      (* "int160" *) token env tok
    )
  | `Int168 tok -> R.Case ("Int168",
      (* "int168" *) token env tok
    )
  | `Int176 tok -> R.Case ("Int176",
      (* "int176" *) token env tok
    )
  | `Int184 tok -> R.Case ("Int184",
      (* "int184" *) token env tok
    )
  | `Int192 tok -> R.Case ("Int192",
      (* "int192" *) token env tok
    )
  | `Int200 tok -> R.Case ("Int200",
      (* "int200" *) token env tok
    )
  | `Int208 tok -> R.Case ("Int208",
      (* "int208" *) token env tok
    )
  | `Int216 tok -> R.Case ("Int216",
      (* "int216" *) token env tok
    )
  | `Int224 tok -> R.Case ("Int224",
      (* "int224" *) token env tok
    )
  | `Int232 tok -> R.Case ("Int232",
      (* "int232" *) token env tok
    )
  | `Int240 tok -> R.Case ("Int240",
      (* "int240" *) token env tok
    )
  | `Int248 tok -> R.Case ("Int248",
      (* "int248" *) token env tok
    )
  | `Int256 tok -> R.Case ("Int256",
      (* "int256" *) token env tok
    )
  )

let map_pat_accdbe2 (env : env) (tok : CST.pat_accdbe2) =
  (* pattern ufixed([0-9]+)x([0-9]+) *) token env tok

let map_pat_585ba4d (env : env) (tok : CST.pat_585ba4d) =
  (* pattern (\d|_)+(\.(\d|_)+)?([eE](-)?(\d|_)+)? *) token env tok

let map_yul_decimal_number (env : env) (tok : CST.yul_decimal_number) =
  (* pattern 0|([1-9][0-9]*\
  ) *) token env tok

let map_solidity_version (env : env) (tok : CST.solidity_version) =
  (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) token env tok

let map_bytes_ (env : env) (x : CST.bytes_) =
  (match x with
  | `Byte tok -> R.Case ("Byte",
      (* "byte" *) token env tok
    )
  | `Bytes tok -> R.Case ("Bytes",
      (* "bytes" *) token env tok
    )
  | `Bytes1 tok -> R.Case ("Bytes1",
      (* "bytes1" *) token env tok
    )
  | `Bytes2 tok -> R.Case ("Bytes2",
      (* "bytes2" *) token env tok
    )
  | `Bytes3 tok -> R.Case ("Bytes3",
      (* "bytes3" *) token env tok
    )
  | `Bytes4 tok -> R.Case ("Bytes4",
      (* "bytes4" *) token env tok
    )
  | `Bytes5 tok -> R.Case ("Bytes5",
      (* "bytes5" *) token env tok
    )
  | `Bytes6 tok -> R.Case ("Bytes6",
      (* "bytes6" *) token env tok
    )
  | `Bytes7 tok -> R.Case ("Bytes7",
      (* "bytes7" *) token env tok
    )
  | `Bytes8 tok -> R.Case ("Bytes8",
      (* "bytes8" *) token env tok
    )
  | `Bytes9 tok -> R.Case ("Bytes9",
      (* "bytes9" *) token env tok
    )
  | `Bytes10 tok -> R.Case ("Bytes10",
      (* "bytes10" *) token env tok
    )
  | `Bytes11 tok -> R.Case ("Bytes11",
      (* "bytes11" *) token env tok
    )
  | `Bytes12 tok -> R.Case ("Bytes12",
      (* "bytes12" *) token env tok
    )
  | `Bytes13 tok -> R.Case ("Bytes13",
      (* "bytes13" *) token env tok
    )
  | `Bytes14 tok -> R.Case ("Bytes14",
      (* "bytes14" *) token env tok
    )
  | `Bytes15 tok -> R.Case ("Bytes15",
      (* "bytes15" *) token env tok
    )
  | `Bytes16 tok -> R.Case ("Bytes16",
      (* "bytes16" *) token env tok
    )
  | `Bytes17 tok -> R.Case ("Bytes17",
      (* "bytes17" *) token env tok
    )
  | `Bytes18 tok -> R.Case ("Bytes18",
      (* "bytes18" *) token env tok
    )
  | `Bytes19 tok -> R.Case ("Bytes19",
      (* "bytes19" *) token env tok
    )
  | `Bytes20 tok -> R.Case ("Bytes20",
      (* "bytes20" *) token env tok
    )
  | `Bytes21 tok -> R.Case ("Bytes21",
      (* "bytes21" *) token env tok
    )
  | `Bytes22 tok -> R.Case ("Bytes22",
      (* "bytes22" *) token env tok
    )
  | `Bytes23 tok -> R.Case ("Bytes23",
      (* "bytes23" *) token env tok
    )
  | `Bytes24 tok -> R.Case ("Bytes24",
      (* "bytes24" *) token env tok
    )
  | `Bytes25 tok -> R.Case ("Bytes25",
      (* "bytes25" *) token env tok
    )
  | `Bytes26 tok -> R.Case ("Bytes26",
      (* "bytes26" *) token env tok
    )
  | `Bytes27 tok -> R.Case ("Bytes27",
      (* "bytes27" *) token env tok
    )
  | `Bytes28 tok -> R.Case ("Bytes28",
      (* "bytes28" *) token env tok
    )
  | `Bytes29 tok -> R.Case ("Bytes29",
      (* "bytes29" *) token env tok
    )
  | `Bytes30 tok -> R.Case ("Bytes30",
      (* "bytes30" *) token env tok
    )
  | `Bytes31 tok -> R.Case ("Bytes31",
      (* "bytes31" *) token env tok
    )
  | `Bytes32 tok -> R.Case ("Bytes32",
      (* "bytes32" *) token env tok
    )
  )

let map_yul_hex_number (env : env) (tok : CST.yul_hex_number) =
  (* pattern 0x[0-9A-Fa-f]* *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_state_location (env : env) (x : CST.state_location) =
  (match x with
  | `Tran tok -> R.Case ("Tran",
      (* "transient" *) token env tok
    )
  )

let map_pat_2b7bb84 (env : env) (tok : CST.pat_2b7bb84) =
  (* pattern 0[xX]([a-fA-F0-9][a-fA-F0-9]?_?)+ *) token env tok

let map_yul_assignment_operator (env : env) (x : CST.yul_assignment_operator) =
  (match x with
  | `COLONEQ tok -> R.Case ("COLONEQ",
      (* ":=" *) token env tok
    )
  | `COLON_EQ (v1, v2) -> R.Case ("COLON_EQ",
      let v1 = (* ":" *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_yul_evm_builtin (env : env) (x : CST.yul_evm_builtin) =
  (match x with
  | `Stop tok -> R.Case ("Stop",
      (* "stop" *) token env tok
    )
  | `Add tok -> R.Case ("Add",
      (* "add" *) token env tok
    )
  | `Sub tok -> R.Case ("Sub",
      (* "sub" *) token env tok
    )
  | `Mul tok -> R.Case ("Mul",
      (* "mul" *) token env tok
    )
  | `Div tok -> R.Case ("Div",
      (* "div" *) token env tok
    )
  | `Sdiv tok -> R.Case ("Sdiv",
      (* "sdiv" *) token env tok
    )
  | `Mod tok -> R.Case ("Mod",
      (* "mod" *) token env tok
    )
  | `Smod tok -> R.Case ("Smod",
      (* "smod" *) token env tok
    )
  | `Exp tok -> R.Case ("Exp",
      (* "exp" *) token env tok
    )
  | `Not tok -> R.Case ("Not",
      (* "not" *) token env tok
    )
  | `Lt tok -> R.Case ("Lt",
      (* "lt" *) token env tok
    )
  | `Gt tok -> R.Case ("Gt",
      (* "gt" *) token env tok
    )
  | `Slt tok -> R.Case ("Slt",
      (* "slt" *) token env tok
    )
  | `Sgt tok -> R.Case ("Sgt",
      (* "sgt" *) token env tok
    )
  | `Eq tok -> R.Case ("Eq",
      (* "eq" *) token env tok
    )
  | `Iszero tok -> R.Case ("Iszero",
      (* "iszero" *) token env tok
    )
  | `And tok -> R.Case ("And",
      (* "and" *) token env tok
    )
  | `Or tok -> R.Case ("Or",
      (* "or" *) token env tok
    )
  | `Xor tok -> R.Case ("Xor",
      (* "xor" *) token env tok
    )
  | `Byte tok -> R.Case ("Byte",
      (* "byte" *) token env tok
    )
  | `Shl tok -> R.Case ("Shl",
      (* "shl" *) token env tok
    )
  | `Shr tok -> R.Case ("Shr",
      (* "shr" *) token env tok
    )
  | `Sar tok -> R.Case ("Sar",
      (* "sar" *) token env tok
    )
  | `Addmod tok -> R.Case ("Addmod",
      (* "addmod" *) token env tok
    )
  | `Mulmod tok -> R.Case ("Mulmod",
      (* "mulmod" *) token env tok
    )
  | `Sign tok -> R.Case ("Sign",
      (* "signextend" *) token env tok
    )
  | `Keccak256 tok -> R.Case ("Keccak256",
      (* "keccak256" *) token env tok
    )
  | `Pop tok -> R.Case ("Pop",
      (* "pop" *) token env tok
    )
  | `Mload tok -> R.Case ("Mload",
      (* "mload" *) token env tok
    )
  | `Mcopy tok -> R.Case ("Mcopy",
      (* "mcopy" *) token env tok
    )
  | `Tload tok -> R.Case ("Tload",
      (* "tload" *) token env tok
    )
  | `Tstore tok -> R.Case ("Tstore",
      (* "tstore" *) token env tok
    )
  | `Mstore tok -> R.Case ("Mstore",
      (* "mstore" *) token env tok
    )
  | `Mstore8 tok -> R.Case ("Mstore8",
      (* "mstore8" *) token env tok
    )
  | `Sload tok -> R.Case ("Sload",
      (* "sload" *) token env tok
    )
  | `Sstore tok -> R.Case ("Sstore",
      (* "sstore" *) token env tok
    )
  | `Msize tok -> R.Case ("Msize",
      (* "msize" *) token env tok
    )
  | `Gas tok -> R.Case ("Gas",
      (* "gas" *) token env tok
    )
  | `Addr tok -> R.Case ("Addr",
      (* "address" *) token env tok
    )
  | `Bala tok -> R.Case ("Bala",
      (* "balance" *) token env tok
    )
  | `Self_e34af40 tok -> R.Case ("Self_e34af40",
      (* "selfbalance" *) token env tok
    )
  | `Caller tok -> R.Case ("Caller",
      (* "caller" *) token env tok
    )
  | `Call_17bffc7 tok -> R.Case ("Call_17bffc7",
      (* "callvalue" *) token env tok
    )
  | `Call_b766e35 tok -> R.Case ("Call_b766e35",
      (* "calldataload" *) token env tok
    )
  | `Call_ee2b8b2 tok -> R.Case ("Call_ee2b8b2",
      (* "calldatasize" *) token env tok
    )
  | `Call_9211e8b tok -> R.Case ("Call_9211e8b",
      (* "calldatacopy" *) token env tok
    )
  | `Extc_8cf31ff tok -> R.Case ("Extc_8cf31ff",
      (* "extcodesize" *) token env tok
    )
  | `Extc_097e5c5 tok -> R.Case ("Extc_097e5c5",
      (* "extcodecopy" *) token env tok
    )
  | `Retu_6316777 tok -> R.Case ("Retu_6316777",
      (* "returndatasize" *) token env tok
    )
  | `Retu_0c570b4 tok -> R.Case ("Retu_0c570b4",
      (* "returndatacopy" *) token env tok
    )
  | `Extc_d7340e7 tok -> R.Case ("Extc_d7340e7",
      (* "extcodehash" *) token env tok
    )
  | `Create tok -> R.Case ("Create",
      (* "create" *) token env tok
    )
  | `Create2 tok -> R.Case ("Create2",
      (* "create2" *) token env tok
    )
  | `Call_53b9e96 tok -> R.Case ("Call_53b9e96",
      (* "call" *) token env tok
    )
  | `Call_bebd5bc tok -> R.Case ("Call_bebd5bc",
      (* "callcode" *) token env tok
    )
  | `Dele tok -> R.Case ("Dele",
      (* "delegatecall" *) token env tok
    )
  | `Stat tok -> R.Case ("Stat",
      (* "staticcall" *) token env tok
    )
  | `Ret tok -> R.Case ("Ret",
      (* "return" *) token env tok
    )
  | `Revert tok -> R.Case ("Revert",
      (* "revert" *) token env tok
    )
  | `Self_482b767 tok -> R.Case ("Self_482b767",
      (* "selfdestruct" *) token env tok
    )
  | `Inva tok -> R.Case ("Inva",
      (* "invalid" *) token env tok
    )
  | `Log0 tok -> R.Case ("Log0",
      (* "log0" *) token env tok
    )
  | `Log1 tok -> R.Case ("Log1",
      (* "log1" *) token env tok
    )
  | `Log2 tok -> R.Case ("Log2",
      (* "log2" *) token env tok
    )
  | `Log3 tok -> R.Case ("Log3",
      (* "log3" *) token env tok
    )
  | `Log4 tok -> R.Case ("Log4",
      (* "log4" *) token env tok
    )
  | `Chai tok -> R.Case ("Chai",
      (* "chainid" *) token env tok
    )
  | `Origin tok -> R.Case ("Origin",
      (* "origin" *) token env tok
    )
  | `Gasp tok -> R.Case ("Gasp",
      (* "gasprice" *) token env tok
    )
  | `Bloc tok -> R.Case ("Bloc",
      (* "blockhash" *) token env tok
    )
  | `Blob_27691aa tok -> R.Case ("Blob_27691aa",
      (* "blobhash" *) token env tok
    )
  | `Base tok -> R.Case ("Base",
      (* "basefee" *) token env tok
    )
  | `Blob_462c371 tok -> R.Case ("Blob_462c371",
      (* "blobfee" *) token env tok
    )
  | `Coin tok -> R.Case ("Coin",
      (* "coinbase" *) token env tok
    )
  | `Time tok -> R.Case ("Time",
      (* "timestamp" *) token env tok
    )
  | `Num tok -> R.Case ("Num",
      (* "number" *) token env tok
    )
  | `Diff tok -> R.Case ("Diff",
      (* "difficulty" *) token env tok
    )
  | `Gasl tok -> R.Case ("Gasl",
      (* "gaslimit" *) token env tok
    )
  | `Prev tok -> R.Case ("Prev",
      (* "prevrandao" *) token env tok
    )
  | `Blob_9a8d86b tok -> R.Case ("Blob_9a8d86b",
      (* "blobbasefee" *) token env tok
    )
  )

let map_pat_c5921c8 (env : env) (tok : CST.pat_c5921c8) =
  (* pattern [^;]+ *) token env tok

let map_number_unit (env : env) (x : CST.number_unit) =
  (match x with
  | `Wei tok -> R.Case ("Wei",
      (* "wei" *) token env tok
    )
  | `Szabo tok -> R.Case ("Szabo",
      (* "szabo" *) token env tok
    )
  | `Finney tok -> R.Case ("Finney",
      (* "finney" *) token env tok
    )
  | `Gwei tok -> R.Case ("Gwei",
      (* "gwei" *) token env tok
    )
  | `Ether tok -> R.Case ("Ether",
      (* "ether" *) token env tok
    )
  | `Seconds tok -> R.Case ("Seconds",
      (* "seconds" *) token env tok
    )
  | `Minutes tok -> R.Case ("Minutes",
      (* "minutes" *) token env tok
    )
  | `Hours tok -> R.Case ("Hours",
      (* "hours" *) token env tok
    )
  | `Days tok -> R.Case ("Days",
      (* "days" *) token env tok
    )
  | `Weeks tok -> R.Case ("Weeks",
      (* "weeks" *) token env tok
    )
  | `Years tok -> R.Case ("Years",
      (* "years" *) token env tok
    )
  )

let map_state_mutability (env : env) (x : CST.state_mutability) =
  (match x with
  | `Pure tok -> R.Case ("Pure",
      (* "pure" *) token env tok
    )
  | `View tok -> R.Case ("View",
      (* "view" *) token env tok
    )
  | `Paya tok -> R.Case ("Paya",
      (* "payable" *) token env tok
    )
  )

let map_pat_f2662db (env : env) (tok : CST.pat_f2662db) =
  (* pattern fixed([0-9]+)x([0-9]+) *) token env tok

let map_storage_location (env : env) (x : CST.storage_location) =
  (match x with
  | `Memory tok -> R.Case ("Memory",
      (* "memory" *) token env tok
    )
  | `Stor tok -> R.Case ("Stor",
      (* "storage" *) token env tok
    )
  | `Call tok -> R.Case ("Call",
      (* "calldata" *) token env tok
    )
  )

let map_yul_boolean (env : env) (x : CST.yul_boolean) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_string_immediate_elt_inside_double_quote (env : env) (tok : CST.string_immediate_elt_inside_double_quote) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_visibility (env : env) (x : CST.visibility) =
  (match x with
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Inte tok -> R.Case ("Inte",
      (* "internal" *) token env tok
    )
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  | `Exte tok -> R.Case ("Exte",
      (* "external" *) token env tok
    )
  )

let map_anon_choice_PLUSPLUS_e498e28 (env : env) (x : CST.anon_choice_PLUSPLUS_e498e28) =
  (match x with
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok

let map_hex_digit (env : env) (tok : CST.hex_digit) =
  (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) token env tok

let map_solidity_version_comparison_operator (env : env) (x : CST.solidity_version_comparison_operator) =
  (match x with
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  )

let map_unicode_string_literal (env : env) (xs : CST.unicode_string_literal) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = (* "unicode" *) token env v1 in
    let v2 =
      (match v2 with
      | `DQUOT_rep_double_quoted_unic_char_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_rep_double_quoted_unic_char_DQUOT",
          let v1 = (* "\"" *) token env v1 in
          let v2 =
            R.List (List.map (token env (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *)) v2)
          in
          let v3 = (* "\"" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `SQUOT_rep_single_quoted_unic_char_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_rep_single_quoted_unic_char_SQUOT",
          let v1 = (* "'" *) token env v1 in
          let v2 =
            R.List (List.map (token env (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *)) v2)
          in
          let v3 = (* "'" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    in
    R.Tuple [v1; v2]
  ) xs)

let map_ufixed (env : env) (x : CST.ufixed) =
  (match x with
  | `Ufixed tok -> R.Case ("Ufixed",
      (* "ufixed" *) token env tok
    )
  | `Pat_accdbe2 x -> R.Case ("Pat_accdbe2",
      map_pat_accdbe2 env x
    )
  )

let map_decimal_number (env : env) (x : CST.decimal_number) =
  (match x with
  | `Pat_585ba4d x -> R.Case ("Pat_585ba4d",
      map_pat_585ba4d env x
    )
  | `Pat_ac20a0c x -> R.Case ("Pat_ac20a0c",
      map_pat_ac20a0c env x
    )
  )

let map_hex_number (env : env) (x : CST.hex_number) =
  map_pat_2b7bb84 env x

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_pragma_value (env : env) (x : CST.pragma_value) =
  map_pat_c5921c8 env x

let map_fixed (env : env) (x : CST.fixed) =
  (match x with
  | `Fixed tok -> R.Case ("Fixed",
      (* "fixed" *) token env tok
    )
  | `Pat_f2662db x -> R.Case ("Pat_f2662db",
      map_pat_f2662db env x
    )
  )

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Str_imme_elt_inside_double_quote tok -> R.Case ("Str_imme_elt_inside_double_quote",
              (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Str_imme_elt_inside_quote tok -> R.Case ("Str_imme_elt_inside_quote",
              (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_rep_opt___hex_digit_c87bea1 (env : env) (xs : CST.anon_rep_opt___hex_digit_c87bea1) =
  R.List (List.map (fun (v1, v2) ->
    let v1 =
      (match v1 with
      | Some tok -> R.Option (Some (
          (* "_" *) token env tok
        ))
      | None -> R.Option None)
    in
    let v2 =
      (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) token env v2
    in
    R.Tuple [v1; v2]
  ) xs)

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Addr_opt_paya (v1, v2) -> R.Case ("Addr_opt_paya",
      let v1 = (* "address" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "payable" *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Var tok -> R.Case ("Var",
      (* "var" *) token env tok
    )
  | `Int x -> R.Case ("Int",
      map_int_ env x
    )
  | `Uint x -> R.Case ("Uint",
      map_uint env x
    )
  | `Bytes x -> R.Case ("Bytes",
      map_bytes_ env x
    )
  | `Fixed x -> R.Case ("Fixed",
      map_fixed env x
    )
  | `Ufixed x -> R.Case ("Ufixed",
      map_ufixed env x
    )
  )

let map_yul_string_literal (env : env) (x : CST.yul_string_literal) =
  map_string_ env x

let map_import_alias (env : env) ((v1, v2) : CST.import_alias) =
  let v1 = (* "as" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  R.Tuple [v1; v2]

let map_anon_choice_yul_id_bd8a33f (env : env) (x : CST.anon_choice_yul_id_bd8a33f) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_yul_path (env : env) ((v1, v2) : CST.yul_path) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_any_pragma_token (env : env) ((v1, v2) : CST.any_pragma_token) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 = map_pragma_value env v2 in
  R.Tuple [v1; v2]

let map_pragma_version_constraint (env : env) (x : CST.pragma_version_constraint) =
  (match x with
  | `Opt_soli_vers_comp_op_soli_vers (v1, v2) -> R.Case ("Opt_soli_vers_comp_op_soli_vers",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_solidity_version_comparison_operator env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Opt_soli_vers_comp_op_id (v1, v2) -> R.Case ("Opt_soli_vers_comp_op_id",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_solidity_version_comparison_operator env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  )

let map_identifier_path (env : env) ((v1, v2) : CST.identifier_path) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e (env : env) ((v1, v2, v3) : CST.anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_yul_hex_string_literal (env : env) ((v1, v2) : CST.yul_hex_string_literal) =
  let v1 = (* "hex" *) token env v1 in
  let v2 =
    (match v2 with
    | `DQUOT_opt_hex_digit_rep_opt___hex_digit_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_opt_hex_digit_rep_opt___hex_digit_DQUOT",
        let v1 = (* "\"" *) token env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 =
                (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) token env v1
              in
              let v2 = map_anon_rep_opt___hex_digit_c87bea1 env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v3 = (* "\"" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `SQUOT_opt_hex_digit_rep_opt___hex_digit_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_opt_hex_digit_rep_opt___hex_digit_SQUOT",
        let v1 = (* "'" *) token env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 =
                (* pattern ([a-fA-F0-9][a-fA-F0-9]) *) token env v1
              in
              let v2 = map_anon_rep_opt___hex_digit_c87bea1 env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v3 = (* "'" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  R.Tuple [v1; v2]

let map_user_defined_type_definition (env : env) ((v1, v2, v3, v4, v5) : CST.user_defined_type_definition) =
  let v1 = (* "type" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 = (* "is" *) token env v3 in
  let v4 = map_primitive_type env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_assembly_flags (env : env) ((v1, v2, v3) : CST.assembly_flags) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_yul_string_literal env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_yul_string_literal env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = (* "from" *) token env v1 in
  let v2 = map_yul_string_literal env v2 in
  R.Tuple [v1; v2]

let map_string_literal (env : env) (xs : CST.string_literal) =
  R.List (List.map (map_yul_string_literal env) xs)

let map_import_declaration (env : env) ((v1, v2) : CST.import_declaration) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_import_alias env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_source_import (env : env) ((v1, v2) : CST.source_import) =
  let v1 = map_yul_string_literal env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_import_alias env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_yul_id_bd8a33f env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_yul_id_bd8a33f env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_solidity_pragma_token (env : env) ((v1, v2) : CST.solidity_pragma_token) =
  let v1 = (* "solidity" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_pragma_version_constraint env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `BARBAR tok -> R.Case ("BARBAR",
                (* "||" *) token env tok
              )
            | `DASH tok -> R.Case ("DASH",
                (* "-" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_user_defined_type (env : env) (x : CST.user_defined_type) =
  map_identifier_path env x

let map_yul_literal (env : env) (x : CST.yul_literal) =
  (match x with
  | `Yul_deci_num tok -> R.Case ("Yul_deci_num",
      (* pattern 0|([1-9][0-9]*\
  ) *) token env tok
    )
  | `Yul_str_lit x -> R.Case ("Yul_str_lit",
      map_yul_string_literal env x
    )
  | `Yul_hex_num tok -> R.Case ("Yul_hex_num",
      (* pattern 0x[0-9A-Fa-f]* *) token env tok
    )
  | `Yul_bool x -> R.Case ("Yul_bool",
      map_yul_boolean env x
    )
  | `Yul_hex_str_lit x -> R.Case ("Yul_hex_str_lit",
      map_yul_hex_string_literal env x
    )
  )

let map_hex_string_literal (env : env) (xs : CST.hex_string_literal) =
  R.List (List.map (map_yul_hex_string_literal env) xs)

let map_enum_declaration (env : env) ((v1, v2, v3) : CST.enum_declaration) =
  let v1 = (* "enum" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 = map_enum_body env v3 in
  R.Tuple [v1; v2; v3]

let map_using_alias (env : env) ((v1, v2) : CST.using_alias) =
  let v1 = map_user_defined_type env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = map_user_definable_operator env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_mapping_key (env : env) (x : CST.mapping_key) =
  (match x with
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  | `User_defi_type x -> R.Case ("User_defi_type",
      map_user_defined_type env x
    )
  )

let map_override_specifier (env : env) ((v1, v2) : CST.override_specifier) =
  let v1 = (* "override" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_user_defined_type env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_user_defined_type env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        let v5 = (* ")" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let rec map_yul_expression (env : env) (x : CST.yul_expression) =
  (match x with
  | `Yul_path x -> R.Case ("Yul_path",
      map_yul_path env x
    )
  | `Yul_func_call x -> R.Case ("Yul_func_call",
      map_yul_function_call env x
    )
  | `Yul_lit x -> R.Case ("Yul_lit",
      map_yul_literal env x
    )
  )

and map_yul_function_call (env : env) (x : CST.yul_function_call) =
  (match x with
  | `Choice_yul_id_LPAR_opt_yul_exp_rep_COMMA_yul_exp_opt_COMMA_RPAR (v1, v2, v3, v4) -> R.Case ("Choice_yul_id_LPAR_opt_yul_exp_rep_COMMA_yul_exp_opt_COMMA_RPAR",
      let v1 =
        (match v1 with
        | `Yul_id tok -> R.Case ("Yul_id",
            (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
          )
        | `Yul_evm_buil x -> R.Case ("Yul_evm_buil",
            map_yul_evm_builtin env x
          )
        )
      in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_yul_expression env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_yul_expression env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Yul_evm_buil x -> R.Case ("Yul_evm_buil",
      map_yul_evm_builtin env x
    )
  )

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Num_lit (v1, v2) -> R.Case ("Num_lit",
      let v1 =
        (match v1 with
        | `Deci_num x -> R.Case ("Deci_num",
            map_decimal_number env x
          )
        | `Hex_num x -> R.Case ("Hex_num",
            map_hex_number env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_number_unit env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Hex_str_lit x -> R.Case ("Hex_str_lit",
      map_hex_string_literal env x
    )
  | `Unic_str_lit x -> R.Case ("Unic_str_lit",
      map_unicode_string_literal env x
    )
  )

let map_import_clause (env : env) (x : CST.import_clause) =
  (match x with
  | `Single_import (v1, v2) -> R.Case ("Single_import",
      let v1 =
        (match v1 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `Id tok -> R.Case ("Id",
            (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_import_alias env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Mult_import (v1, v2, v3) -> R.Case ("Mult_import",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_import_declaration env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_import_declaration env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_yul_variable_declaration (env : env) (x : CST.yul_variable_declaration) =
  (match x with
  | `Let_yul_id_opt_COLONEQ_yul_exp (v1, v2, v3) -> R.Case ("Let_yul_id_opt_COLONEQ_yul_exp",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":=" *) token env v1 in
            let v2 = map_yul_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Let_choice_yul_id_rep_COMMA_yul_id_opt_COMMA_opt_COLONEQ_yul_func_call (v1, v2, v3) -> R.Case ("Let_choice_yul_id_rep_COMMA_yul_id_opt_COMMA_opt_COLONEQ_yul_func_call",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (match v2 with
        | `Yul_id_rep_COMMA_yul_id_opt_COMMA x -> R.Case ("Yul_id_rep_COMMA_yul_id_opt_COMMA",
            map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e env x
          )
        | `LPAR_yul_id_rep_COMMA_yul_id_opt_COMMA_RPAR (v1, v2, v3, v4, v5) -> R.Case ("LPAR_yul_id_rep_COMMA_yul_id_opt_COMMA_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
            in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
                in
                R.Tuple [v1; v2]
              ) v3)
            in
            let v4 =
              (match v4 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            let v5 = (* ")" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":=" *) token env v1 in
            let v2 = map_yul_function_call env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_yul_assignment (env : env) (x : CST.yul_assignment) =
  (match x with
  | `Yul_path_yul_assign_op_yul_exp (v1, v2, v3) -> R.Case ("Yul_path_yul_assign_op_yul_exp",
      let v1 = map_yul_path env v1 in
      let v2 = map_yul_assignment_operator env v2 in
      let v3 = map_yul_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Yul_path_rep_COMMA_yul_path_opt_COMMA_opt_yul_assign_op_yul_func_call (v1, v2, v3, v4) -> R.Case ("Yul_path_rep_COMMA_yul_path_opt_COMMA_opt_yul_assign_op_yul_func_call",
      let v1 = map_yul_path env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_yul_path env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_yul_assignment_operator env v1 in
            let v2 = map_yul_function_call env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let rec map_anon_choice_exp_97f816a (env : env) (x : CST.anon_choice_exp_97f816a) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
    )
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STARSTAR_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_call_argument (env : env) (x : CST.call_argument) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `LCURL_opt_call_struct_arg_rep_COMMA_call_struct_arg_opt_COMMA_RCURL (v1, v2, v3) -> R.Case ("LCURL_opt_call_struct_arg_rep_COMMA_call_struct_arg_opt_COMMA_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_call_struct_argument env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_call_struct_argument env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_call_arguments (env : env) ((v1, v2, v3) : CST.call_arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_call_argument env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_call_argument env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_call_arguments env v2 in
  R.Tuple [v1; v2]

and map_call_struct_argument (env : env) ((v1, v2, v3) : CST.call_struct_argument) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_bin_exp x -> R.Case ("Choice_bin_exp",
      (match x with
      | `Bin_exp x -> R.Case ("Bin_exp",
          map_binary_expression env x
        )
      | `Un_exp x -> R.Case ("Un_exp",
          map_unary_expression env x
        )
      | `Update_exp x -> R.Case ("Update_exp",
          map_update_expression env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      | `Paya_conv_exp x -> R.Case ("Paya_conv_exp",
          map_payable_conversion_expression env x
        )
      | `Meta_type_exp x -> R.Case ("Meta_type_exp",
          map_meta_type_expression env x
        )
      | `Prim_exp x -> R.Case ("Prim_exp",
          map_primary_expression env x
        )
      | `Struct_exp x -> R.Case ("Struct_exp",
          map_struct_expression env x
        )
      | `Tern_exp x -> R.Case ("Tern_exp",
          map_ternary_expression env x
        )
      | `Type_cast_exp x -> R.Case ("Type_cast_exp",
          map_type_cast_expression env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Member_ellips_exp (v1, v2, v3) -> R.Case ("Member_ellips_exp",
      let v1 = map_anon_choice_exp_97f816a env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_meta_type_expression (env : env) ((v1, v2, v3, v4) : CST.meta_type_expression) =
  let v1 = (* "type" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_name env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_nameless_parameter (env : env) ((v1, v2) : CST.nameless_parameter) =
  let v1 = map_type_name env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_storage_location env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Type_name_opt_stor_loca_opt_id (v1, v2, v3) -> R.Case ("Type_name_opt_stor_loca_opt_id",
      let v1 = map_type_name env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_storage_location env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_parameter env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_payable_conversion_expression (env : env) ((v1, v2) : CST.payable_conversion_expression) =
  let v1 = (* "payable" *) token env v1 in
  let v2 = map_call_arguments env v2 in
  R.Tuple [v1; v2]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Paren_exp (v1, v2, v3) -> R.Case ("Paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Member_exp (v1, v2, v3) -> R.Case ("Member_exp",
      let v1 = map_anon_choice_exp_97f816a env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `Array_access (v1, v2, v3, v4) -> R.Case ("Array_access",
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Slice_access (v1, v2, v3, v4, v5, v6) -> R.Case ("Slice_access",
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ":" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "]" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Augm_assign_exp (v1, v2, v3) -> R.Case ("Augm_assign_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `User_defi_type x -> R.Case ("User_defi_type",
      map_user_defined_type env x
    )
  | `Tuple_exp (v1, v2, v3) -> R.Case ("Tuple_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_expression env x
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_expression env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Inline_array_exp (v1, v2, v3) -> R.Case ("Inline_array_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_expression env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `New_exp (v1, v2, v3) -> R.Case ("New_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_type_name env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_call_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_return_parameters (env : env) ((v1, v2, v3, v4, v5, v6) : CST.return_parameters) =
  let v1 = (* "returns" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_nameless_parameter env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_nameless_parameter env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_struct_expression (env : env) ((v1, v2, v3, v4) : CST.struct_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_struct_field_assignment env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_struct_field_assignment env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_struct_field_assignment (env : env) ((v1, v2, v3) : CST.struct_field_assignment) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_ternary_expression (env : env) ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_cast_expression (env : env) ((v1, v2) : CST.type_cast_expression) =
  let v1 = map_primitive_type env v1 in
  let v2 = map_call_arguments env v2 in
  R.Tuple [v1; v2]

and map_type_name (env : env) (x : CST.type_name) =
  (match x with
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  | `User_defi_type x -> R.Case ("User_defi_type",
      map_user_defined_type env x
    )
  | `Mapp (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Mapp",
      let v1 = (* "mapping" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_mapping_key env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* "=>" *) token env v5 in
      let v6 = map_type_name env v6 in
      let v7 =
        (match v7 with
        | Some tok -> R.Option (Some (
            (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
          ))
        | None -> R.Option None)
      in
      let v8 = (* ")" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Array_type (v1, v2, v3, v4) -> R.Case ("Array_type",
      let v1 = map_type_name env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_type (v1, v2, v3, v4) -> R.Case ("Func_type",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        R.List (List.map (fun x ->
          (match x with
          | `Visi x -> R.Case ("Visi",
              map_visibility env x
            )
          | `State_muta x -> R.Case ("State_muta",
              map_state_mutability env x
            )
          )
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_return_parameters env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `DASH_exp (v1, v2) -> R.Case ("DASH_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Delete_exp (v1, v2) -> R.Case ("Delete_exp",
      let v1 = (* "delete" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `BANG_exp (v1, v2) -> R.Case ("BANG_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `TILDE_exp (v1, v2) -> R.Case ("TILDE_exp",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_choice_PLUSPLUS (v1, v2) -> R.Case ("Exp_choice_PLUSPLUS",
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_PLUSPLUS_exp (v1, v2) -> R.Case ("Choice_PLUSPLUS_exp",
      let v1 = map_anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

let rec map_yul_block (env : env) ((v1, v2, v3) : CST.yul_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_yul_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_yul_statement (env : env) (x : CST.yul_statement) =
  (match x with
  | `Yul_blk x -> R.Case ("Yul_blk",
      map_yul_block env x
    )
  | `Yul_var_decl x -> R.Case ("Yul_var_decl",
      map_yul_variable_declaration env x
    )
  | `Yul_assign x -> R.Case ("Yul_assign",
      map_yul_assignment env x
    )
  | `Yul_func_call x -> R.Case ("Yul_func_call",
      map_yul_function_call env x
    )
  | `Yul_if_stmt (v1, v2, v3) -> R.Case ("Yul_if_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_yul_expression env v2 in
      let v3 = map_yul_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Yul_for_stmt (v1, v2, v3, v4, v5) -> R.Case ("Yul_for_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_yul_block env v2 in
      let v3 = map_yul_expression env v3 in
      let v4 = map_yul_block env v4 in
      let v5 = map_yul_block env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Yul_switch_stmt (v1, v2, v3) -> R.Case ("Yul_switch_stmt",
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_yul_expression env v2 in
      let v3 =
        (match v3 with
        | `Defa_yul_blk (v1, v2) -> R.Case ("Defa_yul_blk",
            let v1 = (* "default" *) token env v1 in
            let v2 = map_yul_block env v2 in
            R.Tuple [v1; v2]
          )
        | `Rep1_case_yul_lit_yul_blk_opt_defa_yul_blk (v1, v2) -> R.Case ("Rep1_case_yul_lit_yul_blk_opt_defa_yul_blk",
            let v1 =
              R.List (List.map (fun (v1, v2, v3) ->
                let v1 = (* "case" *) token env v1 in
                let v2 = map_yul_literal env v2 in
                let v3 = map_yul_block env v3 in
                R.Tuple [v1; v2; v3]
              ) v1)
            in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* "default" *) token env v1 in
                  let v2 = map_yul_block env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Yul_leave tok -> R.Case ("Yul_leave",
      (* "leave" *) token env tok
    )
  | `Yul_brk tok -> R.Case ("Yul_brk",
      (* "break" *) token env tok
    )
  | `Yul_cont tok -> R.Case ("Yul_cont",
      (* "continue" *) token env tok
    )
  | `Yul_func_defi (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Yul_func_defi",
      let v1 = (* "function" *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_yul_id_rep_COMMA_yul_id_opt_COMMA_477546e env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* ")" *) token env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 = (* "->" *) token env v1 in
            let v2 =
              (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
            in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
                in
                R.Tuple [v1; v2]
              ) v3)
            in
            let v4 =
              (match v4 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      let v7 = map_yul_block env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Yul_label (v1, v2) -> R.Case ("Yul_label",
      let v1 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
      in
      let v2 = (* ":" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Yul_lit x -> R.Case ("Yul_lit",
      map_yul_literal env x
    )
  )

let map_variable_declaration (env : env) ((v1, v2, v3) : CST.variable_declaration) =
  let v1 = map_type_name env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_storage_location env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
  in
  R.Tuple [v1; v2; v3]

let map_layout_specifier (env : env) ((v1, v2, v3) : CST.layout_specifier) =
  let v1 = (* "layout" *) token env v1 in
  let v2 = (* "at" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

let map_error_parameter (env : env) ((v1, v2) : CST.error_parameter) =
  let v1 = map_type_name env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_return_type_definition (env : env) ((v1, v2) : CST.return_type_definition) =
  let v1 = (* "returns" *) token env v1 in
  let v2 = map_parameter_list env v2 in
  R.Tuple [v1; v2]

let map_expression_statement (env : env) (x : CST.expression_statement) =
  (match x with
  | `Exp_semi (v1, v2) -> R.Case ("Exp_semi",
      let v1 = map_expression env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips_SEMI (v1, v2) -> R.Case ("Ellips_SEMI",
      let v1 = (* "..." *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_struct_member (env : env) (x : CST.struct_member) =
  (match x with
  | `Type_name_id_semi (v1, v2, v3) -> R.Case ("Type_name_id_semi",
      let v1 = map_type_name env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_using_directive (env : env) ((v1, v2, v3, v4, v5, v6) : CST.using_directive) =
  let v1 = (* "using" *) token env v1 in
  let v2 =
    (match v2 with
    | `User_defi_type x -> R.Case ("User_defi_type",
        map_user_defined_type env x
      )
    | `LCURL_using_alias_rep_COMMA_using_alias_opt_COMMA_RCURL (v1, v2, v3, v4, v5) -> R.Case ("LCURL_using_alias_rep_COMMA_using_alias_opt_COMMA_RCURL",
        let v1 = (* "{" *) token env v1 in
        let v2 = map_using_alias env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_using_alias env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        let v5 = (* "}" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    )
  in
  let v3 = (* "for" *) token env v3 in
  let v4 =
    (match v4 with
    | `Any_source_type tok -> R.Case ("Any_source_type",
        (* "*" *) token env tok
      )
    | `Type_name x -> R.Case ("Type_name",
        map_type_name env x
      )
    )
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "global" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_modifier_invocation (env : env) ((v1, v2) : CST.modifier_invocation) =
  let v1 = map_user_defined_type env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_call_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_event_parameter (env : env) (x : CST.event_parameter) =
  (match x with
  | `Type_name_opt_inde_opt_id (v1, v2, v3) -> R.Case ("Type_name_opt_inde_opt_id",
      let v1 = map_type_name env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "indexed" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_state_variable_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.state_variable_declaration) =
  let v1 = map_type_name env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Visi x -> R.Case ("Visi",
          map_visibility env x
        )
      | `Cst tok -> R.Case ("Cst",
          (* "constant" *) token env tok
        )
      | `Over_spec x -> R.Case ("Over_spec",
          map_override_specifier env x
        )
      | `Immu tok -> R.Case ("Immu",
          (* "immutable" *) token env tok
        )
      | `State_loca x -> R.Case ("State_loca",
          map_state_location env x
        )
      )
    ) v2)
  in
  let v3 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_directive (env : env) (x : CST.directive) =
  (match x with
  | `Pragma_dire (v1, v2, v3) -> R.Case ("Pragma_dire",
      let v1 = (* "pragma" *) token env v1 in
      let v2 =
        (match v2 with
        | `Soli_pragma_tok x -> R.Case ("Soli_pragma_tok",
            map_solidity_pragma_token env x
          )
        | `Any_pragma_tok x -> R.Case ("Any_pragma_tok",
            map_any_pragma_token env x
          )
        )
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Import_dire (v1, v2, v3) -> R.Case ("Import_dire",
      let v1 = (* "import" *) token env v1 in
      let v2 =
        (match v2 with
        | `Source_import x -> R.Case ("Source_import",
            map_source_import env x
          )
        | `Import_clause_from_clause (v1, v2) -> R.Case ("Import_clause_from_clause",
            let v1 = map_import_clause env v1 in
            let v2 = map_from_clause env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_variable_declaration_tuple (env : env) (x : CST.variable_declaration_tuple) =
  (match x with
  | `LPAR_opt_opt_var_decl_rep_COMMA_opt_var_decl_opt_COMMA_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_opt_var_decl_rep_COMMA_opt_var_decl_opt_COMMA_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_variable_declaration env x
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_variable_declaration env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Var_LPAR_opt_opt_id_rep_COMMA_opt_id_opt_COMMA_RPAR (v1, v2, v3, v4) -> R.Case ("Var_LPAR_opt_opt_id_rep_COMMA_opt_id_opt_COMMA_RPAR",
      let v1 = (* "var" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some tok -> R.Option (Some (
                      (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_error_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.error_declaration) =
  let v1 = (* "error" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_error_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_error_parameter env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_struct_body (env : env) ((v1, v2, v3) : CST.struct_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_struct_member env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_inheritance_specifier (env : env) (x : CST.inheritance_specifier) =
  (match x with
  | `User_defi_type_opt_call_args x -> R.Case ("User_defi_type_opt_call_args",
      map_modifier_invocation env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_event_parameter_list (env : env) ((v1, v2, v3) : CST.event_parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_event_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_event_parameter env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_variable_declaration_statement (env : env) ((v1, v2) : CST.variable_declaration_statement) =
  let v1 =
    (match v1 with
    | `Var_decl_opt_EQ_exp (v1, v2) -> R.Case ("Var_decl_opt_EQ_exp",
        let v1 = map_variable_declaration env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "=" *) token env v1 in
              let v2 = map_expression env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Var_decl_tuple_EQ_exp (v1, v2, v3) -> R.Case ("Var_decl_tuple_EQ_exp",
        let v1 = map_variable_declaration_tuple env v1 in
        let v2 = (* "=" *) token env v2 in
        let v3 = map_expression env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_struct_declaration (env : env) ((v1, v2, v3) : CST.struct_declaration) =
  let v1 = (* "struct" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 = map_struct_body env v3 in
  R.Tuple [v1; v2; v3]

let map_class_heritage (env : env) ((v1, v2, v3, v4) : CST.class_heritage) =
  let v1 = (* "is" *) token env v1 in
  let v2 = map_inheritance_specifier env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_inheritance_specifier env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_event_definition (env : env) ((v1, v2, v3, v4, v5) : CST.event_definition) =
  let v1 = (* "event" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 = map_event_parameter_list env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "anonymous" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_block_statement (env : env) ((v1, v2, v3, v4) : CST.block_statement) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "unchecked" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "{" *) token env v2 in
  let v3 = R.List (List.map (map_statement env) v3) in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = map_parameter_list env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_block_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_for_statement (env : env) (x : CST.for_statement) =
  (match x with
  | `For_LPAR_choice_var_decl_stmt_choice_exp_stmt_opt_exp_RPAR_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("For_LPAR_choice_var_decl_stmt_choice_exp_stmt_opt_exp_RPAR_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Var_decl_stmt x -> R.Case ("Var_decl_stmt",
            map_variable_declaration_statement env x
          )
        | `Exp_stmt x -> R.Case ("Exp_stmt",
            map_expression_statement env x
          )
        | `Semi tok -> R.Case ("Semi",
            (* ";" *) token env tok
          )
        )
      in
      let v4 =
        (match v4 with
        | `Exp_stmt x -> R.Case ("Exp_stmt",
            map_expression_statement env x
          )
        | `Semi tok -> R.Case ("Semi",
            (* ";" *) token env tok
          )
        )
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_statement env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `For_LPAR_ellips_RPAR_stmt (v1, v2, v3, v4, v5) -> R.Case ("For_LPAR_ellips_RPAR_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Blk_stmt x -> R.Case ("Blk_stmt",
      map_block_statement env x
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Var_decl_stmt x -> R.Case ("Var_decl_stmt",
      map_variable_declaration_statement env x
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `For_stmt x -> R.Case ("For_stmt",
      map_for_statement env x
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Do_while_stmt",
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = (* "(" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      let v7 = (* ";" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Cont_stmt (v1, v2) -> R.Case ("Cont_stmt",
      let v1 = (* "continue" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Brk_stmt (v1, v2) -> R.Case ("Brk_stmt",
      let v1 = (* "break" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Try_stmt (v1, v2, v3, v4, v5) -> R.Case ("Try_stmt",
      let v1 = (* "try" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_return_type_definition env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_block_statement env v4 in
      let v5 = R.List (List.map (map_catch_clause env) v5) in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Emit_stmt (v1, v2, v3, v4) -> R.Case ("Emit_stmt",
      let v1 = (* "emit" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_call_arguments env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Asse_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("Asse_stmt",
      let v1 = (* "assembly" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "\"evmasm\"" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_assembly_flags env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "{" *) token env v4 in
      let v5 = R.List (List.map (map_yul_statement env) v5) in
      let v6 = (* "}" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Revert_stmt (v1, v2, v3, v4) -> R.Case ("Revert_stmt",
      let v1 = (* "revert" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_call_arguments env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_function_body (env : env) ((v1, v2, v3) : CST.function_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_constructor_definition (env : env) ((v1, v2, v3, v4) : CST.constructor_definition) =
  let v1 = (* "constructor" *) token env v1 in
  let v2 = map_parameter_list env v2 in
  let v3 =
    R.List (List.map (fun x ->
      (match x with
      | `Modi_invo x -> R.Case ("Modi_invo",
          map_modifier_invocation env x
        )
      | `Paya tok -> R.Case ("Paya",
          (* "payable" *) token env tok
        )
      | `Choice_inte x -> R.Case ("Choice_inte",
          (match x with
          | `Inte tok -> R.Case ("Inte",
              (* "internal" *) token env tok
            )
          | `Public tok -> R.Case ("Public",
              (* "public" *) token env tok
            )
          )
        )
      )
    ) v3)
  in
  let v4 = map_function_body env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_semi_f2fe6be (env : env) (x : CST.anon_choice_semi_f2fe6be) =
  (match x with
  | `Semi tok -> R.Case ("Semi",
      (* ";" *) token env tok
    )
  | `Func_body x -> R.Case ("Func_body",
      map_function_body env x
    )
  )

let map_modifier_definition (env : env) ((v1, v2, v3, v4, v5) : CST.modifier_definition) =
  let v1 = (* "modifier" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun x ->
      (match x with
      | `Virt tok -> R.Case ("Virt",
          (* "virtual" *) token env tok
        )
      | `Over_spec x -> R.Case ("Over_spec",
          map_override_specifier env x
        )
      )
    ) v4)
  in
  let v5 = map_anon_choice_semi_f2fe6be env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_fallback_receive_definition (env : env) ((v1, v2, v3, v4, v5) : CST.fallback_receive_definition) =
  let v1 =
    (match v1 with
    | `Choice_fall v1 -> R.Case ("Choice_fall",
        (match v1 with
        | `Fall tok -> R.Case ("Fall",
            (* "fallback" *) token env tok
          )
        | `Rece tok -> R.Case ("Rece",
            (* "receive" *) token env tok
          )
        | `Func tok -> R.Case ("Func",
            (* "function" *) token env tok
          )
        )
      )
    | `Func tok -> R.Case ("Func",
        (* "function" *) token env tok
      )
    )
  in
  let v2 = map_parameter_list env v2 in
  let v3 =
    R.List (List.map (fun x ->
      (match x with
      | `Visi x -> R.Case ("Visi",
          map_visibility env x
        )
      | `Modi_invo x -> R.Case ("Modi_invo",
          map_modifier_invocation env x
        )
      | `State_muta x -> R.Case ("State_muta",
          map_state_mutability env x
        )
      | `Virt tok -> R.Case ("Virt",
          (* "virtual" *) token env tok
        )
      | `Over_spec x -> R.Case ("Over_spec",
          map_override_specifier env x
        )
      )
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_return_type_definition env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_anon_choice_semi_f2fe6be env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_function_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.function_definition) =
  let v1 = (* "function" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 = map_parameter_list env v3 in
  let v4 =
    R.List (List.map (fun x ->
      (match x with
      | `Modi_invo x -> R.Case ("Modi_invo",
          map_modifier_invocation env x
        )
      | `Visi x -> R.Case ("Visi",
          map_visibility env x
        )
      | `State_muta x -> R.Case ("State_muta",
          map_state_mutability env x
        )
      | `Virt tok -> R.Case ("Virt",
          (* "virtual" *) token env tok
        )
      | `Over_spec x -> R.Case ("Over_spec",
          map_override_specifier env x
        )
      )
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_return_type_definition env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_anon_choice_semi_f2fe6be env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_contract_member (env : env) (x : CST.contract_member) =
  (match x with
  | `Choice_func_defi x -> R.Case ("Choice_func_defi",
      (match x with
      | `Func_defi x -> R.Case ("Func_defi",
          map_function_definition env x
        )
      | `Modi_defi x -> R.Case ("Modi_defi",
          map_modifier_definition env x
        )
      | `Error_decl x -> R.Case ("Error_decl",
          map_error_declaration env x
        )
      | `State_var_decl x -> R.Case ("State_var_decl",
          map_state_variable_declaration env x
        )
      | `Struct_decl x -> R.Case ("Struct_decl",
          map_struct_declaration env x
        )
      | `Enum_decl x -> R.Case ("Enum_decl",
          map_enum_declaration env x
        )
      | `Event_defi x -> R.Case ("Event_defi",
          map_event_definition env x
        )
      | `Using_dire x -> R.Case ("Using_dire",
          map_using_directive env x
        )
      | `Cons_defi x -> R.Case ("Cons_defi",
          map_constructor_definition env x
        )
      | `Fall_rece_defi x -> R.Case ("Fall_rece_defi",
          map_fallback_receive_definition env x
        )
      | `User_defi_type_defi x -> R.Case ("User_defi_type_defi",
          map_user_defined_type_definition env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_contract_body (env : env) ((v1, v2, v3) : CST.contract_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_contract_member env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Cont_decl (v1, v2, v3, v4, v5) -> R.Case ("Cont_decl",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "abstract" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "contract" *) token env v2 in
      let v3 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
      in
      let v4 =
        R.List (List.map (fun x ->
          (match x with
          | `Class_heri x -> R.Case ("Class_heri",
              map_class_heritage env x
            )
          | `Layout_spec x -> R.Case ("Layout_spec",
              map_layout_specifier env x
            )
          )
        ) v4)
      in
      let v5 = map_contract_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Inte_decl (v1, v2, v3, v4) -> R.Case ("Inte_decl",
      let v1 = (* "interface" *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_class_heritage env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_contract_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Error_decl x -> R.Case ("Error_decl",
      map_error_declaration env x
    )
  | `Libr_decl (v1, v2, v3) -> R.Case ("Libr_decl",
      let v1 = (* "library" *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      let v3 = map_contract_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Struct_decl x -> R.Case ("Struct_decl",
      map_struct_declaration env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Cst_var_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Cst_var_decl",
      let v1 = map_type_name env v1 in
      let v2 = (* "constant" *) token env v2 in
      let v3 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `User_defi_type_defi x -> R.Case ("User_defi_type_defi",
      map_user_defined_type_definition env x
    )
  | `Event_defi x -> R.Case ("Event_defi",
      map_event_definition env x
    )
  | `Using_dire x -> R.Case ("Using_dire",
      map_using_directive env x
    )
  )

let map_source_unit (env : env) (x : CST.source_unit) =
  (match x with
  | `Dire x -> R.Case ("Dire",
      map_directive env x
    )
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  )

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Rep_source_unit v1 -> R.Case ("Rep_source_unit",
      R.List (List.map (map_source_unit env) v1)
    )
  | `Rep1_stmt xs -> R.Case ("Rep1_stmt",
      R.List (List.map (map_statement env) xs)
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Cons_defi x -> R.Case ("Cons_defi",
      map_constructor_definition env x
    )
  | `Modi_defi x -> R.Case ("Modi_defi",
      map_modifier_definition env x
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras

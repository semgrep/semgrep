type token =
  | VIEWBOUND of Parse_info.t
  | Unknown of Parse_info.t
  | USCORE of Parse_info.t
  | T_INTERPOLATED_STRING of (string * Parse_info.t)
  | T_INTERPOLATED_START of (string * Parse_info.t)
  | T_INTERPOLATED_END of Parse_info.t
  | T_DOLLAR_LBRACE of Parse_info.t
  | TILDE of Parse_info.t
  | SymbolLiteral of (Parse_info.t * (string * Parse_info.t))
  | StringLiteral of (string * Parse_info.t)
  | Space of Parse_info.t
  | SUPERTYPE of Parse_info.t
  | SUBTYPE of Parse_info.t
  | STAR of Parse_info.t
  | SEMI of Parse_info.t
  | RPAREN of Parse_info.t
  | RDots of Parse_info.t
  | RBRACKET of Parse_info.t
  | RBRACE of Parse_info.t
  | PLUS of Parse_info.t
  | PIPE of Parse_info.t
  | OP of (string * Parse_info.t)
  | Nl of Parse_info.t
  | NEWLINES of Parse_info.t
  | NEWLINE of Parse_info.t
  | MINUS of Parse_info.t
  | LPAREN of Parse_info.t
  | LDots of Parse_info.t
  | LBRACKET of Parse_info.t
  | LBRACE of Parse_info.t
  | LARROW of Parse_info.t
  | Kyield of Parse_info.t
  | Kwith of Parse_info.t
  | Kwhile of Parse_info.t
  | Kvar of Parse_info.t
  | Kval of Parse_info.t
  | Ktype of Parse_info.t
  | Ktry of Parse_info.t
  | Ktrait of Parse_info.t
  | Kthrow of Parse_info.t
  | Kthis of Parse_info.t
  | Ksuper of Parse_info.t
  | Ksealed of Parse_info.t
  | Kreturn of Parse_info.t
  | Kprotected of Parse_info.t
  | Kprivate of Parse_info.t
  | Kpackage of Parse_info.t
  | Koverride of Parse_info.t
  | Kobject of Parse_info.t
  | Knull of Parse_info.t
  | Knew of Parse_info.t
  | Kmatch of Parse_info.t
  | Klazy of Parse_info.t
  | Kimport of Parse_info.t
  | Kimplicit of Parse_info.t
  | Kif of Parse_info.t
  | KforSome of Parse_info.t
  | Kfor of Parse_info.t
  | Kfinally of Parse_info.t
  | Kfinal of Parse_info.t
  | Kextends of Parse_info.t
  | Kelse of Parse_info.t
  | Kdo of Parse_info.t
  | Kdef of Parse_info.t
  | Kclass of Parse_info.t
  | Kcatch of Parse_info.t
  | Kcase of Parse_info.t
  | Kabstract of Parse_info.t
  | IntegerLiteral of (int option * Parse_info.t)
  | ID_UPPER of (string * Parse_info.t)
  | ID_LOWER of (string * Parse_info.t)
  | ID_DOLLAR of (string * Parse_info.t)
  | ID_BACKQUOTED of (string * Parse_info.t)
  | HASH of Parse_info.t
  | FloatingPointLiteral of (float option * Parse_info.t)
  | Ellipsis of Parse_info.t
  | EQUALS of Parse_info.t
  | EOF of Parse_info.t
  | DOT of Parse_info.t
  | QUOTE of Parse_info.t
  | Comment of Parse_info.t
  | CharacterLiteral of (string * Parse_info.t)
  | COMMA of Parse_info.t
  | COLON of Parse_info.t
  | BooleanLiteral of (bool * Parse_info.t)
  | BANG of Parse_info.t
  | AT of Parse_info.t
  | ARROW of Parse_info.t
  | INDENT
  | DEDENT
[@@deriving show { with_path = false }]

type t = token [@@deriving show]

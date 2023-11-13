type token =
  | VIEWBOUND of Tok.t
  | Unknown of Tok.t
  | USCORE of Tok.t
  | T_INTERPOLATED_STRING of (string * Tok.t)
  | T_INTERPOLATED_START of (string * Tok.t)
  | T_INTERPOLATED_END of Tok.t
  | T_DOLLAR_LBRACE of Tok.t
  | TILDE of Tok.t
  | SymbolLiteral of (Tok.t * (string * Tok.t))
  | StringLiteral of (string * Tok.t)
  | Space of Tok.t
  | SUPERTYPE of Tok.t
  | SUBTYPE of Tok.t
  | STAR of Tok.t
  | SEMI of Tok.t
  | RPAREN of Tok.t
  | RDots of Tok.t
  | RBRACKET of Tok.t
  | RBRACE of Tok.t
  | PLUS of Tok.t
  | PIPE of Tok.t
  | OP of (string * Tok.t)
  | Nl of Tok.t
  | NEWLINES of Tok.t
  | NEWLINE of Tok.t
  | MINUS of Tok.t
  | LPAREN of Tok.t
  | LDots of Tok.t
  | LBRACKET of Tok.t
  | LBRACE of Tok.t
  | LARROW of Tok.t
  | Kyield of Tok.t
  | Kwith of Tok.t
  | Kwhile of Tok.t
  | Kvar of Tok.t
  | Kval of Tok.t
  | Ktype of Tok.t
  | Ktry of Tok.t
  | Ktrait of Tok.t
  | Kthrow of Tok.t
  | Kthis of Tok.t
  | Ksuper of Tok.t
  | Ksealed of Tok.t
  | Kreturn of Tok.t
  | Kprotected of Tok.t
  | Kprivate of Tok.t
  | Kpackage of Tok.t
  | Koverride of Tok.t
  | Kobject of Tok.t
  | Knull of Tok.t
  | Knew of Tok.t
  | Kmatch of Tok.t
  | Klazy of Tok.t
  | Kimport of Tok.t
  | Kimplicit of Tok.t
  | Kif of Tok.t
  | KforSome of Tok.t
  | Kfor of Tok.t
  | Kfinally of Tok.t
  | Kfinal of Tok.t
  | Kextends of Tok.t
  | Kelse of Tok.t
  | Kdo of Tok.t
  | Kdef of Tok.t
  | Kclass of Tok.t
  | Kcatch of Tok.t
  | Kcase of Tok.t
  | Kabstract of Tok.t
  | IntegerLiteral of Parsed_int.t
  | ID_UPPER of (string * Tok.t)
  | ID_LOWER of (string * Tok.t)
  | ID_DOLLAR of (string * Tok.t)
  | ID_BACKQUOTED of (string * Tok.t)
  | HASH of Tok.t
  | FloatingPointLiteral of (float option * Tok.t)
  | Ellipsis of Tok.t
  | EQUALS of Tok.t
  | EOF of Tok.t
  | DOT of Tok.t
  | QUOTE of Tok.t
  | Comment of Tok.t
  | CharacterLiteral of (string * Tok.t)
  | COMMA of Tok.t
  | COLON of Tok.t
  | BooleanLiteral of (bool * Tok.t)
  | BANG of Tok.t
  | AT of Tok.t
  | ARROW of Tok.t
  (* emitted only via lexing tricks *)
  | DEDENT of (* line *) int * (* width *) int
[@@deriving show { with_path = false }]

type t = token [@@deriving show]

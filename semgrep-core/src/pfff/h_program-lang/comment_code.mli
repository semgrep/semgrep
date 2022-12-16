
type 'tok hooks = {
  kind: 'tok -> Parse_info.token_kind;
  tokf: 'tok -> Parse_info.t;
}

val comment_before:
  'a hooks -> Parse_info.t -> 'a list -> Parse_info.t option

val comment_after:
  'a hooks -> Parse_info.t -> 'a list -> Parse_info.t option

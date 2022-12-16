type state =
  | Bol (* beginning of line *)
  | AfterCommand (* after command, before args *)
  | EndOfExpr (* end of expr / literal *)
  | AfterDef (* after a 'def' or '.' token *)
  | AfterLocal (* after a local variable *)

(* alternatives:
 *  - THIS FILE: use continuation and special epsilon trick in
 *    Lexer_ruby.token to do the switch
 *  - instead of passing continuations, have a state for each lexer rule
 *    and let the lexer caller do the switch (see parse_php.ml)
*)
type t = {
  mutable state : state;
  lexer_stack : (string (* to debug *) * cps_lexer) Stack.t;
}
and cps_lexer = t -> Lexing.lexbuf -> Parser_ruby.token

let beg_state t = t.state <- Bol
let mid_state t = t.state <- AfterCommand
let end_state t = t.state <- EndOfExpr
let def_state t = t.state <- AfterDef
let local_state t = t.state <- AfterLocal

let create entry =
  let stk = Stack.create () in
  Stack.push entry stk;
  {state = Bol;
   lexer_stack = stk;
  }

let string_of_state = function
  | Bol -> "Bol"
  | AfterCommand -> "AfterCommand"
  | EndOfExpr -> "EndOfExpr"
  | AfterDef -> "AfterDef"
  | AfterLocal -> "AfterLocal"

let string_of_t x =
  Common.spf "state = %s, stack = [%s]"
    (string_of_state x.state)
    (x.lexer_stack |> Stack.to_seq |> List.of_seq
     |> List.map fst |> String.concat ",")

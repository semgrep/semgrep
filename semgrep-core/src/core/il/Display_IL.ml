open IL

let string_of_lval x =
  (match x.base with
  | Var n -> str_of_name n
  | VarSpecial _ -> "<varspecial>"
  | Mem _ -> "<Mem>")
  ^
  match x.offset with
  | NoOffset -> ""
  | Dot n -> "." ^ str_of_name n
  | Index _ -> "[...]"

let string_of_exp e =
  match e.e with
  | Fetch l -> string_of_lval l
  | _ -> "<EXP>"

let short_string_of_node_kind nkind =
  match nkind with
  | Enter -> "<enter>"
  | Exit -> "<exit>"
  | TrueNode -> "<TRUE path>"
  | FalseNode -> "<FALSE path>"
  | Join -> "<join>"
  | NCond _ -> "cond(...)"
  | NGoto (_, l) -> "goto " ^ str_of_label l
  | NReturn _ -> "return ...;"
  | NThrow _ -> "throw ...;"
  | NOther Noop -> "<noop>"
  | NOther _ -> "<other>"
  | NInstr x -> (
      match x.i with
      | Assign (lval, _) -> string_of_lval lval ^ " = ..."
      | AssignAnon _ -> " ... = <lambda|class>"
      | Call (_lopt, exp, _) -> string_of_exp exp ^ "(...)"
      | CallSpecial _ -> "<special>"
      | FixmeInstr _ -> "<fix-me instr>")
  | NTodo _ -> "<to-do stmt>"

(* using internally graphviz dot and ghostview on X11 *)
let (display_cfg : cfg -> unit) =
 fun flow ->
  flow.graph
  |> Ograph_extended.print_ograph_mutable_generic
       ~s_of_node:(fun (_nodei, node) ->
         (short_string_of_node_kind node.n, None, None))

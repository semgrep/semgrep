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
  | Literal _ -> "<LIT>"
  | Operator _ -> "<OP>"
  | FixmeExp _ -> "<FIXME-EXP>"
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
  | NOther (Noop str) -> Common.spf "<noop: %s>" str
  | NOther _ -> "<other>"
  | NInstr x -> (
      match x.i with
      | Assign (lval, exp) -> string_of_lval lval ^ " = " ^ string_of_exp exp
      | AssignAnon _ -> " ... = <lambda|class>"
      | Call (_lopt, exp, _) -> string_of_exp exp ^ "(...)"
      | CallSpecial (lval_opt, (call_special, _tok), _args) ->
          let lval_str =
            match lval_opt with
            | None -> ""
            | Some lval -> Common.spf " %s =" (string_of_lval lval)
          in
          Common.spf "<special>%s %s(...)" lval_str
            (IL.show_call_special call_special)
      | FixmeInstr _ -> "<fix-me instr>")
  | NTodo _ -> "<to-do stmt>"
  | NFunc _ -> "<func>"
  | NClass _ -> "<class>"
  | NModule _ -> "<module>"

(* using internally graphviz dot and ghostview on X11 *)
let (display_cfg : cfg -> unit) =
 fun flow ->
  flow.graph
  |> Ograph_extended.print_ograph_mutable_generic
       ~s_of_node:(fun (_nodei, node) ->
         (short_string_of_node_kind node.n, None, None))

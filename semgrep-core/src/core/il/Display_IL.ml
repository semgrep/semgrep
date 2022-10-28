open IL

(* coupling: Dataflow_xyz.str_of_name *)
let string_of_name name = Common.spf "%s:%d" (fst name.ident) name.sid

let string_of_base base =
  match base with
  | Var x -> string_of_name x
  | VarSpecial _ -> "<VarSpecial>"
  | Mem _ -> "<Mem>"

let string_of_offset offset =
  match offset.o with
  | Dot a -> str_of_name a
  | Index _ -> "[...]"

let string_of_lval { base; rev_offset } =
  string_of_base base
  ^
  if rev_offset <> [] then
    "." ^ String.concat "." (List.rev_map string_of_offset rev_offset)
  else ""

let string_of_exp e =
  match e.e with
  | Fetch l -> string_of_lval l
  | Literal _ -> "<LIT>"
  | Operator _ -> "<OP>"
  | FixmeExp _ -> "<FIXME-EXP>"
  | Composite (_, _)
  | Record _
  | Cast (_, _) ->
      "<EXP>"

let string_of_argument arg =
  match arg with
  | Unnamed { e = Fetch lval; _ } -> string_of_lval lval
  | Unnamed _
  | Named _ ->
      "_"

let string_of_arguments args =
  Common.map string_of_argument args |> String.concat ","

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
  | NLambda params ->
      let params_strs = Common.map string_of_name params in
      "LAMBDA " ^ String.concat ", " params_strs
  | NOther (Noop str) -> Common.spf "<noop: %s>" str
  | NOther _ -> "<other>"
  | NInstr x -> (
      match x.i with
      | Assign (lval, exp) -> string_of_lval lval ^ " = " ^ string_of_exp exp
      | AssignAnon _ -> " ... = <lambda|class>"
      | Call (lval_opt, exp, args) ->
          let lval_str =
            match lval_opt with
            | None -> ""
            | Some lval -> string_of_lval lval ^ " = "
          in
          lval_str ^ string_of_exp exp ^ "(" ^ string_of_arguments args ^ ")"
      | CallSpecial (lval_opt, (call_special, _tok), args) ->
          let lval_str =
            match lval_opt with
            | None -> ""
            | Some lval -> Common.spf " %s =" (string_of_lval lval)
          in
          Common.spf "<special>%s = %s(%s)" lval_str
            (IL.show_call_special call_special)
            (string_of_arguments args)
      | FixmeInstr _ -> "<fix-me instr>")
  | NTodo _ -> "<to-do stmt>"

(* using internally graphviz dot and ghostview on X11 *)
let (display_cfg : cfg -> unit) =
 fun flow ->
  flow.graph
  |> Ograph_extended.print_ograph_mutable_generic
       ~s_of_node:(fun (_nodei, node) ->
         (short_string_of_node_kind node.n, None, None))

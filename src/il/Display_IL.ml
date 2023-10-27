open IL

let string_of_type (ty : IL.type_) =
  match ty.type_.t with
  | TyN (Id (id, _)) -> fst id
  | __else__ -> "<TYPE>"

let string_of_base base =
  match base with
  | Var x -> str_of_name x
  | VarSpecial _ -> "<VarSpecial>"
  | Mem _ -> "<Mem>"

let string_of_offset offset =
  match offset.o with
  | Dot a -> ident_str_of_name a
  | Index _ -> "[...]"

let string_of_lval { base; rev_offset } =
  string_of_base base
  ^
  if rev_offset <> [] then
    "." ^ String.concat "." (List.rev_map string_of_offset rev_offset)
  else ""

let string_of_literal (lit : AST_generic.literal) =
  match lit with
  | Bool (b, _) -> string_of_bool b
  | Int (Some i, _) -> string_of_int i
  | Int _ -> "<INT-LIT>"
  | Float (Some f, _) -> string_of_float f
  | Float _ -> "<FLOAT-LIT>"
  | Char (s, _) -> s
  | String (_, (s, _), _) -> s
  | Regexp _ -> "<REGEXP-LIT>"
  | Atom _ -> "<ATOM>"
  | Unit _ -> "<UNIT>"
  | Null _ -> "<NULL>"
  | Undefined _ -> "<UNDEFINEDL>"
  | Imag _ -> "<IMAG>"
  | Ratio _ -> "<RATIO>"

let rec string_of_exp_kind e =
  match e with
  | Fetch l -> string_of_lval l
  | Literal lit -> string_of_literal lit
  | Operator ((op, _), [ Unnamed e1; Unnamed e2 ]) ->
      Common.spf "(%s `%s` %s)" (string_of_exp e1) (G.show_operator op)
        (string_of_exp e2)
  | Operator ((op, _), _) -> Common.spf "<OP %s ...>" (G.show_operator op)
  | FixmeExp _ -> "<FIXME-EXP>"
  | Composite (_, _)
  | Record _
  | Cast (_, _) ->
      "<EXP>"

and string_of_exp e = string_of_exp_kind e.e

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
  | TrueNode e -> Common.spf "<TRUE %s>" (string_of_exp e)
  | FalseNode e -> Common.spf "<FALSE %s>" (string_of_exp e)
  | Join -> "<join>"
  | NCond _ -> "cond(...)"
  | NGoto (_, l) -> "goto " ^ str_of_label l
  | NReturn (_, e) -> Common.spf "return %s" (string_of_exp e)
  | NThrow _ -> "throw ...;"
  | NLambda params ->
      let params_strs = Common.map str_of_name params in
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
          "CALL " ^ lval_str ^ string_of_exp exp ^ "("
          ^ string_of_arguments args ^ ")"
      | New (lval, ty, _cons, args) ->
          Common.spf "%s = new %s(%s)" (string_of_lval lval) (string_of_type ty)
            (string_of_arguments args)
      | CallSpecial (lval_opt, (call_special, _tok), args) ->
          let lval_str =
            match lval_opt with
            | None -> ""
            | Some lval -> Common.spf " %s = " (string_of_lval lval)
          in
          Common.spf "<special>%s%s(%s)" lval_str
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

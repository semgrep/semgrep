open IL
module G = AST_generic

let string_of_type (ty : G.type_) =
  match ty.t with
  | TyN (Id (id, _)) -> fst id
  | __else__ -> "<TYPE>"

let string_of_base base =
  match base with
  | Var x -> str_of_name x
  | VarSpecial (This, _) -> "<this>"
  | VarSpecial (Self, _) -> "<self>"
  | VarSpecial _ -> "<VarSpecial>"
  | Mem _ -> "<Mem>"

let string_of_offset offset =
  match offset.o with
  | Dot a -> ident_str_of_name a
  | Index { e = Literal (G.Int parsed_int); _ } -> (
      match Parsed_int.to_int_opt parsed_int with
      | Some i -> Common.spf "[%d]" i
      | None -> "[...]")
  | Index { e = Literal (G.String (_, (str, _), _)); _ } ->
      Common.spf "[\"%s\"]" str
  | Index _ -> "[...]"

let string_of_offset_list offset =
  if offset <> [] then
    "." ^ String.concat "." (List_.map string_of_offset offset)
  else ""

let string_of_lval { base; rev_offset } =
  string_of_base base
  ^
  if rev_offset <> [] then
    "." ^ String.concat "." (List.rev_map string_of_offset rev_offset)
  else ""

let string_of_literal (lit : AST_generic.literal) =
  match lit with
  | Bool (b, _) -> string_of_bool b
  | Int pi -> (
      match Parsed_int.to_string_opt pi with
      | None -> "<INT-LIT>"
      | Some s -> s)
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
  | Composite (_, _) -> "<COMPOSITE>"
  | RecordOrDict _ -> "<RECORD-OR-DICT>"
  | Cast (_, _) -> "<CAST>"

and string_of_exp e = string_of_exp_kind e.e

let string_of_argument arg =
  match arg with
  | Unnamed e
  | Named (_, e) ->
      string_of_exp e

let string_of_arguments args =
  List_.map string_of_argument args |> String.concat ","

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
  | NOther (Noop str) -> Common.spf "<noop: %s>" str
  | NOther _ -> "<other>"
  | NInstr x -> (
      match x.i with
      | Assign (lval, exp) -> string_of_lval lval ^ " = " ^ string_of_exp exp
      | AssignAnon (lval, _) -> string_of_lval lval ^ " = " ^ "<lambda|class>"
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

let at_exit_mark node str = if node.at_exit then str ^ " @exit" else str

(* using internally graphviz dot and ghostview on X11 *)
let display_cfg (caps : < Cap.exec >) (flow : cfg) : unit =
  flow.graph
  |> Ograph_call_dot_gv.print_ograph_mutable_generic caps
       ~s_of_node:(fun (_nodei, node) ->
         (short_string_of_node_kind node.n |> at_exit_mark node, None, None))

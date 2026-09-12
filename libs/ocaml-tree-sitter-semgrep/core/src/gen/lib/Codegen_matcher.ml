(*
   Compile a grammar rule into OCaml code operating on the
   tree-sitter tree and matching it with grammar nodes.

   The whole difficulty is that tree-sitter's CST doesn't provide information
   on which path is taken within a rule. The code we generate recovers that
   info.
*)

open Printf
open Indent.Types

open CST_grammar
open Regexp

let trans = Codegen_util.translate_ident

(*
   Generate a matching function in an environment where '_parse_tail'
   is bound to a suitable function. The generated expression has the
   following type:

     nodes -> ((result * tail), nodes) option

   where 'nodes' represents the input sequence and 'tail' represents the
   result of '_parse_tail' when it succeeds. Upon success, the nodes returned
   are the remaining input.
*)
let rec make_seq_elt (body : rule_body) : seq_elt =
  match body with
  | Symbol name
  | Alias (name, _) ->
      Opaque [Line (sprintf "parse_node_%s" (trans name))]

  | Token { name; _ } ->
      Opaque [Line (sprintf "_parse_leaf_rule %S" name)]

  | Blank ->
      assert false

  | Repeat body ->
      Repeat (Repeat, make_seq body)

  | Repeat1 body ->
      Repeat (Repeat1, make_seq body)

  | Choice cases ->
      Choice (List.map (fun (name, arg) -> (name, make_seq arg)) cases)

  | Optional body ->
      Repeat (Optional, make_seq body)

  | Seq _ ->
      Seq (make_seq body)

and make_seq (body : rule_body) : seq_elt list =
  match body with
  | Seq bodies -> List.map make_seq_elt bodies
  | other -> [make_seq_elt other]

(* Optional simplifications

   This results in more natural code, with possible minor performance
   improvements. However it makes debugging a little harder, so it's
   a good idea to turn it off when debugging.

   The 'top' parameter is to preserve expressions of the form

     (fun x -> e x)

   Replacing it by just 'e' may be disallowed when it is part of
   a mutually-recursive definition. For example, the following is forbidden:

     let rec f = g
     and g = f

   Instead we would write:

     let rec f = fun x -> g x
     and g = fun x -> f x
*)
let rec simplify ?(top = false) (exp : exp) : exp =
  match exp with
  | Atom _
  | Var _ -> exp
  | App (Fun (var1, e), Var var2)
    when not top && var1 = var2 -> simplify e
  | App (e1, e2) -> App (simplify e1, simplify e2)
  | Fun (var, e) -> Fun (var, simplify e)
  | Def (var1, e1, Var var2)
    when var1 = var2 -> simplify e1
  | Def (var, e1, e2) -> Def (var, simplify e1, simplify e2)
  | Op (rep_kind, e) -> Op (rep_kind, simplify e)
  | Alt (e1, e2) -> Alt (simplify e1, simplify e2)
  | Flatten (e, (1 | 2), None) -> simplify e
  | Flatten (e, n, wrap) -> Flatten (simplify e, n, wrap)
  | Check_tail e -> Check_tail (simplify e)
  | Seq e -> Seq (simplify e)

(*** OCaml code formatting ***)

let paren code =
  [
    Group [
      Line "(";
      Block code;
      Line ")";
    ]
  ]

(*
   Format to OCaml syntax.
*)
let rec format (exp : exp) : Indent.t =
  match exp with
  | Atom code -> code
  | Var var -> [ Line var ]
  | App (f, arg) ->
      [
        Inline (format_paren f);
        Block (format_paren arg);
      ]
  | Fun (param, body) ->
      [
        Line (sprintf "fun %s ->" param);
        Block (format body);
      ]
  | Def (var, Fun (param, e1), e2) ->
      [
        Line (sprintf "let %s %s =" var param);
        Block (format e1);
        Line "in";
        Inline (format e2);
      ]
  | Def (var, e1, e2) ->
      [
        Line (sprintf "let %s =" var);
        Block (format e1);
        Line "in";
        Inline (format e2);
      ]
  | Op (repeat_kind, f1) ->
      let function_name =
        match repeat_kind with
        | Optional -> "Combine.parse_optional"
        | Repeat -> "Combine.parse_repeat"
        | Repeat1 -> "Combine.parse_repeat1"
      in
      [
        Line function_name;
        Block [
          Inline (format_paren f1);
          Line "check_tail";
        ]
      ]
  | Alt (e1, e2) ->
      [
        Line "match";
        Block (format e1);
        Line "with";
        Line "| Some _ as res -> res";
        Line "| None ->";
        Block [Block (format e2)];
      ]
  | Flatten (e, n, opt_wrap) ->
      let vars = List.init n (fun i -> sprintf "e%i" i) in
      let vars_short = List.init (n-1) (fun i -> sprintf "e%i" i) in
      let pat =
        let last = sprintf "e%i" (n-1) in
        List.fold_right
          (fun var acc -> sprintf "(%s, %s)" var acc)
          vars_short last
      in
      let exp = sprintf "(%s)" (String.concat ", " vars) in
      let wrap =
        match opt_wrap with
        | None -> (fun s -> s)
        | Some wrap -> wrap
      in
      [
        Line "match";
        Block (format e);
        Line "with";
        Line "| None -> None";
        Line (sprintf "| Some (%s, nodes) ->" pat);
        Block [Block [Line (sprintf "Some (%s, nodes)" (wrap exp))]];
      ]
  | Check_tail f ->
      [
        Line "Combine.check_seq";
        Block [
          Inline (format_paren f);
          Line "check_tail";
        ];
      ]
  | Seq f ->
      [
        Line "Combine.parse_seq";
        Block [
          Inline (format_paren f);
          Line "_parse_tail";
        ];
      ]

and format_paren e =
  match e with
  | Var _ -> format e
  | e -> paren (format e)

let generate_ocaml (rule_body : rule_body) : Indent.t =
  let seq_elt = make_seq_elt rule_body in
  Regexp.print_seq_elt seq_elt;
  let pseudo_code =
    Regexp.compile_seq_elt seq_elt None
    |> simplify ~top:true
  in
  format pseudo_code

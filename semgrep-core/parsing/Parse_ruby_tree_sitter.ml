(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module AST = Ast_ruby
module CST = Tree_sitter_ruby.CST
module PI = Parse_info
open Ast_ruby
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ruby parser using ocaml-tree-sitter-lang/ruby and converting
 * to pfff/lang_ruby/parsing/ast_ruby.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * pfff/lang_ruby/analyze/ruby_to_generic.ml
 *)
(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(* not very multicore friendly *)
let global_file = ref ""
let global_conv = ref (Hashtbl.create 0)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* mostly a copy of Parse_info.full_charpos_to_pos_large *)
let line_col_to_pos = fun file ->

    let chan = open_in file in
    let size = Common2.filesize file + 2 in

    let charpos   = ref 0 in
    let line  = ref 0 in
    let h = Hashtbl.create size in

    let full_charpos_to_pos_aux () =
      try
        while true do begin
          let s = (input_line chan) in
          incr line;

          (* '... +1 do'  cos input_line dont return the trailing \n *)
          for i = 0 to (String.length s - 1) + 1 do
            Hashtbl.add h (!line, i) (!charpos + i);
          done;
          charpos := !charpos + String.length s + 1;
        end done
     with End_of_file ->
       Hashtbl.add h (!line, 0) !charpos;
    in
    full_charpos_to_pos_aux ();
    close_in chan;
    h



let token2 (tok : Tree_sitter_run.Token.t) =
  let (loc, str) = tok in
  let h = !global_conv in
  let start = loc.Tree_sitter_run.Loc.start in
  (* Parse_info is 1-line based and 0-column based, like Emacs *)
  let line = start.Tree_sitter_run.Loc.row + 1 in
  let column = start.Tree_sitter_run.Loc.column in
  let charpos =
    try Hashtbl.find h (line, column)
    with Not_found -> -1
  in
  let file = !global_file in
  let tok_loc = { PI. str; charpos; line; column; file; } in
  { PI.token = PI.OriginTok tok_loc; transfo = PI.NoTransfo }

let str (tok : Tree_sitter_run.Token.t) =
  let (_, s) = tok in
  s, token2 tok

let list_to_maybe_tuple = function
 | [] -> raise Impossible
 | [x] -> x
 | xs -> Tuple (xs)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying ocaml-tree-sitter-lang/ruby/Boilerplate.ml *)


let blank () = ()

let false_ (x : CST.false_) : bool wrap =
  (match x with
  | `False_false tok -> false, token2 tok
  | `False_FALSE tok -> false, token2 tok
  )

let true_ (x : CST.true_) : bool wrap =
  (match x with
  | `True_true tok -> true, token2 tok
  | `True_TRUE tok -> true, token2 tok
  )

let nil (x : CST.nil) : tok =
  (match x with
  | `Nil_nil tok -> token2 tok
  | `Nil_NIL tok -> token2 tok
  )


let operator (x : CST.operator) =
  (match x with

  | `Op_DOTDOT tok -> Left Op_DOT2, (token2 tok)
  | `Op_BAR tok -> Left Op_BOR, (token2 tok)
  | `Op_HAT tok -> Left Op_XOR, (token2 tok)
  | `Op_AMP tok -> Left Op_BAND, (token2 tok)

  | `Op_LTEQGT tok -> Left Op_CMP, (token2 tok)
  | `Op_EQEQ tok -> Left Op_EQ, (token2 tok)
  | `Op_EQEQEQ tok -> Left Op_EQQ, (token2 tok)
  | `Op_EQTILDE tok -> Left Op_MATCH, (token2 tok)
  | `Op_GT tok -> Left Op_GT, (token2 tok)
  | `Op_GTEQ tok -> Left Op_GEQ, (token2 tok)
  | `Op_LT tok -> Left Op_LT, (token2 tok)
  | `Op_LTEQ tok -> Left Op_LEQ, (token2 tok)

  | `Op_PLUS tok -> Left Op_PLUS, (token2 tok)
  | `Op_DASH tok -> Left Op_MINUS, (token2 tok)
  | `Op_STAR tok -> Left Op_TIMES, (token2 tok)
  | `Op_SLASH tok -> Left Op_DIV, (token2 tok)
  | `Op_PERC tok -> Left Op_REM, (token2 tok)
  | `Op_BANGTILDE tok -> Left Op_NMATCH, (token2 tok)
  | `Op_STARSTAR tok -> Left Op_POW, (token2 tok)
  | `Op_LTLT tok -> Left Op_LSHIFT, (token2 tok)
  | `Op_GTGT tok -> Left Op_RSHIFT, (token2 tok)
  | `Op_LBRACKRBRACK tok -> Left Op_AREF, (token2 tok)
  | `Op_LBRACKRBRACKEQ tok -> Left Op_ASET, (token2 tok)

  | `Op_PLUSAT tok -> Right Op_UPlus, token2 tok
  | `Op_DASHAT tok -> Right Op_UMinus, token2 tok

  | `Op_TILDE tok -> Right Op_UTilde, token2 tok
  | `Op_BANG tok -> Right Op_UBang, token2 tok

  (* TODO *)
  | `Op_BQUOT tok ->
        pr2_gen tok;
        failwith "Op_BQUOT???"
  )


let terminator (x : CST.terminator) : unit =
  (match x with
  | `Term_line_brk _tok -> ()
  | `Term_SEMI _tok -> ()
  )

let variable (x : CST.variable) : AST.variable =
  (match x with
  (* TODO: move this to variable type *)
  | `Self tok ->
        (str tok, ID_Self)
  | `Super tok ->
        (str tok, ID_Super)
  | `Inst_var tok ->
        (str tok, ID_Instance)
  | `Class_var tok ->
        (str tok, ID_Class)
  | `Glob_var tok ->
        (str tok, ID_Global)
  | `Id tok ->
        (str tok, ID_Lowercase)
  | `Cst tok ->
        (str tok, ID_Uppercase)
  )

let do_ (x : CST.do_) : unit =
  (match x with
  | `Do_do _tok -> ()
  | `Do_term x -> terminator x
  )

let rec statements (x : CST.statements) : AST.stmts =
  (match x with
  | `Stmts_rep1_choice_stmt_term_opt_stmt (v1, v2) ->
      let v1 =
        v1 |> List.map (fun x ->
          (match x with
          | `Stmt_term (v1, v2) ->
              let v1 = statement v1 in
              let _v2 = terminator v2 in
              [v1]
          (* TODO? use EmptyStmt in generic AST? *)
          | `Empty_stmt _tok -> []
          )
        ) |> List.flatten
      in
      let v2 =
        (match v2 with
        | Some x -> [statement x]
        | None -> [])
      in
      v1 @ v2
  | `Stmts_stmt x -> [statement x]
  )

and statement (x : CST.statement) : AST.expr (* TODO AST.stmt at some point *)=
  (match x with
  | `Stmt_undef (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 = method_name v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let _v1 = token2 v1 in
          let v2 = method_name v2 in
          v2
        ) v3
      in
      D (Undef (v1, (v2::v3)))
  | `Stmt_alias (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 = method_name v2 in
      let v3 = method_name v3 in
      D (Alias (v1, v2, v3))
  | `Stmt_if_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (If (v2, v3, [v1], None))
  | `Stmt_unle_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (Unless (v2, v3, [v1], None))
  | `Stmt_while_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      let b = true (* ?? *) in
      S (While (v2, b, v3, [v1]))
  | `Stmt_until_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (Until (v2, true, v3, [v1]))

  | `Stmt_resc_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (ExnBlock ({
         body_exprs = [v1];
         rescue_exprs = [v2, [], None, [v3]];
         ensure_expr = None;
         else_expr = None;
        }))


  | `Stmt_begin_blk (v1, v2, v3, v4) ->
      let v1 = token2 v1 in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> statements x
        | None -> [])
      in
      let v4 = token2 v4 in
      D (BeginBlock (v1, (v2, v3, v4)))
  | `Stmt_end_blk (v1, v2, v3, v4) ->
      let v1 = token2 v1 in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> statements x
        | None -> [])
      in
      let v4 = token2 v4 in
      D (EndBlock (v1, (v2, v3, v4)))
  | `Stmt_exp x -> expression x
  )

and method_rest ((v1, v2, v3) : CST.method_rest) =
  let v1 = method_name v1 in
  let v2 =
    (match v2 with
    | `Params_opt_term (v1, v2) ->
        let v1 = parameters v1 |> G.unbracket in
        let _v2 =
          (match v2 with
          | Some x -> terminator x
          | None -> ())
        in
        v1
    | `Opt_bare_params_term (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> bare_parameters x
          | None -> [])
        in
        let _v2 = terminator v2 in
        v1
    )
  in
  let (xs, _tend) = body_statement v3 in
  v1, v2, xs

and parameters ((v1, v2, v3) : CST.parameters) : AST.formal_param list bracket=
  let lp = token2 v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = formal_parameter v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = token2 v1 in
            let v2 = formal_parameter v2 in
            v2
          ) v2
        in
        v1::v2
    | None -> [])
  in
  let rp = token2 v3 in
  (lp, v2, rp)

and bare_parameters ((v1, v2) : CST.bare_parameters) : AST.formal_param list =
  let v1 = simple_formal_parameter v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token2 v1 in
      let v2 = formal_parameter v2 in
      v2
    ) v2
  in
  v1::v2

and block_parameters ((v1, v2, v3, v4, v5) : CST.block_parameters) :
  AST.formal_param list bracket =
  let pipe1 = token2 v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = formal_parameter v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = token2 v1 in
            let v2 = formal_parameter v2 in
            v2
          ) v2
        in
        v1::v2
    | None -> [])
  in
  let _v3 =
    (match v3 with
    | Some _tok -> ()
    | None -> ())
  in
  let _v4SEMICOLONPARAMS =
    (match v4 with
    | Some (v1, v2, v3) ->
        let _v1 = token2 v1 in
        let v2 = str v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let _v1 = token2 v1 in
            let v2 = str v2 in
            v2
          ) v3
        in
         v2::v3
    | None -> [])
  in
  let pipe2 = token2 v5 in
  (pipe1, v2, pipe2)

and formal_parameter (x : CST.formal_parameter) : AST.formal_param =
  (match x with
  | `Form_param_simple_form_param x ->
      simple_formal_parameter x
  | `Form_param_params x ->
        let (lp, xs, rp) = parameters x in
        Formal_tuple ((lp, xs, rp))
  )

and simple_formal_parameter (x : CST.simple_formal_parameter) : AST.formal_param =
  (match x with
  | `Simple_form_param_id tok ->
        let id = str tok in
        Formal_id ((id))
  | `Simple_form_param_splat_param (v1, v2) ->
      let v1 = token2 v1 in
        (match v2 with
        | Some tok -> let id = str tok in
           Formal_star (v1, id)
        | None ->
           Formal_rest v1
        )
  | `Simple_form_param_hash_splat_param (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some tok -> Some (str tok)
        | None -> None)
      in
      Formal_hash_splat (v1, v2)
  | `Simple_form_param_blk_param (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = str v2 in
      Formal_amp (v1, v2)
  | `Simple_form_param_kw_param (v1, v2, v3) ->
      let v1 = str v1 in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> Some (arg x)
        | None -> None)
      in
      Formal_kwd (v1, v2, v3)
  | `Simple_form_param_opt_param (v1, v2, v3) ->
      let v1 = str v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Formal_default (v1, v2, v3)
  )

and superclass ((v1, v2) : CST.superclass) =
  let v1 = token2 v1 in
  let v2 = arg v2 in
  (v1, v2)

and in_ ((v1, v2) : CST.in_) =
  let v1 = token2 v1 in
  let v2 = arg v2 in
  (v1, v2)

and when_ ((v1, v2, v3, v4) : CST.when_) =
  let twhen = token2 v1 in
  let v2 = pattern v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _tcomma = token2 v1 in
      let v2 = pattern v2 in
      v2
    ) v3
  in
  let v4 =
    (match v4 with
    | `Term x -> let _ = terminator x in []
    | `Then x -> (then_ x)
    )
  in
  twhen, (v2::v3), v4

and pattern (x : CST.pattern) : AST.pattern =
  (match x with
  | `Pat_arg x -> arg x
  | `Pat_splat_arg x -> splat_argument x
  )

and elsif ((v1, v2, v3, v4) : CST.elsif) : AST.tok * AST.stmt =
  let v1 = token2 v1 in
  let v2 = statement v2 in
  let v3 =
    (match v3 with
    | `Term x -> let _ = terminator x in []
    | `Then x -> (then_ x)
    )
  in
  let v4 =
    (match v4 with
    | Some x ->
        Some (match x with
        | `Else x -> else_ x
        | `Elsif x ->
                let (t, s) = elsif x in
                t, [S s]
        )
    | None -> None)
  in
  v1, If (v1, v2, v3, v4)

and else_ ((v1, v2, v3) : CST.else_) : (AST.tok * AST.stmts) =
  let v1 = token2 v1 in
  let _v2 =
    (match v2 with
    | Some x -> terminator x
    | None -> ())
  in
  let v3 =
    (match v3 with
    | Some x -> statements x
    | None -> [])
  in
  v1, v3

and then_ (x : CST.then_) : AST.stmts =
  (match x with
  | `Then_term_stmts (v1, v2) ->
      let _v1 = terminator v1 in
      let v2 = statements v2 in
      v2
  | `Then_opt_term_then_opt_stmts (v1, v2, v3) ->
      let _v1 =
        (match v1 with
        | Some x -> terminator x
        | None -> ())
      in
      let _v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> statements x
        | None -> [])
      in
      v3
  )

and ensure ((v1, v2) : CST.ensure) =
  let v1 = token2 v1 in
  let v2 =
    (match v2 with
    | Some x -> statements x
    | None -> [])
  in
  (v1, v2)

and rescue ((v1, v2, v3, v4) : CST.rescue) =
  let v1 = token2 v1 in
  let v2 =
    (match v2 with
    | Some x -> exceptions x
    | None -> []
    )
  in
  let v3 =
    (match v3 with
    | Some x -> Some (exception_variable x)
    | None -> None
    )
  in
  let v4 =
    (match v4 with
    | `Term x -> let _ = terminator x in []
    | `Then x -> (then_ x)
    )
  in
  (v1, v2, v3, v4)

and exceptions ((v1, v2) : CST.exceptions) : AST.expr list =
  let v1 =
    (match v1 with
    | `Arg x -> arg x
    | `Splat_arg x -> splat_argument x
    )
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token2 v1 in
      let v2 =
        (match v2 with
        | `Arg x -> arg x
        | `Splat_arg x -> splat_argument x
        )
      in
      v2
    ) v2
  in
  v1::v2

and exception_variable ((v1, v2) : CST.exception_variable) =
  let v1 = token2 v1 in
  let v2 = lhs v2 in
  v1, v2

and body_statement ((v1, v2, v3) : CST.body_statement) :
 AST.body_exn * AST.tok =
  let v1 =
    (match v1 with
    | Some x -> statements x
    | None -> [])
  in
  let (rescue_exprs, else_expr, ensure_expr) =
    Common2.partition_either3 (fun x ->
      (match x with
      | `Resc x -> Common2.Left3 (rescue x)
      | `Else x -> Common2.Middle3 (else_ x)
      | `Ensu x ->
              let (t, xs) = ensure x in
              Common2.Right3 (t, xs)
      )
    ) v2
  in
  let tend = token2 v3 in
  let ensure_expr =
    match ensure_expr with
    | [] -> None
    | [x] -> Some x
    | x::_ -> Some x (* TODO: weird, should have only one no? *)
   in
  let else_expr =
    match else_expr with
    | [] -> None
    | [x] -> Some x
    | x::_ -> Some x (* TODO: weird, should have only one no? *)
   in
  { body_exprs = v1; rescue_exprs; else_expr; ensure_expr}, tend

and expression (x : CST.expression) : AST.expr =
  (match x with
  | `Exp_cmd_bin (v1, v2, v3) ->
      let v1 = expression v1 in
      let v2 =
        (match v2 with
        | `Or tok -> Op_kOR, token2 tok
        | `And tok -> Op_kAND, token2 tok
        )
      in
      let v3 = expression v3 in
      Binop (v1, v2, v3)
  | `Exp_cmd_assign x -> command_assignment x
  | `Exp_cmd_op_assign (v1, v2, v3) ->
      let v1 = lhs v1 in
      let (op, tok) =
        (match v2 with
        | `PLUSEQ tok     -> B Op_PLUS  , token2 tok
        | `DASHEQ tok     -> B Op_MINUS , token2 tok
        | `STAREQ tok     -> B Op_TIMES , token2 tok
        | `STARSTAREQ tok -> B Op_POW   , token2 tok
        | `SLASHEQ tok    -> B Op_DIV   , token2 tok
        | `BARBAREQ tok   -> Op_OR    , token2 tok
        | `BAREQ tok      -> B Op_BOR   , token2 tok
        | `AMPAMPEQ tok   -> Op_AND   , token2 tok
        | `AMPEQ tok      -> B Op_BAND  , token2 tok
        | `PERCEQ tok     -> B Op_REM   , token2 tok
        | `GTGTEQ tok     -> B Op_RSHIFT, token2 tok
        | `LTLTEQ tok     -> B Op_LSHIFT, token2 tok
        | `HATEQ tok      -> B Op_XOR   , token2 tok
        )
      in
      let v3 = expression v3 in
      Binop (v1, (Op_OP_ASGN op, tok), v3)
  | `Exp_cmd_call x -> command_call x
  | `Exp_ret_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Return (v1, v2))
  | `Exp_yield_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Yield (v1, v2))
  | `Exp_brk_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Break (v1, v2))
  | `Exp_next_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Next (v1, v2))
  | `Exp_arg x -> arg x
  )

and arg (x : CST.arg) : AST.expr =
  (match x with
  | `Arg_prim x -> primary x
  | `Arg_assign x -> assignment x
  | `Arg_op_assign (v1, v2, v3) ->
      let v1 = lhs v1 in
      let (op, tok) =
        (match v2 with
        | `PLUSEQ tok     -> B Op_PLUS  , token2 tok
        | `DASHEQ tok     -> B Op_MINUS , token2 tok
        | `STAREQ tok     -> B Op_TIMES , token2 tok
        | `STARSTAREQ tok -> B Op_POW   , token2 tok
        | `SLASHEQ tok    -> B Op_DIV   , token2 tok
        | `BARBAREQ tok   -> Op_OR    , token2 tok
        | `BAREQ tok      -> B Op_BOR   , token2 tok
        | `AMPAMPEQ tok   -> Op_AND   , token2 tok
        | `AMPEQ tok      -> B Op_BAND  , token2 tok
        | `PERCEQ tok     -> B Op_REM   , token2 tok
        | `GTGTEQ tok     -> B Op_RSHIFT, token2 tok
        | `LTLTEQ tok     -> B Op_LSHIFT, token2 tok
        | `HATEQ tok      -> B Op_XOR   , token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, (Op_OP_ASGN op, tok), v3)
  | `Arg_cond (v1, v2, v3, v4, v5) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      let v4 = token2 v4 in
      let v5 = arg v5 in
      Ternary (v1, v2, v3, v4, v5)
  | `Arg_range (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> B Op_DOT2, token2 tok
        | `DOTDOTDOT tok -> Op_DOT3, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Arg_bin x -> binary x
  | `Arg_un x -> unary x
  )

and primary (x : CST.primary) : AST.expr =
  (match x with
  | `Prim_paren_stmts x ->
        let (lp, xs, rp) = parenthesized_statements x in
        S (Block (lp, xs, rp))
  | `Prim_lhs x -> lhs x
  | `Prim_array (v1, v2, v3) ->
      let lb = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list_with_trailing_comma x
        | None -> [])
      in
      let rb = token2 v3 in
      Array (lb, v2, rb)
  (* ?? *)
  | `Prim_str_array (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let _v2 =
        (match v2 with
        | Some () -> ()
        | None -> ())
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = literal_contents v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let _v1 = blank v1 in
                let v2 = literal_contents v2 in
                v2
              ) v2
            in
            v1::v2
        | None -> [])
      in
      let _v4 =
        (match v4 with
        | Some () -> ()
        | None -> ())
      in
      let _v5 = token2 v5 in
      Literal (String (Double (v3 |> List.flatten), v1)) (* Double? *)
  | `Prim_symb_array (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let _v2 =
        (match v2 with
        | Some () -> ()
        | None -> ())
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = literal_contents v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let _v1 = blank v1 in
                let v2 = literal_contents v2 in
                v2
              ) v2
            in
            v1::v2
        | None -> [])
      in
      let _v4 =
        (match v4 with
        | Some () -> ()
        | None -> ())
      in
      let _v5 = token2 v5 in
      Literal (Atom (v3 |> List.flatten, v1))
  | `Prim_hash (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | `Pair x -> pair x
              | `Hash_splat_arg x -> hash_splat_argument x
              )
            in
            let v2 =
              List.map (fun (v1, v2) ->
                let _v1 = token2 v1 in
                let v2 =
                  (match v2 with
                  | `Pair x -> pair x
                  | `Hash_splat_arg x -> hash_splat_argument x
                  )
                in
                v2
              ) v2
            in
            let _v3 =
              (match v3 with
              | Some _tok -> ()
              | None -> ())
            in
            v1::v2
        | None -> [])
      in
      let v3 = token2 v3 in
      Hash (true, (v1, v2, v3))
  | `Prim_subs (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> literal_contents x
        | None -> [])
      in
      let _v3 = token2 v3 in
      Literal (String (Tick v2, v1)) (* Tick? *)
  | `Prim_symb x -> Literal (Atom (symbol x))
  | `Prim_int tok -> Literal (Num (str tok))
  | `Prim_float tok -> Literal (Float (str tok))
  | `Prim_comp tok -> Literal (Complex (str tok))
  | `Prim_rati (v1, v2) ->
      let v1 = str v1 in
      let v2 = token2 v2 in
      Literal (Rational (v1, v2))
  | `Prim_str x ->
        let (t1, xs, _t2) = string_ x in
        Literal (String (Double xs, t1))
  | `Prim_char tok ->
        Literal (Char (str tok))
  | `Prim_chai_str (v1, v2) ->
      let (t1, v1, _) = string_ v1 in
      let v2 = List.map (fun x ->
              let (_lp, x, _) = string_ x in
              x
        ) v2 |> List.flatten
        in
      Literal (String (Double (v1 @ v2), t1))

  | `Prim_regex (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> literal_contents x
        | None -> [])
      in
      let _v3 = token2 v3 in
      Literal (Regexp ((v2, "??"), v1))
  | `Prim_lamb (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x ->
            Some (match x with
            | `Params x -> parameters x |> G.unbracket
            | `Bare_params x -> bare_parameters x
            )
        | None -> None)
      in
      let v3 =
        (match v3 with
        | `Blk x -> block x
        | `Do_blk x -> do_block x
        )
      in
      let b = false in (* should have a third option for lambdas *)
      CodeBlock ((v1, b, v1), v2, [v3])
  | `Prim_meth (v1, v2) ->
      let v1 = token2 v1 in
      let (n, params, body_exn) = method_rest v2 in
      D (MethodDef (v1, M n, params, body_exn))
  | `Prim_sing_meth (v1, v2, v3, v4) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | `Var x -> Id (variable x)
        | `LPAR_arg_RPAR (v1, v2, v3) ->
            let _lp = token2 v1 in
            let v2 = arg v2 in
            let _rp = token2 v3 in
            v2
        )
      in
      let v3 =
        (match v3 with
        | `DOT tok ->
                (fun a b -> DotAccess(a, token2 tok, b))
        | `COLONCOLON tok ->
                (fun a b -> ScopedId(Scope(a, token2 tok, SM b)))
        )
      in
      let (n, params, body_exn) = method_rest v4 in
      let n = v3 v2 n in
      D (MethodDef (v1, SingletonM n, params, body_exn))
  | `Prim_class (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | `Cst tok -> NameConstant (str tok)
        | `Scope_resol x -> NameScope (scope_resolution x)
        )
      in
      let v3 =
        (match v3 with
        | Some x -> Some (superclass x)
        | None -> None)
      in
      let _v4 = terminator v4 in
      let (v5, _tend) = body_statement v5 in
      D (ClassDef (v1, C (v2, v3), v5))
  | `Prim_sing_class (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      let _v4 = terminator v4 in
      let (v5, _tend) = body_statement v5 in
      D (ClassDef (v1, SingletonC (v2, v3), v5))
  | `Prim_modu (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | `Cst tok -> NameConstant (str tok)
        | `Scope_resol x -> NameScope (scope_resolution x)
        )
      in
      let (v3, _tend) =
        (match v3 with
        | `Term_body_stmt (v1, v2) ->
            let _v1 = terminator v1 in
            let v2 = body_statement v2 in
            v2
        | `End tok -> empty_body_exn, token2 tok
        )
      in
      D (ModuleDef (v1, v2, v3))
  | `Prim_begin (v1, v2, v3) ->
      let tbegin = token2 v1 in
      let _v2 =
        (match v2 with
        | Some x -> terminator x
        | None -> ())
      in
      let (v3, tend) = body_statement v3 in
      S (Block (tbegin, [S (ExnBlock (v3))], tend))
  | `Prim_while (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      let _v3 = do_ v3 in
      let v4 =
        (match v4 with
        | Some x -> statements x
        | None -> [])
      in
      let _tend = token2 v5 in
      S (While (v1, true, v2, v4))
  | `Prim_until (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      let _v3 = do_ v3 in
      let v4 =
        (match v4 with
        | Some x -> statements x
        | None -> [])
      in
      let _tend = token2 v5 in
      S (Until (v1, true, v2, v4))
  | `Prim_if (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let v2 = statement v2 in
      let v3 =
        (match v3 with
        | `Term x -> let _ = terminator x in []
        | `Then x -> (then_ x)
        )
      in
      let v4 =
        (match v4 with
        | Some x ->
            Some (match x with
            | `Else x -> (else_ x)
            | `Elsif x ->
              let (t, s) =  elsif x in
              t, [S s]
            )
        | None -> None)
      in
      let _v5 = token2 v5 in
      S (If (v1, v2, v3, v4))
  | `Prim_unle (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let v2 = statement v2 in
      let v3 =
        (match v3 with
        | `Term x -> let _ = terminator x in []
        | `Then x -> (then_ x)
        )
      in
      let v4 =
        (match v4 with
        | Some x ->
            Some (match x with
            | `Else x -> (else_ x)
            | `Elsif x ->
                      let (t, s) = elsif x in
                      t, [S s]
            )
        | None -> None)
      in
      let _v5 = token2 v5 in
      S (Unless (v1, v2, v3, v4))
  | `Prim_for (v1, v2, v3, v4, v5, v6) ->
      let v1 = token2 v1 in
      let v2 = mlhs v2 in
      let (t, e) = in_ v3 in
      let _v4 = do_ v4 in
      let v5 =
        (match v5 with
        | Some x -> statements x
        | None -> [])
      in
      let _v6 = token2 v6 in
      S (For (v1, v2 |> list_to_maybe_tuple, t, e, v5))
  | `Prim_case (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> Some (arg x)
        | None -> None)
      in
      let _v3 = terminator v3 in
      let _v4 = List.map token2 v4 in (* ?? list of ';'?? *)
      let v5 = List.map when_ v5 in
      let v6 =
        (match v6 with
        | Some x -> Some (else_ x)
        | None -> None)
      in
      let _v7 = token2 v7 in
      S (Case (v1, { case_guard = v2; case_whens = v5; case_else = v6}))
  | `Prim_ret (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Return (v1, v2))
  | `Prim_yield (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Yield (v1, v2))
  | `Prim_brk (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Break (v1, v2))
  | `Prim_next (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Next (v1, v2))
  | `Prim_redo (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Redo (v1, v2))
  | `Prim_retry (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Retry (v1, v2))
  | `Prim_paren_un (v1, v2) ->
      let v1 =
        (match v1 with
        | `Defi tok -> Op_DefinedQuestion, token2 tok
        | `Not tok -> Op_UNot, token2 tok
        )
      in
      let (lp, v2, rp) = parenthesized_statements v2 in
      let block = S (Block (lp, v2, rp)) in
      Unary (v1, block)
  | `Prim_un_lit (v1, v2) ->
      let v1 =
        (match v1 with
        | `Un_minus tok -> U Op_UMinus, (token2 tok)
        | `PLUS tok -> U Op_UPlus, (token2 tok)
        )
      in
      let v2 =
        (match v2 with
        | `Int tok -> Literal (Num (str tok))
        | `Float tok -> Literal (Float (str tok))
        )
      in
      Unary (v1, v2)
  | `Prim_here_begin tok ->
    let (s, tok) = str tok in
    Literal (String (Single s, tok))
  )

and parenthesized_statements ((v1, v2, v3) : CST.parenthesized_statements) =
  let v1 = token2 v1 in
  let v2 =
    (match v2 with
    | Some x -> statements x
    | None -> [])
  in
  let v3 = token2 v3 in
  (v1, v2, v3)

and scope_resolution ((v1, v2) : CST.scope_resolution) : AST.scope_resolution =
  let v1 =
    (match v1 with
    | `COLONCOLON tok -> (fun e -> (TopScope(token2 tok, e)))
    | `Prim_COLONCOLON (v1, v2) ->
        let v1 = primary v1 in
        let v2 = token2 v2 in
        (fun e -> (Scope((v1, v2, SV e))))
    )
  in
  let v2 =
    (match v2 with
    | `Id tok -> (str tok, ID_Lowercase)
    | `Cst tok -> (str tok, ID_Uppercase)
    )
  in
  v1 v2

and call ((v1, v2, v3) : CST.call) =
  let v1 = primary v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> token2 tok
    | `AMPDOT tok -> token2 tok
    )
  in
  let v3 =
    (match v3 with
    | `Id tok -> MethodId (str tok, ID_Lowercase)
    | `Op x ->
          let op = operator x in
          (match op with
          | Left bin, t -> MethodOperator (bin, t)
          | Right un, t -> MethodUOperator (un, t)
          )
    | `Cst tok -> MethodId (str tok, ID_Uppercase)
    | `Arg_list x -> (* ?? *)
          MethodDynamic (Tuple ((argument_list x |> G.unbracket)))
    )
  in
  DotAccess (v1, v2, v3)

and command_call (x : CST.command_call) : AST.expr =
  (match x with
  | `Cmd_call_choice_var_cmd_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = command_argument_list v2 in
      Call (v1, v2, None)
  | `Cmd_call_choice_var_cmd_arg_list_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = command_argument_list v2 in
      let v3 = block v3 in
      Call (v1, v2, Some v3)
  | `Cmd_call_choice_var_cmd_arg_list_do_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = command_argument_list v2 in
      let v3 = do_block v3 in
      Call (v1, v2, Some v3)
  )

and method_call (x : CST.method_call) : AST.expr =
  (match x with
  | `Meth_call_choice_var_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = argument_list v2 |> G.unbracket in
      Call (v1, v2, None)
  | `Meth_call_choice_var_arg_list_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = argument_list v2 |> G.unbracket in
      let v3 = block v3 in
      Call (v1, v2, Some v3)
  | `Meth_call_choice_var_arg_list_do_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = argument_list v2 |> G.unbracket in
      let v3 = do_block v3 in
      Call (v1, v2, Some v3)
  | `Meth_call_choice_var_blk (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = block v2 in
      Call (v1, [], Some v2)
  | `Meth_call_choice_var_do_blk (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = do_block v2 in
      Call (v1, [], Some v2)
  )

and command_argument_list (x : CST.command_argument_list) : AST.expr list =
  (match x with
  | `Cmd_arg_list_arg_rep_COMMA_arg (v1, v2) ->
      let v1 = argument v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let _t = token2 v1 in
          let v2 = argument v2 in
          v2
        ) v2
      in
      v1::v2
  | `Cmd_arg_list_cmd_call x -> [command_call x]
  )

and argument_list ((v1, v2, v3) : CST.argument_list) : AST.expr list AST.bracket =
  let lp = token2 v1 in
  let v2 =
    (match v2 with
    | Some x -> argument_list_with_trailing_comma x
    | None -> [])
  in
  let rp = token2 v3 in
  (lp, v2, rp)

and argument_list_with_trailing_comma ((v1, v2, v3) : CST.argument_list_with_trailing_comma) : AST.expr list =
  let v1 = argument v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _ = token2 v1 in
      let v2 = argument v2 in
      v2
    ) v2
  in
  let _v3 =
    (match v3 with
    | Some _tok -> ()
    | None -> ())
  in
  v1::v2

and argument (x : CST.argument) : AST.expr =
  (match x with
  | `Arg_arg x -> arg x
  | `Arg_splat_arg x -> splat_argument x
  | `Arg_hash_splat_arg x -> hash_splat_argument x
  | `Arg_blk_arg (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      Unary ((Op_UAmper, v1), v2)
  | `Arg_pair x -> pair x
  )

and splat_argument ((v1, v2) : CST.splat_argument) =
  let v1 = token2 v1 in
  let v2 = arg v2 in
  Splat(v1, Some v2)

and hash_splat_argument ((v1, v2) : CST.hash_splat_argument) =
  let v1 = token2 v1 in
  let v2 = arg v2 in
  Unary ((Op_UStarStar, v1), v2)

and do_block ((v1, v2, v3, v4) : CST.do_block) : AST.expr =
  let tdo = token2 v1 in
  let _v2 =
    (match v2 with
    | Some x -> terminator x
    | None -> ())
  in
  let params_opt =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = block_parameters v1 |> G.unbracket in
        let _v2 =
          (match v2 with
          | Some x -> terminator x
          | None -> ())
        in
        Some v1
    | None -> None)
  in
  let (exn_block, tend) = body_statement v4 in
  let xs = [S (ExnBlock (exn_block))] in
  CodeBlock ((tdo, false, tend), params_opt, xs)

and block ((v1, v2, v3, v4) : CST.block) =
  let lb = token2 v1 in
  let params_opt =
    (match v2 with
    | Some x -> Some (block_parameters x |> G.unbracket)
    | None -> None)
  in
  let v3 =
    (match v3 with
    | Some x -> statements x
    | None -> [])
  in
  let rb = token2 v4 in
  CodeBlock ((lb,true,rb), params_opt, v3)

and assignment (x : CST.assignment) =
  (match x with
  | `Choice_lhs_EQ_choice_arg (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Lhs x -> lhs x
        | `Left_assign_list x -> left_assignment_list x
        )
      in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | `Arg x -> arg x
        | `Splat_arg x -> splat_argument x
        | `Right_assign_list x -> right_assignment_list x |> list_to_maybe_tuple
        )
      in
      Binop (v1, (Op_ASSIGN, v2), v3)
  )

and command_assignment (x : CST.command_assignment) =
  (match x with
  | `Choice_lhs_EQ_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Lhs x -> lhs x
        | `Left_assign_list x -> left_assignment_list x
        )
      in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      Binop (v1, (Op_ASSIGN, v2), v3)
  )

and binary (x : CST.binary) =
  (match x with
  | `Bin_arg_and_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kAND, v2), v3)
  | `Bin_arg_or_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kOR, v2), v3)
  | `Bin_arg_BARBAR_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_OR, v2), v3)
  | `Bin_arg_AMPAMP_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_AND, v2), v3)
  | `Bin_arg_choice_LTLT_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> B Op_LSHIFT, token2 tok
        | `GTGT tok -> B Op_RSHIFT, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Bin_arg_choice_LT_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `LT tok -> B Op_LT, token2 tok
        | `LTEQ tok -> B Op_LEQ, token2 tok
        | `GT tok -> B Op_GT, token2 tok
        | `GTEQ tok -> B Op_GEQ, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Bin_arg_AMP_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kAND, v2), v3)
  | `Bin_arg_choice_HAT_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `HAT tok -> B Op_XOR, token2 tok
        | `BAR tok -> B Op_BOR, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Bin_arg_choice_PLUS_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> B Op_PLUS, token2 tok
        | `Bin_minus tok -> B Op_MINUS, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Bin_arg_choice_SLASH_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `SLASH tok -> B Op_DIV, token2 tok
        | `PERC tok -> B Op_REM, token2 tok
        | `Bin_star tok -> B Op_TIMES, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Bin_arg_choice_EQEQ_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> B Op_EQ, token2 tok
        | `BANGEQ tok -> B Op_NEQ, token2 tok
        | `EQEQEQ tok -> B Op_EQQ, token2 tok
        | `LTEQGT tok -> B Op_CMP, token2 tok
        | `EQTILDE tok -> B Op_MATCH, token2 tok
        | `BANGTILDE tok -> B Op_NMATCH, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Bin_arg_STARSTAR_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kAND, v2), v3)
  )

and unary (x : CST.unary) =
  (match x with
  | `Un_defi_arg (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      Unary ((Op_DefinedQuestion, v1), v2)
  | `Un_not_arg (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      Unary ((Op_UNot, v1), v2)
  | `Un_choice_un_minus_arg (v1, v2) ->
      let v1 =
        (match v1 with
        | `Un_minus tok -> U Op_UMinus, token2 tok
        | `PLUS tok -> U Op_UPlus, token2 tok
        )
      in
      let v2 = arg v2 in
      Unary (v1, v2)
  | `Un_choice_BANG_arg (v1, v2) ->
      let v1 =
        (match v1 with
        | `BANG tok -> U Op_UBang, token2 tok
        | `TILDE tok -> U Op_UTilde, token2 tok
        )
      in
      let v2 = arg v2 in
      Unary (v1, v2)
  )

and right_assignment_list ((v1, v2) : CST.right_assignment_list) : AST.expr list =
  let v1 =
    (match v1 with
    | `Arg x -> arg x
    | `Splat_arg x -> splat_argument x
    )
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token2 v1 in
      let v2 =
        (match v2 with
        | `Arg x -> arg x
        | `Splat_arg x -> splat_argument x
        )
      in
      v2
    ) v2
  in
  v1::v2

and left_assignment_list (x : CST.left_assignment_list) : AST.expr =
  mlhs x |> list_to_maybe_tuple

and mlhs ((v1, v2, v3) : CST.mlhs) : AST.expr list =
  let v1 =
    (match v1 with
    | `Lhs x -> lhs x
    | `Rest_assign x -> rest_assignment x
    | `Dest_left_assign x -> destructured_left_assignment x
    )
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token2 v1 in
      let v2 =
        (match v2 with
        | `Lhs x -> lhs x
        | `Rest_assign x -> rest_assignment x
        | `Dest_left_assign x -> destructured_left_assignment x
        )
      in
      v2
    ) v2
  in
  let _v3 =
    (match v3 with
    | Some _tok -> ()
    | None -> ())
  in
  v1::v2

and destructured_left_assignment ((v1, v2, v3) : CST.destructured_left_assignment) =
  let _lp = token2 v1 in
  let v2 = mlhs v2 in
  let _rp = token2 v3 in
  Tuple (v2)

and rest_assignment ((v1, v2) : CST.rest_assignment) =
  let v1 = token2 v1 in
  let v2 =
    (match v2 with
    | Some x -> Some (lhs x)
    | None -> None
    )
  in
  Splat (v1, v2)

and lhs (x : CST.lhs) : AST.expr =
  (match x with
  | `Var x -> Id (variable x)
  | `True x -> Literal (Bool (true_ x))
  | `False x -> Literal (Bool (false_ x))
  | `Nil x -> Literal (Nil (nil x))
  | `Scope_resol x -> ScopedId (scope_resolution x)
  | `Elem_ref (v1, v2, v3, v4) ->
      let v1 = primary v1 in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> argument_list_with_trailing_comma x
        | None -> [])
      in
      let _v4 = token2 v4 in
      let e = DotAccess (v1, (v2), MethodOperator (Op_AREF, v2)) in
      Call (e, v3, None)
  | `Call x -> call x
  | `Meth_call x -> method_call x
  )

and method_name (x : CST.method_name) : AST.method_name =
  (match x with
  | `Meth_name_id tok -> MethodId (str tok, ID_Lowercase)
  | `Meth_name_cst tok -> MethodId (str tok, ID_Uppercase)
  | `Meth_name_sett (v1, v2) ->
      let v1 = str v1 in
      let v2 = token2 v2 in
      MethodIdAssign (v1, v2, ID_Lowercase)
  | `Meth_name_symb x -> MethodAtom (symbol x)
  | `Meth_name_op x -> let op = operator x in
        (match op with
        | Left bin, t -> MethodOperator (bin ,t)
        | Right un, t -> MethodUOperator (un, t)
        )
  | `Meth_name_inst_var tok -> MethodId (str tok, ID_Instance)
  | `Meth_name_class_var tok -> MethodId (str tok, ID_Class)
  | `Meth_name_glob_var tok -> MethodId (str tok, ID_Global)
  )

and interpolation ((v1, v2, v3) : CST.interpolation) : AST.expr AST.bracket =
  let lb = token2 v1 in
  let v2 = statement v2 in
  let rb = token2 v3 in
  (lb, v2, rb)

and string_ ((v1, v2, v3) : CST.string_) : AST.string_contents list bracket =
  let v1 = token2 v1 in
  let v2 =
    (match v2 with
    | Some x -> literal_contents x
    | None -> [])
  in
  let v3 = token2 v3 in
  v1, v2, v3

and symbol (x : CST.symbol) : AST.atom =
  (match x with
  | `Symb_simple_symb tok -> let (s, t) = str tok in
        ([StrChars s], t)
  | `Symb_symb_start_opt_lit_content_str_end (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> literal_contents x
        | None -> [])
      in
      let _v3 = token2 v3 in
      (v2, v1)
  )

and literal_contents (xs : CST.literal_contents) : AST.string_contents list =
  List.map (fun x ->
    (match x with
    | `Str_content tok ->
            let (str, _t) = str tok in
            StrChars str
    | `Interp x ->
            let (_lb, e, _rb) = interpolation x in
            StrExpr e
    | `Esc_seq tok ->
            let (str, _t) = str tok in
            StrChars str
    )
  ) xs

and pair (x : CST.pair) =
  (match x with
  | `Pair_arg_EQGT_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop(v1, (Op_ASSOC, v2), v3)
  | `Pair_choice_id_hash_key_COLON_arg (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id_hash_key tok -> Id (str tok, ID_Lowercase)
        | `Id tok -> Id (str tok, ID_Lowercase)
        | `Cst tok -> Id (str tok, ID_Uppercase)
        | `Str x ->
        let (t1, xs, _t2) = string_ x in
        Literal (String (Double xs, t1))
        )
      in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop(v1, (Op_ASSOC, v2), v3)
  )

let program ((v1, _v2interpreted) : CST.program) : AST.stmts  =
  match v1 with
  | Some x -> statements x
  | None -> []


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  (* TODO: tree-sitter bindings are buggy so we cheat and fork to
   * avoid segfaults to popup. See Main.ml test_parse_ruby function.
   *)
   let cst_opt =
      if false
      then Tree_sitter_ruby.Parse.file file
      else begin
         Parallel.backtrace_when_exn := false;
         Parallel.invoke Tree_sitter_ruby.Parse.file file ()
      end
   in
   match cst_opt with
   | None -> failwith (spf "No CST returned for %s" file)
   | Some x ->
      (*
      let sexp = CST.sexp_of_program x in
      let s = Sexplib.Sexp.to_string_hum sexp in
      pr s;
      *)
      global_file := file;
      global_conv := line_col_to_pos file;
      program x

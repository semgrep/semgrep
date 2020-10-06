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
module H = Parse_tree_sitter_helpers

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
(* TODO: pass via env parameter like we do in Parse_java_tree_sitter.ml
 * which started from a better Boilerplate.ml.
 * Not very multicore friendly
 *)
let global_file = ref ""
let global_conv = ref (Hashtbl.create 0)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let list_to_maybe_tuple = function
 | [] -> raise Impossible
 | [x] -> x
 | xs -> Tuple (xs)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying ocaml-tree-sitter-lang/ruby/Boilerplate.ml *)

let token2 x =
  let env = { H.file = !global_file; conv = !global_conv } in
  H.token env x

let str x =
  let env = { H.file = !global_file; conv = !global_conv } in
  H.str env x

let false_ (x : CST.false_) : bool wrap =
  (match x with
  | `False tok -> false, token2 tok
  | `FALSE tok -> false, token2 tok
  )

let true_ (x : CST.true_) : bool wrap =
  (match x with
  | `True tok -> true, token2 tok
  | `TRUE tok -> true, token2 tok
  )

let nil (x : CST.nil) : tok =
  (match x with
  | `Nil tok -> token2 tok
  | `NIL tok -> token2 tok
  )


let operator (x : CST.operator) =
  (match x with

  | `DOTDOT tok -> Left Op_DOT2, (token2 tok)
  | `BAR tok -> Left Op_BOR, (token2 tok)
  | `HAT tok -> Left Op_XOR, (token2 tok)
  | `AMP tok -> Left Op_BAND, (token2 tok)

  | `LTEQGT tok -> Left Op_CMP, (token2 tok)
  | `EQEQ tok -> Left Op_EQ, (token2 tok)
  | `EQEQEQ tok -> Left Op_EQQ, (token2 tok)
  | `EQTILDE tok -> Left Op_MATCH, (token2 tok)
  | `GT tok -> Left Op_GT, (token2 tok)
  | `GTEQ tok -> Left Op_GEQ, (token2 tok)
  | `LT tok -> Left Op_LT, (token2 tok)
  | `LTEQ tok -> Left Op_LEQ, (token2 tok)

  | `PLUS tok -> Left Op_PLUS, (token2 tok)
  | `DASH tok -> Left Op_MINUS, (token2 tok)
  | `STAR tok -> Left Op_TIMES, (token2 tok)
  | `SLASH tok -> Left Op_DIV, (token2 tok)
  | `PERC tok -> Left Op_REM, (token2 tok)
  | `BANGTILDE tok -> Left Op_NMATCH, (token2 tok)
  | `STARSTAR tok -> Left Op_POW, (token2 tok)
  | `LTLT tok -> Left Op_LSHIFT, (token2 tok)
  | `GTGT tok -> Left Op_RSHIFT, (token2 tok)
  | `LBRACKRBRACK tok -> Left Op_AREF, (token2 tok)
  | `LBRACKRBRACKEQ tok -> Left Op_ASET, (token2 tok)

  | `PLUSAT tok -> Right Op_UPlus, token2 tok
  | `DASHAT tok -> Right Op_UMinus, token2 tok

  | `TILDE tok -> Right Op_UTilde, token2 tok
  | `BANG tok -> Right Op_UBang, token2 tok

  (* TODO *)
  | `BQUOT tok ->
        pr2_gen tok;
        failwith "Op_BQUOT???"
  )


let terminator (x : CST.terminator) : unit =
  (match x with
  | `Line_brk _tok -> ()
  | `SEMI _tok -> ()
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
  | `Global_var tok ->
        (str tok, ID_Global)
  | `Id tok ->
        (str tok, ID_Lowercase)
  | `Cst tok ->
        (str tok, ID_Uppercase)
  )

let do_ (x : CST.do_) : unit =
  (match x with
  | `Do _tok -> ()
  | `Term x -> terminator x
  )

let rec statements (x : CST.statements) : AST.stmts =
  (match x with
  | `Rep1_choice_stmt_term_opt_stmt (v1, v2) ->
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
  | `Stmt x -> [statement x]
  )

and statement (x : CST.statement) : AST.expr (* TODO AST.stmt at some point *)=
  (match x with
  | `Undef (v1, v2, v3) ->
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
  | `Alias (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 = method_name v2 in
      let v3 = method_name v3 in
      D (Alias (v1, v2, v3))
  | `If_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (If (v2, v3, [v1], None))
  | `Unless_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (Unless (v2, v3, [v1], None))
  | `While_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      let b = true (* ?? *) in
      S (While (v2, b, v3, [v1]))
  | `Until_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (Until (v2, true, v3, [v1]))

  | `Rescue_modi (v1, v2, v3) ->
      let v1 = statement v1 in
      let v2 = token2 v2 in
      let v3 = expression v3 in
      S (ExnBlock ({
         body_exprs = [v1];
         rescue_exprs = [v2, [], None, [v3]];
         ensure_expr = None;
         else_expr = None;
        }))


  | `Begin_blk (v1, v2, v3, v4) ->
      let v1 = token2 v1 in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> statements x
        | None -> [])
      in
      let v4 = token2 v4 in
      D (BeginBlock (v1, (v2, v3, v4)))
  | `End_blk (v1, v2, v3, v4) ->
      let v1 = token2 v1 in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> statements x
        | None -> [])
      in
      let v4 = token2 v4 in
      D (EndBlock (v1, (v2, v3, v4)))
  | `Exp x -> expression x
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
  | `Simple_formal_param x ->
      simple_formal_parameter x
  | `Params x ->
        let (lp, xs, rp) = parameters x in
        Formal_tuple ((lp, xs, rp))
  )

and simple_formal_parameter (x : CST.simple_formal_parameter) : AST.formal_param =
  (match x with
  | `Id tok ->
        let id = str tok in
        Formal_id ((id))
  | `Splat_param (v1, v2) ->
      let v1 = token2 v1 in
        (match v2 with
        | Some tok -> let id = str tok in
           Formal_star (v1, id)
        | None ->
           Formal_rest v1
        )
  | `Hash_splat_param (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some tok -> Some (str tok)
        | None -> None)
      in
      Formal_hash_splat (v1, v2)
  | `Blk_param (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = str v2 in
      Formal_amp (v1, v2)
  | `Kw_param (v1, v2, v3) ->
      let v1 = str v1 in
      let v2 = token2 v2 in
      let v3 =
        (match v3 with
        | Some x -> Some (arg x)
        | None -> None)
      in
      Formal_kwd (v1, v2, v3)
  | `Opt_param (v1, v2, v3) ->
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
  | `Arg x -> arg x
  | `Splat_arg x -> splat_argument x
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
  | `Term_stmts (v1, v2) ->
      let _v1 = terminator v1 in
      let v2 = statements v2 in
      v2
  | `Opt_term_then_opt_stmts (v1, v2, v3) ->
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
      | `Rescue x -> Common2.Left3 (rescue x)
      | `Else x -> Common2.Middle3 (else_ x)
      | `Ensure x ->
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
  | `Cmd_bin (v1, v2, v3) ->
      let v1 = expression v1 in
      let v2 =
        (match v2 with
        | `Or tok -> Op_kOR, token2 tok
        | `And tok -> Op_kAND, token2 tok
        )
      in
      let v3 = expression v3 in
      Binop (v1, v2, v3)
  | `Cmd_assign x -> command_assignment x
  | `Cmd_op_assign (v1, v2, v3) ->
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
  | `Cmd_call x -> command_call x
  | `Ret_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Return (v1, v2))
  | `Yield_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Yield (v1, v2))
  | `Brk_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Break (v1, v2))
  | `Next_cmd (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = command_argument_list v2 in
      S (Next (v1, v2))
  | `Arg x -> arg x
  )

and arg (x : CST.arg) : AST.expr =
  (match x with
  | `Prim x -> primary x
  | `Assign x -> assignment x
  | `Op_assign (v1, v2, v3) ->
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
  | `Cond (v1, v2, v3, v4, v5) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      let v4 = token2 v4 in
      let v5 = arg v5 in
      Ternary (v1, v2, v3, v4, v5)
  | `Range (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> B Op_DOT2, token2 tok
        | `DOTDOTDOT tok -> Op_DOT3, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Bin x -> binary x
  | `Un x -> unary x
  )

and anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e ((v1, v2) : CST.anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e) =
  let v1 = literal_contents v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token2 v1 (* pattern \s+ *) in
      let v2 = literal_contents v2 in
      v2
    ) v2
  in
  v1::v2

and primary (x : CST.primary) : AST.expr =
  (match x with
  | `Paren_stmts x ->
        let (lp, xs, rp) = parenthesized_statements x in
        S (Block (lp, xs, rp))
  | `Lhs x -> lhs x
  | `Array (v1, v2, v3) ->
      let lb = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list_with_trailing_comma x
        | None -> [])
      in
      let rb = token2 v3 in
      Array (lb, v2, rb)
  (* ?? *)
  | `Str_array (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 (* string_array_start *) in
      let _v2 =
        (match v2 with
        | Some tok -> Some (token2 tok) (* pattern \s+ *)
        | None -> None)
      in
      let v3 =
        (match v3 with
        | Some x ->
            (anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e x)
        | None -> [])
      in
      let _v4 =
        (match v4 with
        | Some tok -> Some (token2 tok)
        | None -> None)
      in
      let v5 = token2 v5 (* string_end *) in
      Literal (String (Double (v1, v3 |> List.flatten, v5)))
  | `Symb_array (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let _v2 =
        (match v2 with
        | Some tok -> Some (token2 tok)
        | None -> None)
      in
      let v3 =
        (match v3 with
        | Some x ->
            (anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e x)
        | None -> [])
      in
      let _v4 =
        (match v4 with
        | Some tok -> Some (token2 tok)
        | None -> None)
      in
      let v5 = token2 v5 in
      Atom (AtomFromString (v1, v3 |> List.flatten, v5))
  | `Hash (v1, v2, v3) ->
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
  | `Subs (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> literal_contents x
        | None -> [])
      in
      let v3 = token2 v3 in
      Literal (String (Tick (v1, v2, v3)))
  | `Symb x -> Atom (symbol x)
  | `Int tok -> Literal (Num (str tok))
  | `Float tok -> Literal (Float (str tok))
  | `Comp tok -> Literal (Complex (str tok))
  | `Rati (v1, v2) ->
      let v1 = str v1 in
      let v2 = token2 v2 in
      Literal (Rational (v1, v2))
  | `Str x ->
        Literal (String (Double (string_ x)))
  | `Char tok ->
        Literal (Char (str tok))
  (* ??? *)
  | `Chai_str (v1, v2) ->
      let (l, v1, r) = string_ v1 in
      let v2 = List.map (fun x ->
              let (_lp, x, _) = string_ x in
              x
        ) v2 |> List.flatten
        in
      Literal (String (Double (l, v1 @ v2, r)))

  | `Regex (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> literal_contents x
        | None -> [])
      in
      let _v3 = token2 v3 in
      Literal (Regexp ((v2, "??"), v1))
  | `Lambda (v1, v2, v3) ->
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
  | `Meth (v1, v2) ->
      let v1 = token2 v1 in
      let (n, params, body_exn) = method_rest v2 in
      D (MethodDef (v1, M n, params, body_exn))
  | `Sing_meth (v1, v2, v3, v4) ->
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
  | `Class (v1, v2, v3, v4, v5) ->
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
  | `Sing_class (v1, v2, v3, v4, v5) ->
      let v1 = token2 v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      let _v4 = terminator v4 in
      let (v5, _tend) = body_statement v5 in
      D (ClassDef (v1, SingletonC (v2, v3), v5))
  | `Module (v1, v2, v3) ->
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
  | `Begin (v1, v2, v3) ->
      let tbegin = token2 v1 in
      let _v2 =
        (match v2 with
        | Some x -> terminator x
        | None -> ())
      in
      let (v3, tend) = body_statement v3 in
      S (Block (tbegin, [S (ExnBlock (v3))], tend))
  | `While (v1, v2, v3, v4, v5) ->
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
  | `Until (v1, v2, v3, v4, v5) ->
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
  | `If (v1, v2, v3, v4, v5) ->
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
  | `Unless (v1, v2, v3, v4, v5) ->
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
  | `For (v1, v2, v3, v4, v5, v6) ->
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
  | `Case (v1, v2, v3, v4, v5, v6, v7) ->
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
  | `Ret (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Return (v1, v2))
  | `Yield (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Yield (v1, v2))
  | `Brk (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Break (v1, v2))
  | `Next (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Next (v1, v2))
  | `Redo (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Redo (v1, v2))
  | `Retry (v1, v2) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> argument_list x |> G.unbracket
        | None -> [])
      in
      S (Retry (v1, v2))
  | `Paren_un (v1, v2) ->
      let v1 =
        (match v1 with
        | `Defi tok -> Op_DefinedQuestion, token2 tok
        | `Not tok -> Op_UNot, token2 tok
        )
      in
      let (lp, v2, rp) = parenthesized_statements v2 in
      let block = S (Block (lp, v2, rp)) in
      Unary (v1, block)
  | `Un_lit (v1, v2) ->
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
  | `Here_begin tok ->
    let x = str tok in
    Literal (String (Single x))
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
    | `Prim_imm_tok_COLONCOLON (v1, v2) ->
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
  | `Choice_var_cmd_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = command_argument_list v2 in
      Call (v1, v2, None)
  | `Choice_var_cmd_arg_list_blk (v1, v2, v3) ->
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
  | `Choice_var_cmd_arg_list_do_blk (v1, v2, v3) ->
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
  | `Choice_var_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = argument_list v2 |> G.unbracket in
      Call (v1, v2, None)
  | `Choice_var_arg_list_blk (v1, v2, v3) ->
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
  | `Choice_var_arg_list_do_blk (v1, v2, v3) ->
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
  | `Choice_var_blk (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> Id (variable x)
        | `Scope_resol x -> ScopedId (scope_resolution x)
        | `Call x -> call x
        )
      in
      let v2 = block v2 in
      Call (v1, [], Some v2)
  | `Choice_var_do_blk (v1, v2) ->
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
  | `Arg_rep_COMMA_arg (v1, v2) ->
      let v1 = argument v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let _t = token2 v1 in
          let v2 = argument v2 in
          v2
        ) v2
      in
      v1::v2
  | `Cmd_call x -> [command_call x]
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
  | `Arg x -> arg x
  | `Splat_arg x -> splat_argument x
  | `Hash_splat_arg x -> hash_splat_argument x
  | `Blk_arg (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      Unary ((Op_UAmper, v1), v2)
  | `Pair x -> pair x
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
  | `Arg_and_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kAND, v2), v3)
  | `Arg_or_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kOR, v2), v3)
  | `Arg_BARBAR_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_OR, v2), v3)
  | `Arg_AMPAMP_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_AND, v2), v3)
  | `Arg_choice_LTLT_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> B Op_LSHIFT, token2 tok
        | `GTGT tok -> B Op_RSHIFT, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Arg_choice_LT_arg (v1, v2, v3) ->
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
  | `Arg_AMP_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kAND, v2), v3)
  | `Arg_choice_HAT_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `HAT tok -> B Op_XOR, token2 tok
        | `BAR tok -> B Op_BOR, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Arg_choice_PLUS_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> B Op_PLUS, token2 tok
        | `Bin_minus tok -> B Op_MINUS, token2 tok
        )
      in
      let v3 = arg v3 in
      Binop (v1, v2, v3)
  | `Arg_choice_SLASH_arg (v1, v2, v3) ->
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
  | `Arg_choice_EQEQ_arg (v1, v2, v3) ->
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
  | `Arg_STARSTAR_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in
      let v3 = arg v3 in
      Binop (v1, (Op_kAND, v2), v3)
  )

and unary (x : CST.unary) =
  (match x with
  | `Defi_arg (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      Unary ((Op_DefinedQuestion, v1), v2)
  | `Not_arg (v1, v2) ->
      let v1 = token2 v1 in
      let v2 = arg v2 in
      Unary ((Op_UNot, v1), v2)
  | `Choice_un_minus_arg (v1, v2) ->
      let v1 =
        (match v1 with
        | `Un_minus tok -> U Op_UMinus, token2 tok
        | `PLUS tok -> U Op_UPlus, token2 tok
        )
      in
      let v2 = arg v2 in
      Unary (v1, v2)
  | `Choice_BANG_arg (v1, v2) ->
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
      let v4 = token2 v4 in
      let e = DotAccess (v1, v2, MethodOperator (Op_AREF, v4)) in
      Call (e, v3, None)
  | `Call x -> call x
  | `Meth_call x -> method_call x
  )

and method_name (x : CST.method_name) : AST.method_name =
  (match x with
  | `Id tok -> MethodId (str tok, ID_Lowercase)
  | `Cst tok -> MethodId (str tok, ID_Uppercase)
  | `Setter (v1, v2) ->
      let v1 = str v1 in
      let v2 = token2 v2 in
      MethodIdAssign (v1, v2, ID_Lowercase)
  | `Symb x -> MethodAtom (symbol x)
  | `Op x -> let op = operator x in
        (match op with
        | Left bin, t -> MethodOperator (bin ,t)
        | Right un, t -> MethodUOperator (un, t)
        )
  | `Inst_var tok -> MethodId (str tok, ID_Instance)
  | `Class_var tok -> MethodId (str tok, ID_Class)
  | `Global_var tok -> MethodId (str tok, ID_Global)
  )

and interpolation ((v1, v2, v3) : CST.interpolation) : AST.expr AST.bracket =
  let lb = token2 v1 in
  let v2 = statement v2 in
  let rb = token2 v3 in
  (lb, v2, rb)

and string_ ((v1, v2, v3) : CST.string_) : AST.interp list bracket =
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
  | `Simple_symb tok -> let x = str tok in
        AtomSimple x
  | `Symb_start_opt_lit_content_str_end (v1, v2, v3) ->
      let v1 = token2 v1 in
      let v2 =
        (match v2 with
        | Some x -> literal_contents x
        | None -> [])
      in
      let v3 = token2 v3 in
      AtomFromString (v1, v2, v3)
  )

and literal_contents (xs : CST.literal_contents) : AST.interp list =
  List.map (fun x ->
    (match x with
    | `Str_content tok ->
            let x = str tok in
            StrChars x
    | `Interp x ->
            let (_lb, e, _rb) = interpolation x in
            StrExpr e
    | `Esc_seq tok ->
            let x = str tok in
            StrChars x
    )
  ) xs

and pair (x : CST.pair) =
  (match x with
  | `Arg_EQGT_arg (v1, v2, v3) ->
      let v1 = arg v1 in
      let v2 = token2 v2 in  (* => *)
      let v3 = arg v3 in
      Binop(v1, (Op_ASSOC, v2), v3)
  | `Choice_id_hash_key_imm_tok_COLON_arg (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id_hash_key tok -> Id (str tok, ID_Lowercase)
        | `Id tok -> Id (str tok, ID_Lowercase)
        | `Cst tok -> Id (str tok, ID_Uppercase)
        | `Str x ->  Literal (String (Double (string_ x)))
        )
      in
      let v2 = token2 v2 in (* : *)
      let v3 = arg v3 in
      (match v1 with
      | Id (x, _) -> AST.keyword_arg_to_expr x v2 v3
      | _ -> Binop(v1, (Op_ASSOC, v2), v3)
      )
  )

let program ((v1, _v2interpreted) : CST.program) : AST.stmts  =
  match v1 with
  | Some x -> statements x
  | None -> []


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
 H.convert_tree_sitter_exn_to_pfff_exn (fun () ->
  (* TODO: tree-sitter bindings are buggy so we cheat and fork to
   * avoid segfaults to popup. See Main.ml test_parse_ruby function.
   *)
   let cst =
      match 2 with
      (* segfault quite often *)
      | 1 -> Tree_sitter_ruby.Parse.file file
      (* segfault less, but as we fork from a more complex point where
       * we allocated quite a few stuff, the probability to get a segfault
       * in the child grows
       *)
      | _ ->
         Parallel.backtrace_when_exn := false;
         Parallel.invoke Tree_sitter_ruby.Parse.file file ()
   in
   (*
   let sexp = CST.sexp_of_program x in
   let s = Sexplib.Sexp.to_string_hum sexp in
   pr s;
  *)
   global_file := file;
   global_conv := H.line_col_to_pos file;
   program cst
 )

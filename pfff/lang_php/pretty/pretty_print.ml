(* Julien Verlaguet
 *
 * Copyright (C) 2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Ast_pp

module A = Ast_pp
module Pp = Pretty_print_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A pretty printer for PHP. The goal is to replace many of bill/jfrank's
 * code reviews by a script :)
 * The current conventions are documented here:
 *  https://tools.facebook.com/dex/core-php/php-style-reference
 *
 * The general idea of the algorihm is to use backtracking.
 * You try to print something and if it fails you try a different strategy.
 * See for instance the use of Pp.fail() below.
 *
 * If you want to look at a quite elaborated example of pretty printing,
 * check the code for the assignement (Expr (Assign...
 *
 * Why not using just use a box model a la Format?
 * Because many facebook conventions wouldn't fit in the box model.
 * The backtracking model costs more at runtime, but you can write any rule
 * in it, as complex as you want.
 *
 * notes:
 *  - we introduce a space after if/while/catch/...
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec escape_quotes buf s i =
  if i >= String.length s
  then Buffer.contents buf
  else match s.[i] with
  | '\\' -> Buffer.add_char buf '\\'; esc_mode buf s (i+1)
  | '\'' -> Buffer.add_string buf "\\'"; escape_quotes buf s (i+1)
  | c -> Buffer.add_char buf c; escape_quotes buf s (i+1)

and esc_mode buf s i =
  if i >= String.length s
  then (Buffer.add_char buf '\\'; Buffer.contents buf)
  else (Buffer.add_char buf s.[i]; escape_quotes buf s (i+1))

let escape_quotes s =
  escape_quotes (Buffer.create 256) s 0

(*****************************************************************************)
(* Priority *)
(*****************************************************************************)

(* The AST does not contain parenthesis. So when pretty-printing a tree,
 * some expressions may need some extra parenthesis around them, otherwise
 * parsing the pretty-printed code back could generate a different tree.
 * We want parse(pretty-print(tree)) = tree.
 *)

type assoc =
  | Left
  | Right
  | NonAssoc

let rec binaryOp = function
  | Cst_php.Arith aop    -> arithOp aop
  | Cst_php.Logical lop  -> logicalOp lop
  | Cst_php.BinaryConcat -> 17 , Left
  | Cst_php.CombinedComparison -> 14, NonAssoc

and arithOp = function
  | Cst_php.Plus
  | Cst_php.Minus        -> 17 , Left
  | Cst_php.Mul
  | Cst_php.Div
  | Cst_php.Mod          -> 18 , Left
  | Cst_php.DecLeft
  | Cst_php.DecRight     -> 16 , Left
  | Cst_php.And          -> 13 , Left
  | Cst_php.Or           -> 11 , Left
  | Cst_php.Xor          -> 12 , Left

and logicalOp = function
  | Cst_php.Inf
  | Cst_php.Sup
  | Cst_php.InfEq
  | Cst_php.SupEq        -> 15 , NonAssoc
  | Cst_php.Eq
  | Cst_php.NotEq
  | Cst_php.Identical
  | Cst_php.NotIdentical -> 14 , NonAssoc
  | Cst_php.AndLog       -> 5  , Left
  | Cst_php.OrLog        -> 3  , Left
  | Cst_php.XorLog       -> 4  , Left
  | Cst_php.AndBool      -> 10 , Left
  | Cst_php.OrBool       -> 9  , Left

and unaryOp = function
  | Cst_php.UnPlus       -> 21 , Right
  | Cst_php.UnMinus      -> 21 , Right
  | Cst_php.UnBang       -> 19 , Right
  | Cst_php.UnTilde      -> 21 , Right

let expr_priority = function
  | A.Int _ | A.Double _ | A.String _ | A.Id _
  | A.Obj_get _ | A.Class_get _ | A.Call _
  | A.Xhp _ | A.ConsArray _ | A.Collection _
  | A.Guil _ | A.HereDoc _
  | A.List _
  | A.This
  | A.Lambda _
    -> -1, NonAssoc
  | A.Array_get _       -> 23, Right
  | A.Assign _          -> 7, Left
  | A.Binop (bop, _, _) -> binaryOp bop
  | A.Unop (uop, _)     -> unaryOp uop
  | A.Ref _             -> 13, Left
  | A.Unpack _          -> 13, Left
  | A.New _             -> 24, NonAssoc
  | A.InstanceOf _      -> 20, NonAssoc
  | A.CondExpr _        -> 8, Left
  | A.Cast _ | A.Infix _  | A.Postfix _  -> 21, Right

(*****************************************************************************)
(* String *)
(*****************************************************************************)

let rec binaryOp = function
  | Cst_php.Arith aop    -> arithOp aop
  | Cst_php.Logical lop  -> logicalOp lop
  | Cst_php.BinaryConcat -> "."
  | Cst_php.CombinedComparison -> "<=>"

and arithOp = function
  | Cst_php.Plus         -> "+"  | Cst_php.Minus        -> "-"
  | Cst_php.Mul          -> "*"  | Cst_php.Div          -> "/"
  | Cst_php.Mod          -> "%"
  | Cst_php.DecLeft      -> "<<" | Cst_php.DecRight     -> ">>"
  | Cst_php.And          -> "&"  | Cst_php.Or           -> "|"
  | Cst_php.Xor          -> "^"

and logicalOp = function
  | Cst_php.Inf          -> "<"   | Cst_php.Sup          -> ">"
  | Cst_php.InfEq        -> "<="  | Cst_php.SupEq        -> ">="
  | Cst_php.Eq           -> "=="  | Cst_php.NotEq        -> "!="
  | Cst_php.Identical    -> "===" | Cst_php.NotIdentical -> "!=="
  | Cst_php.AndLog       -> "AND" | Cst_php.OrLog        -> "OR"
  | Cst_php.AndBool      -> "&&"  | Cst_php.OrBool       -> "||"
  | Cst_php.XorLog       -> "XOR"

let unaryOp = function
  | Cst_php.UnPlus       -> ""
  | Cst_php.UnMinus      -> "-"
  | Cst_php.UnBang       -> "!"
  | Cst_php.UnTilde      -> "~"


let visibility _env = function
  | A.Novis        -> ""
  | A.Public       -> "public"
  | A.Private      -> "private"
  | A.Protected    -> "protected"
  | A.Abstract     -> "abstract"

let rec hint_type env = function
  | A.Hint s         -> s
  | A.HintArray      -> "array"
  | A.HintQuestion s -> "?" ^ (hint_type env s)
  | A.HintTuple l ->
    let elts = String.concat ", " (List.map (hint_type env) l) in
    "(" ^ elts ^ ")"
  | A.HintCallback (args, ret) ->
      let args = List.map (hint_type env) args in
      let args = "(" ^ (String.concat ", " args) ^ ")" in
      let ret  = match ret with
                  | Some t -> ": " ^ (hint_type env t)
                  | None -> "" in
      Printf.sprintf "(function%s%s)" args ret
  | A.HintVariadic None -> ""
  | A.HintVariadic (Some hint) -> Printf.sprintf "%s" (hint_type env hint)

let ptype = function
  | Cst_php.BoolTy   -> "bool"
  | Cst_php.IntTy    -> "int"
  | Cst_php.DoubleTy -> "double"
  | Cst_php.StringTy -> "string"
  | Cst_php.ArrayTy  -> "array"
  | Cst_php.ObjectTy -> "object"

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* note: the leading <?php should have been converted in a Comment by
 * ast_pp_build so no need to Pp.print "<?php" here anymore
 *)
let rec program o tpl =
  let env = Pp.empty o in
  program_env env tpl

and program_env env tpl =
  toplevel env tpl



and toplevel env = function
  | [] -> ()
  | x :: rl -> stmt env x; toplevel env rl

and stmt_block env stl =
  Pp.nest_block env (fun env ->
      stmts env stl;
  )

and stmt_block_nl env stl =
  Pp.nest_block env (fun env ->
      stmts env stl;
  );
  Pp.newline env


and stmt env st =
  match st with
  | StmtEsthet Newline -> Pp.newline env
  (* pad: ?? *)
  | StmtEsthet (Comment "") -> ()
  | _ ->
      Pp.spaces env;
      stmt_ env st

and stmt_ env = function
  | StmtEsthet Newline -> raise Common.Impossible
  | StmtEsthet (Comment s) ->
      Pp.print env s;
      Pp.newline env
  | Global el ->
      Pp.print env "global";
      Pp.print env " ";
      Pp.simpl_list env expr ", " el;
      Pp.print env ";";
      Pp.newline env
  | Noop -> ()
  | Block stl ->
      stmt_block env stl
  | If (e, st1, st2) ->
      Pp.print env "if (";
      Pp.nestc env (
        fun env ->
          expr env e;
      );
      Pp.print env ") ";
      stmt_ env st1;
      (match st2 with
      | Noop -> Pp.newline env
      | st2 ->
          Pp.print env " else ";
          stmt_ env st2;
          (match st2 with
          | If _ -> Pp.newline_opt env
          | _ -> ())
      );
      Pp.newline_opt env

  | FuncDef fd ->
      func_def env fd
  | ClassDef c -> class_def env c
  | ConstantDef c ->
      Pp.print env "const ";
      Pp.print env c.cst_name;
      (* todo? what if we reach 80 col after the =?
       * we will cut but then we will have a trailing space :(
       *)
      (match c.cst_body with
      | None -> ()
      | Some b -> Pp.print env " = "; expr env b);
      Pp.print env ";";
      Pp.newline env

  | While (e, stl) ->
      Pp.print env "while (";
      expr env e;
      Pp.print env ") ";
      stmt_block_nl env stl
  | Do (stl, e) ->
      Pp.print env "do ";
      stmt_block env stl;
      Pp.print env " while(";
      expr env e;
      Pp.print env ");";
      Pp.newline env;
  | For (el1, el2, el3, st) ->
      Pp.print env "for (";
      Pp.simpl_list env expr ", " el1;
      Pp.print env "; ";
      Pp.simpl_list env expr ", " el2;
      Pp.print env "; ";
      Pp.simpl_list env expr ", " el3;
      Pp.print env ") ";
      stmt_block_nl env st
  | Switch (e, cl) ->
      Pp.print env "switch (";
      expr env e;
      Pp.print env ") ";
      Pp.nest_block_nl env (
        fun env ->
          List.iter (case env) cl;
      )
  | Foreach (e1, e2, e3, stl) ->
      Pp.print env "foreach (";
      expr env e1;
      Pp.print env " as ";
      expr env e2;
      (match e3 with
      | None -> ()
      | Some e ->
          Pp.print env " => ";
          expr env e);
      Pp.print env ") ";
      stmt_block_nl env stl
  | Try (stl, cl, fl) ->
      Pp.print env "try ";
      stmt_block env stl;
      List.iter (catch env) (cl);
      List.iter (finally env) (fl);
      Pp.newline env
  | InlineHtml s -> Pp.print env s
  | Expr (Assign (bop, e1, e2)) ->
      expr env e1;
      assignOp env bop;
      (match e2 with
      | Xhp _ ->
        Pp.choice_left env (fun env ->
          let line = env.Pp.line in
          Pp.print env " ";
          expr env e2;
          if env.Pp.line <> line then Pp.fail();
       ) (fun env ->
          Pp.newline env;
          Pp.nest env (fun env ->
            Pp.spaces env;
            expr env e2
         ))

      | _ ->
          Pp.choice_right env (fun env ->
            Pp.try_hard env (fun env ->
              Pp.print env " ";
              expr env e2;
                            )
         ) (fun env ->
            Pp.newline env;
            Pp.nest env (fun env ->
              Pp.spaces env;
              expr env e2;
           )
         );
      );
      Pp.print env ";";
      Pp.newline env
  | StaticVars el ->
      Pp.print env "static";
      Pp.print env " ";
      Pp.simpl_list env (
        fun env (s, eopt) ->
          match eopt with
          | None -> Pp.print env s
          | Some e ->
              expr env (Assign (None, Id s, e))
      ) ", " el;
      Pp.print env ";";
      Pp.newline env
  | Break _ | Continue _ | Throw _
  | Return _ as e ->
      Pp.nest env (fun env ->
          stmt_simple env e;
          Pp.print env ";";
          Pp.newline env
      )
  | Expr _ as e ->
      stmt_simple env e;
      Pp.print env ";";
      Pp.newline env

and assignOp env = function
  | None -> Pp.print env " =";
  | Some x ->
      Pp.print env " ";
      Pp.print env (binaryOp x);
      Pp.print env "=";

and case env = function
  | CaseEsthet (Newline) -> Pp.newline env
  | CaseEsthet (Comment s) -> Pp.print env s; Pp.newline env
  | Case (e, stl) ->
      Pp.spaces env;
      Pp.print env "case ";
      expr env e;
      Pp.print env ":";
      Pp.newline env;
      Pp.nest env (fun env ->
          stmts env stl;
      );
  | Default stl ->
      Pp.spaces env;
      Pp.print env "default:";
      Pp.newline env;
      Pp.nest env (fun env ->
          stmts env stl;
      );

and catch env (c, v, stl) =
  Pp.print env " catch (";
  Pp.print env (hint_type env c);
  Pp.print env " ";
  Pp.print env v;
  Pp.print env ") ";
  stmt_block env stl

and finally env (stl) =
  Pp.print env " finally ";
  stmt_block env stl

and stmt_simple env = function
  | Expr e -> expr env e;
  | Return None ->
      Pp.print env "return";
  | Return (Some e) ->
      Pp.print env "return ";
      expr env e;
  | Break e ->
      Pp.print env "break";
      (match e with
      | None -> ()
      | Some e ->
          Pp.print env " ";
          expr env e;
      );
  | Continue e ->
      Pp.print env "continue";
      (match e with
      | None -> ()
      | Some e ->
          Pp.print env " ";
          expr env e;
      );
  | Throw e ->
      Pp.print env "throw ";
      expr env e
  | _ -> raise Common.Impossible

and obj_get_flat e env =
  match e with
  | Call (Obj_get _ as e, el) ->
      obj_get_flat e env;
      Pp.list env expr "(" el "," ")";
  | Obj_get (e, n) ->
      obj_get_flat e env;
      Pp.print env "->";
      expr env n
  | e -> expr env e

and obj_get_nest e env =
  match e with
  | Call (Obj_get _ as e, el) ->
      obj_get_nest e env;
      Pp.list env expr "(" el "," ")";
  | Obj_get (e, n) ->
      obj_get_nest e env;
      Pp.newline env;
      Pp.spaces env;
      Pp.print env "->";
      expr env n;
  | e -> expr env e

and expr env e =
  let prio, _ = expr_priority e in
  Pp.paren prio env (
    fun env -> expr_ env e
  )

and expr_ env = function
  | Int x
  | Double x ->
      Pp.print env x
  | String x ->
      let x = escape_quotes x in
      Pp.string "'" " ." env x;
  | Guil x ->
      Pp.print env "\"";
      encaps_list env x;
      Pp.print env "\"";
  | HereDoc (x, el, y) ->
      Pp.print env x;
      encaps_list env el;
      Pp.print env y;
  | Id x -> Pp.print env x
  | This -> Pp.print env "$this"
  | Array_get (e, eopt) ->
      expr env e;
      Pp.print env "[";
      (match eopt with
      | None -> ()
      | Some e -> expr env e);
      Pp.print env "]"
  | Obj_get _ as e ->
      Pp.choice_left env
        (obj_get_flat e)
        (obj_get_nest e)
  | Class_get (e, n) ->
      expr env e;
      Pp.print env "::";
      expr env n
  | Assign (bop, e1, e2) ->
      expr env e1;
      assignOp env bop;
      Pp.print env " ";
      expr env e2
  | Binop (Cst_php.BinaryConcat, e1, e2) ->
      Pp.nestc env (fun env ->
      Pp.choice_left env (
        fun env ->
          expr env e1;
          Pp.print env " . ";
          expr env e2;
      ) (
        fun env ->
          expr env e1;
          Pp.print env " .";
          Pp.newline env;
          Pp.spaces env;
          expr env e2;
      ))
  | Binop (bop, e1, e2) ->
      Pp.nestc env (fun env ->
      Pp.choice_left env (
        fun env ->
          expr env e1;
          Pp.print env " ";
          Pp.print env (binaryOp bop);
          Pp.print env " ";
          expr env e2;
          if env.Pretty_print_code.cmargin >= 75 then raise Pp.Fail;
      ) (
        fun env ->
          expr env e1;
          Pp.print env " ";
          Pp.print env (binaryOp bop);
          Pp.newline env;
          Pp.spaces env;
          expr env e2;
      ))
  | Unop (uop, e) ->
      Pp.print env (unaryOp uop);
      expr env e
  | Call (Id ("yield"), [Call (Id s2, el)]) ->
      Pp.choice_right env (fun env ->
        Pp.print env "yield ";
        Pp.print env s2;
        let line = env.Pp.line in
        Pp.flat_list env expr "(" el "," ")";
        if env.Pp.line <> line then Pp.fail();
      ) (fun env ->
        Pp.print env "yield ";
        Pp.print env s2;
        Pp.print env "(";
        Pp.fun_args env expr "" el "," "";
        Pp.spaces env;
        Pp.print env ")";
      )

  | Call (Id ("yield"), _) ->
      failwith "weird yield"

  | Call (f, el) ->
      expr env f;
      Pp.fun_args env expr "(" el "," ")";
  | Ref e ->
      Pp.print env "&";
      expr env e
  | Unpack e ->
      Pp.print env "...";
      expr env e
  | Xhp x ->
      Pp.nestc env (fun env ->
          xml env x;
      )
  | ConsArray ((_ :: _ :: _) as avl) when A.is_string_key avl ->
      let size = A.key_length avl in
      Pp.print env "array";
      Pp.nl_nested_list env (array_value size) "(" avl "," ")"
  | ConsArray avl ->
      Pp.print env "array";
      Pp.list env (array_value 0) "(" avl "," ")";
  | Collection (s, vel) ->
      let size = A.key_length vel in
      Pp.print env s;
      Pp.nl_nested_list env (array_value size) "{" vel "," "}"
  | List el ->
      Pp.print env "list";
      Pp.list env expr "(" el "," ")";
  | New (s, el) ->
      Pp.print env "new ";
      expr env s;
      Pp.list env expr "(" el "," ")";
  | InstanceOf (e, s) ->
      expr env e;
      Pp.print env " instanceof ";
      expr env s;
  | CondExpr (e1, e2, e3) ->
      Pp.choice_left env (
        fun env ->
          expr env e1;
          Pp.print env " ? ";
          expr env e2;
          Pp.print env " : ";
          expr env e3
      ) (
        fun env ->
          Pp.nestc env (fun env ->
              expr env e1;
              Pp.print env " ?";
              Pp.newline env;
              Pp.spaces env;
              expr env e2;
              Pp.newline env;
              Pp.spaces env;
              Pp.print env ": ";
              expr env e3;
              Pp.newline env;
              Pp.spaces env;
          )
      )
  | Cast (pty, e) ->
      Pp.print env "(";
      Pp.print env (ptype pty);
      (* some code put space after cast operation, som don't *)
      Pp.print env ") ";
      expr env e;
  | Infix (fop, e) ->
      fixop env fop;
      expr env e
  | Postfix (fop, e) ->
      expr env e;
      fixop env fop
  (* similar to func_def *)
  | Lambda def ->
      Pp.print env "function";
      Pp.nest env (fun env ->
        Pp.choice_left env (fun env ->
          Pp.print env "(";
          parameters_ ~nl:false env def.l_params;
          Pp.print env ")";
        ) (fun env ->
          Pp.print env "(";
          parameters_ ~nl:true env def.l_params;
          Pp.print env ")";
        )
      );
      (match def.l_use with
      | [] -> ()
      | _x::_xs ->
          Pp.nest env (fun env ->
            Pp.choice_left env (fun env ->
              Pp.print env " use(";
              parameters_ ~nl:false env def.l_use;
              Pp.print env ")";
            ) (fun env ->
              Pp.print env " use(";
              parameters_ ~nl:true env def.l_use;
              Pp.print env ")";
            )
          );
      );
      Pp.print env " {";
      Pp.newline env;
      Pp.nest env (fun env ->
        List.iter (stmt env) def.l_body;
      );
      Pp.spaces env;
      Pp.print env "}";
      ()


and fixop env = function
  | AST_generic.Incr -> Pp.print env "++"
  | AST_generic.Decr -> Pp.print env "--"

and array_value pad env = function
  | Aval e ->
      expr env e
  | Akval (String s as e, e2) ->
      Pp.nest env (
        fun env ->
          expr env e;
          for _i = 1 to pad - String.length s do
            Pp.print env " ";
          done;
          Pp.print env " =>";
          Pp.choice_left env (
            fun env ->
              Pp.print env " ";
              expr env e2;
          ) (
            fun env ->
              Pp.newline env;
              Pp.spaces env;
              expr env e2
          )
      )
  | Akval (e1, e2) ->
      expr env e1;
      Pp.print env " => ";
      expr env e2

and class_def env c =
  (match c.c_type with
  | ClassRegular -> Pp.print env "class "
  | ClassFinal -> Pp.print env "final class"
  | ClassAbstract -> Pp.print env "abstract class"
  | ClassAbstractFinal -> Pp.print env "abstract final class"
  | Interface -> Pp.print env "interface "
  | Trait -> Pp.print env "trait "
  );

  Pp.print env c.c_name;
  (match c.c_extends with
  | [] -> ()
  | _ ->
      Pp.print env " extends ";
      Pp.list env (fun env x -> Pp.print env (hint_type env x))
        "" c.c_extends " " "";
  );
  (match c.c_implements with
  | [] -> Pp.print env " "
  | _ ->
      (* ugly *)
      (match c.c_type with
      | Interface -> Pp.print env " extends "
      | _ -> Pp.print env " implements ";
      );
      interfaces env c.c_implements;
      Pp.print env " ";
  );
  Pp.nest_block env (fun env -> List.iter (class_element env) c.c_body);
  Pp.newline env

and class_element env = function
  | CEEsthet (Newline) -> Pp.newline env
  (* pad: ?? *)
  | CEEsthet (Comment "") -> ()
  | CEEsthet (Comment s) ->
      Pp.spaces env;
      Pp.print env s;
      Pp.newline env
  | CEconst (is_abs, l) -> class_const env is_abs l
  | CEdef cvd -> class_variables env cvd
  | CEmethod md -> method_def env md

and class_const env is_abs l =
  match l with [] -> () | _ ->
    Pp.spaces env;
    if is_abs then Pp.print env "abstract const"
    else Pp.print env "const";
    Pp.newline env;
    Pp.nest env (
    fun env ->
      class_constants env l;
      Pp.print env ";";
      Pp.newline env;
      Pp.newline env;
   )

and interfaces env = function
  | [] -> ()
  | [x] -> Pp.print env (hint_type env x)
  | x :: rl ->
    Pp.print env (hint_type env x);
    Pp.print env ", ";
    interfaces env rl

and class_constants env = function
  | [] -> ()
  | [ {cst_name = x; cst_body = v_opt}] ->
      Pp.spaces env;
      Pp.print env x;
      (match v_opt with
        | Some v -> (Pp.print env " = "; expr env v)
        | None -> (* abstract const has no value *) ())
  | {cst_name = x; cst_body = v_opt} :: rl ->
      Pp.spaces env;
      Pp.print env x;
      (match v_opt with
        | Some v -> (Pp.print env " = "; expr env v)
        | None -> (* abstract const has no value *) ());
      Pp.print env ",";
      Pp.newline env;
      class_constants env rl

and class_variables env cv =
  Pp.spaces env;
  if cv.cv_abstract
  then Pp.print env "abstract ";
  if cv.cv_visibility <> Novis
  then begin
    Pp.print env (visibility env cv.cv_visibility);
    Pp.print env " ";
  end;
  if cv.cv_static
  then Pp.print env "static ";
  if cv.cv_final
  then Pp.print env "final ";
  (match cv.cv_type with
  | None -> ()
  | Some s ->
      Pp.print env (hint_type env s)
  );
  if not (has_modifier cv)
  then Pp.print env "var ";
  (match cv.cv_vars with
  | [(x, v)] ->
      Pp.print env x;
      (match v with
      | None -> ()
      | Some v ->
          Pp.print env " = ";
          expr env v)
  | _ ->
      Pp.newline env;
      Pp.nest env (
        fun env ->
          class_vars env cv.cv_vars;
      );
  );
  Pp.print env ";";
  Pp.newline env;


and class_var env x v =
  Pp.spaces env;
  Pp.print env x;
  (match v with
  | None -> ()
  | Some v ->
      Pp.print env " = ";
      expr env v)

and class_vars env = function
  | [] -> ()
  | [(x, v)] ->
      class_var env x v
  | (x, v) :: rl ->
      class_var env x v;
      Pp.print env ",";
      Pp.newline env;
      class_vars env rl

and method_modifiers env m =
  if m.m_visibility <> Novis
  then begin
    Pp.print env (visibility env m.m_visibility);
    Pp.print env " ";
  end;
  if m.m_final
  then Pp.print env "final ";
  if m.m_static
  then Pp.print env "static ";
  if m.m_abstract
  then Pp.print env "abstract "
  else ()

and method_def env m =
  (* TODO return type *)
  Pp.spaces env;
  method_modifiers env m;
  Pp.print env "function ";
  if m.m_ref
  then Pp.print env "&";
  Pp.print env m.m_name;
  Pp.nest env (fun env ->
    Pp.choice_left env (
    fun env ->
      Pp.print env "(";
      parameters_ ~nl:false env m.m_params;
      Pp.print env ")";
      if m.m_body = []
      then Pp.print env ";"
      else Pp.print env " {"
   ) (
    fun env ->
      Pp.print env "(";
      (* 4 spaces for parameters *)
      Pp.nest env (fun env ->
        parameters_ ~nl:true env m.m_params;
      );
      Pp.print env ")";
      if m.m_body = []
      then Pp.print env ";"
      else Pp.print env " {"
   );
    Pp.newline env;
    List.iter (stmt env) m.m_body;
   );
  if m.m_body = []
  then ()
  else (Pp.spaces env; Pp.print env "}"; Pp.newline env)

and parameters_ ~nl env = function
  | [] -> ()
  | [x] ->
      if nl then (Pp.newline env; Pp.spaces env);
      parameter env x
  | x :: rl ->
      if nl
      then begin
        Pp.newline env;
        Pp.spaces env;
        parameter env x;
        Pp.print env ",";
        parameters_ nl env rl
      end
      else begin
        parameter env x;
        Pp.print env ", ";
        parameters_ nl env rl
      end

and parameter env p =
  (match p.p_type with
  | None -> ()
  | Some s ->
      Pp.print env (hint_type env s);
      Pp.print env " ";
  );
  if p.p_ref
  then Pp.print env "&";
  if p.p_variadic
  then Pp.print env "...";
  Pp.print env p.p_name;
  (match p.p_default with
  | None -> ()
  | Some e ->
      Pp.print env " = ";
      expr env e);

and func_def env f =
  Pp.spaces env;
  Pp.print env "function ";
  if f.f_ref
  then Pp.print env "&";
  Pp.print env f.f_name;
  Pp.nest env (fun env ->
    Pp.choice_left env (
    fun env ->
      Pp.print env "(";
      parameters_ ~nl:false env f.f_params;
      Pp.print env ") {";
   ) (
    fun env ->
      Pp.print env "(";
      (* We actually want 4 spaces for parameters, so we add another
       * nest() here in addition to the one above.
       *)
      Pp.nest env (fun env ->
        parameters_ ~nl:true env f.f_params;
      );
      Pp.print env ") {";
   )
  );
  (* TODO f_return_type *)
  Pp.newline env;
  Pp.nest env (fun env ->
    List.iter (stmt env) f.f_body;
  );
  Pp.spaces env;
  Pp.print env "}";
  Pp.newline env;
  ()

and xhp env = function
  | XhpText s when Pp.is_only_space s 0 -> ()
  | XhpText s -> Pp.print_text env s
  | XhpExpr e -> Pp.print env "{"; expr env e; Pp.print env "}"
  | XhpXml x -> xml env x

and xhp_attr env = function
  | AttrString el ->
    Pp.print env "\"";
      Pp.choice_left env (
        fun env ->
          List.iter (encaps env) el;
      ) (
        fun env ->
          xhp_attr env (AttrExpr (Guil el))
      );
    Pp.print env "\"";

  | AttrExpr e ->
      Pp.nestc env (
        fun env ->
          Pp.print env "{";
          Pp.nestc env (
            fun env ->
              expr env e;
          );
          Pp.print env "}"
      )

and xml env x =
  match x.xml_body with
  | [] -> xml_simpl env x
  | _ -> xml_nest env x

and xml_simpl env x =
  open_tag "/" env x;

and xml_nest env x =
  let body = List.filter (
    function XhpText s when Pp.is_only_space s 0 -> false | _ -> true
  ) x.xml_body
  in
  Pp.choice_left env (
    fun env ->
      open_tag "" env x;
      if List.length body <> 1 then raise Pp.Fail;
      (match body with
      | [XhpXml _] -> raise Pp.Fail | _ -> ());
      List.iter (xhp env) body;
      close_tag env x;
  ) (
    fun env ->
      open_tag "" env x;
      Pp.newline env;
      Pp.nest env (fun env ->
          List.iter (fun x -> Pp.spaces env; xhp env x; Pp.newline env) body;
      );
      Pp.spaces env;
      close_tag env x;
  )

and open_tag last env x =
  Pp.print env "<";
  tag_name env x.xml_tag;
  Pp.list_left env (
  fun env (x, v) ->
    Pp.print env x;
    Pp.print env "=";
    xhp_attr env v;
 ) "" x.xml_attrs "" "" ;
  if last <> ""
  then Pp.print env " ";
  if env.Pp.last_nl
  then Pp.spaces env;
  Pp.print env last;
  Pp.print env ">"

and close_tag env x =
  Pp.print env "</";
  tag_name env x.xml_tag;
  Pp.print env ">";

and tag_name env l =
  Pp.simpl_list env Pp.print ":" l

and encaps_list env = function
  | [] -> ()
  | x :: rl ->
      encaps env x;
      encaps_list env rl

and encaps env = function
  | EncapsString s -> Pp.print env s
  | EncapsVar x -> expr env x
  | EncapsCurly x -> Pp.print env "{"; expr env x; Pp.print env "}"
  | EncapsDollarCurly _ -> failwith "TODO EncapsDollarCurly"
  | EncapsExpr _ -> failwith "EncapsExpr"(* of expr *)


(*****************************************************************************)
(* Main entry points for spatch *)
(*****************************************************************************)

(* pad: used by the mix unparser/pretty-printer *)
and class_elements env l =
  Pp.nest env (fun env ->
    List.iter (class_element env) l
  )

and stmts env l =
  List.iter (stmt env) l

and class_footer env () =
  Pp.print env "}";
  Pp.newline env;
  ()

and class_header env xs =
  List.iter (fun x ->
    match x with
    (* copy paste of class_def but without printing the body nor the ending
     * brace
     *)
    | ClassDef c ->

        (match c.c_type with
        | ClassRegular -> Pp.print env "class "
        | ClassFinal -> Pp.print env "final "
        | ClassAbstract -> Pp.print env "abstract "
        | ClassAbstractFinal -> Pp.print env "abstract final "
        | Interface -> Pp.print env "interface "
        | Trait -> Pp.print env "trait "
        );

        Pp.print env c.c_name;
        (match c.c_extends with
        | [] -> ()
        | _ ->
            Pp.print env " extends ";
            Pp.list env (fun env s ->
              Pp.print env (hint_type env s))
              "" c.c_extends " " "";
        );
        (match c.c_implements with
        | [] -> Pp.print env " "
        | _ ->

            (* ugly *)
            (match c.c_type with
            | Interface -> Pp.print env " extends "
            | _ -> Pp.print env " implements ";
            );
            interfaces env c.c_implements;
            Pp.print env " ";
        );
        (* diff with copy paste *)
        Pp.print env "{";
        Pp.newline env;
        (* old:
        Pp.nest_block env (fun env -> List.iter (class_element env) c.c_body);
        Pp.newline env
        *)

    | _ -> stmt env x
  ) xs

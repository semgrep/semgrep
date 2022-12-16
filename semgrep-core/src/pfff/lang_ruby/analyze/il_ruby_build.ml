open Common
open Printf
open Il_ruby
open Utils_ruby
module Utils = Utils_ruby
module Ast = Ast_ruby
open Il_ruby_helpers
module C = Il_ruby_helpers.Abbr
module PI = Parse_info
module Log = Ruby_log
module CodePrinter = Il_ruby

let tok_of _e =
  raise Todo

type 'a acc = {
  q : 'a DQueue.t;
  seen : StrSet.t;
  super_args : (star_expr list * expr option) option;
}

let fb = PI.fake_bracket

let acc_empty old =
  {q = DQueue.empty; seen = StrSet.empty;
   super_args = old.super_args}

let acc_enqueue stmt acc =
  let q = DQueue.enqueue stmt acc.q in
  {acc with q = q}

let acc_emptyq acc = {acc with q=DQueue.empty}

let acc_seen acc1 acc2 =
  {acc1 with seen = StrSet.union acc1.seen acc2.seen}

let add_seen x acc = {acc with seen = StrSet.add x acc.seen}

let acc_append acc1 acc2 =
  {q = DQueue.append acc1.q acc2.q;
   seen = StrSet.union acc1.seen acc2.seen;
   super_args = (assert(acc1.super_args == acc2.super_args);acc1.super_args);
  }

let rec seen_lhs acc (lhs:lhs) = match lhs with
  | LId (Var(_,str)) | LStar ((Var(_,str))) ->
      {acc with seen = StrSet.add str acc.seen}

  | LId (_) | LStar _ -> acc
  | LTup ls -> List.fold_left seen_lhs acc ls

let uniq_counter = ref 0
let uniq () = incr uniq_counter; !uniq_counter

let fresh acc =
  let fresh_name = "__tmp_" ^ (string_of_int (uniq ())) in
  let id = Var (Local, fresh_name) in
  (seen_lhs acc (LId id)), LId id

let fresh_global pos =
  let name = sprintf "$__druby_global_%d_%d"
      (Parse_info.line_of_info pos)
      (uniq())
  in
  Var(Global, name)

let formal_counter = ref 0
let fresh_formal () = incr formal_counter; !formal_counter

let _re_init () = uniq_counter := 0

let gen_super_args params : (star_expr list * expr option) option  =
  let work = function
    | Formal_default(s,_)
    | Formal_meth_id s -> SE (EId (Var(Local,s)))
    | Formal_amp _s -> assert false
    | Formal_star s -> SStar ((EId (Var(Local,s))))
  in
  match List.rev params with
  | (Formal_amp s)::rest ->
      let args = List.rev_map work rest in
      Some (args, Some (EId (Var(Local,s))))
  | lst  ->
      Some (List.rev_map work lst, None)

let make_call_expr acc targ msg (args: star_expr list) cb _pos : stmt acc * expr =
  let acc, lhs = fresh acc in
  let lhs' = match lhs with LId id -> id | _ -> failwith "Impossible" in
  let lhs'' = EId lhs' in
  let call = C.mcall ~lhs ?targ msg args ?cb () in
  (acc_enqueue call acc), lhs''

(* convert a single quoted string into a double quoated string.  We do
   three transformations:
   - substitute \' for just '
   - substitute \c for \\c where c is not another slash
     This step is broken into several subcases to try and keep an
     ounce of readability in the regexps
   - explicitly esacpe special characters such as # or {
*)
let unescape_single_string s =
  (* first unescaped any single quotes: \' becomes ' *)
  let s = Str.global_replace (Str.regexp "\\\\'") "'" s in
  (* ^\[^\] *)
  let start_re = Str.regexp "^\\\\\\([^\\\\]\\)" in
  let t = "\\\\\\\\\\1" in
  let s = Str.global_replace start_re t s in
  (* ([^\])\(^[\]) *)
  let no_prefix = Str.regexp "\\([^\\\\]\\)\\\\\\([^\\\\]\\)" in
  let t = "\\1\\\\\\\\\\2" in
  let s = Str.global_replace no_prefix t s in
  (* (\\)+\[^\]*)
  let some_prefix = Str.regexp "\\(\\\\\\\\\\)+\\\\\\([^\\\\]\\)" in
  let t = "\\1\\\\\\\\\\2" in
  let s = Str.global_replace some_prefix t s in
  (* convert to double string by escaping necessary chars *)
  escape_chars s ['#';'{';'}';'[';']';'(';')' ]


(* Bitwise the value of Regexp::<s> onto <fin> updating the code
   accumulator as necessary.
*)
let or_opt acc fin lang once s _pos =
  let v = Scope(Var(Constant, "Regexp"),s) in
  match fin with
  | Some e ->
      let acc, lhs = fresh acc in
      let lhs' = match lhs with LId id -> id | _ -> failwith "Impossible" in

      let call = C.mcall ~lhs ~targ:e (ID_Operator(Op_BOr)) [SE (EId v)] () in
      let acc = acc_enqueue call acc in
      acc, Some (EId lhs'), lang, once
  | None ->
      acc, Some (EId v), lang, once

(* Takes a string of regexp options like "ixm", and constructs the
   necessary arguments to Regexp.new for those modifiers.  Returns the
   4-tuple <acc,mod,lang,once> where

   acc - code accumulator
   mod - cfg option representing the bitwise or the corresponding options
   lang - option type containing the string identifier for a language
   once - boolean representing if this regexp should be interpreted once
*)
let parse_regexp_options acc str pos : (stmt acc * expr option * char option * bool) =
  let build_opt (acc,fin,lang,once) = function
    | 'i' -> or_opt acc fin lang once "IGNORECASE" pos
    | 'x' -> or_opt acc fin lang once "EXTENDED" pos
    | 'm' -> or_opt acc fin lang once"MULTILINE" pos
    | 'o' -> (acc,fin,lang,true)
    | 'n' | 'N' | 'e' | 'E' | 's' | 'S' | 'u' | 'U' as c->
        begin match lang with
          | None -> acc,fin,Some c,once
          | Some c' ->
              Log.err ~ctx:(Log.of_tok pos)
                "multiple language modifiers for regexp: %c %c??"
                c c';
              acc,fin,Some c',once
        end
    | c -> Log.fatal (Log.of_tok pos) "unknown regexp modifier: %c" c
  in
  string_fold_left build_opt (acc,None,None,false) str

let escape_regexp re =
  let escaped = ['{'; '}'] in
  let buf = Buffer.create 32 in
  let _ =
    string_fold_left
      (fun state c -> match state with
         | `NoEsc ->
             Buffer.add_char buf c;
             if c = '\\' then `Esc
             else if c = '['
             then `CharClass
             else `NoEsc

         | `Esc -> Buffer.add_char buf c; `NoEsc

         | `CharClass ->
             if List.mem c escaped
             then Buffer.add_char buf '\\';
             Buffer.add_char buf c;
             if c = '\\' then `EscClass
             else if c = ']'
             then `NoEsc
             else `CharClass

         | `EscClass ->
             Buffer.add_char buf c;
             `CharClass
      ) (`NoEsc) re
  in
  Buffer.contents buf

(** [refactor_list f (acc,res) lst] returns the the pair (acc',es)
    where acc' is the DQueue of hoisted values and es is the list of
    refactored values.  The function 'f' performs the individual
    refactoring (but must return a pair).
*)
let rec refactor_list f (acc,lst) = function
  | [] -> acc, lst
  | hd::tl ->
      let acc,e = f acc hd in
      let es = DQueue.enqueue e lst in
      refactor_list f (acc,es) tl

let is_literal = function
  | "true"
  | "false" -> true
  | _ -> false

let is_special = function
  | "true" | "false"
  | "__FILE__" | "__LINE__" ->
      true
  | _ -> false

let special_of_string pos x : expr =
  match x with
  | "true"  -> EId (True)
  | "false" -> EId (False)
  | "__FILE__" -> ELit (String (Parse_info.file_of_info pos))
  | "__LINE__" -> ELit (Num (spf "%d" (Parse_info.line_of_info pos)))
  | _ -> raise (Invalid_argument "special_of_string")

let refactor_id_kind _pos : Ast.id_kind -> var_kind = function
  | Ast.ID_Self | Ast.ID_Super -> raise Todo
  | Ast.ID_Lowercase -> Local
  | Ast.ID_Instance -> Instance
  | Ast.ID_Class -> Class
  | Ast.ID_Global -> Global
  | Ast.ID_Uppercase -> Constant

let refactor_builtin_or_global pos = function
  | Ast.ID_Global -> Global
  | _ ->
      Log.fatal (Log.of_tok pos)
        "trying to refactor other kind into builtin or global"

let refactor_uop pos = function
  | Ast.U Ast.Op_UMinus -> Op_UMinus
  | Ast.U Ast.Op_UPlus -> Op_UPlus
  | Ast.U Ast.Op_UTilde -> Op_UTilde

  | Ast.Op_UStarStar | Ast.Op_DefinedQuestion
  | Ast.U Ast.Op_UBang
  | Ast.Op_UNot
  | Ast.Op_UAmper
    ->
      Log.fatal (Log.of_tok pos)
        "trying to refactor construct posing as unary op"

let refactor_binop pos : Ast.binary_op -> binary_op = function
  | Ast.B Ast.Op_PLUS -> Op_Plus
  | Ast.B Ast.Op_MINUS -> Op_Minus
  | Ast.B Ast.Op_TIMES -> Op_Times
  | Ast.B Ast.Op_REM -> Op_Rem
  | Ast.B Ast.Op_DIV -> Op_Div
  | Ast.B Ast.Op_CMP -> Op_CMP
  | Ast.B Ast.Op_EQ -> Op_EQ
  | Ast.B Ast.Op_EQQ -> Op_EQQ
  | Ast.B Ast.Op_GEQ -> Op_GEQ
  | Ast.B Ast.Op_LEQ -> Op_LEQ
  | Ast.B Ast.Op_LT -> Op_LT
  | Ast.B Ast.Op_GT -> Op_GT
  | Ast.B Ast.Op_BAND -> Op_BAnd
  | Ast.B Ast.Op_BOR -> Op_BOr
  | Ast.B Ast.Op_MATCH -> Op_Match
  | Ast.B Ast.Op_XOR -> Op_XOR
  | Ast.B Ast.Op_POW -> Op_Pow
  | Ast.B Ast.Op_AREF -> Op_ARef
  | Ast.B Ast.Op_ASET -> Op_ASet
  | Ast.B Ast.Op_LSHIFT -> Op_LShift
  | Ast.B Ast.Op_RSHIFT -> Op_RShift

  | Ast.B Ast.Op_NEQ
  | Ast.B Ast.Op_NMATCH
  | Ast.Op_OP_ASGN _
  | Ast.Op_ASSIGN
  | Ast.Op_AND
  | Ast.Op_OR
  | Ast.Op_kAND
  | Ast.Op_kOR
  | Ast.Op_ASSOC
  | Ast.B Ast.Op_DOT2
  | Ast.Op_DOT3 as bop ->
      Log.fatal (Log.of_tok pos)
        "trying to refactor construct posing as binary op: %s"
        (Common.dump bop)

let msg_id_from_string = function
  | "+" -> ID_Operator Op_Plus
  | "-" -> ID_Operator Op_Minus
  | "*" -> ID_Operator Op_Times
  | "/" -> ID_Operator Op_Div
  | "%" -> ID_Operator Op_Rem
  | "<=>" -> ID_Operator Op_CMP
  | "==" -> ID_Operator Op_EQ
  | "===" -> ID_Operator Op_EQQ
  | ">=" -> ID_Operator Op_GEQ
  | "<=" -> ID_Operator Op_LEQ
  | "<" -> ID_Operator Op_LT
  | ">" -> ID_Operator Op_GT
  | "&" -> ID_Operator Op_BAnd
  | "|" -> ID_Operator Op_BOr
  | "=~" -> ID_Operator Op_Match
  | "^" -> ID_Operator Op_XOR
  | "**" -> ID_Operator Op_Pow
  | "[]" -> ID_Operator Op_ARef
  | "[]=" -> ID_Operator Op_ASet
  | "<<" -> ID_Operator Op_LShift
  | ">>" -> ID_Operator Op_RShift

  | "-@" -> ID_UOperator Op_UMinus
  | "+@" -> ID_UOperator Op_UPlus
  | "~@" | "~" -> ID_UOperator Op_UTilde
  | s -> ID_MethodName s

let rec tuple_of_lhs (lhs:lhs) pos : tuple_expr = match lhs with
  | LId id -> TE (EId id)
  | LTup l ->
      let l' = List.map (fun x -> tuple_of_lhs x pos) l in
      TTup (l')
  | LStar id -> TStar ((TE (EId id)))

let make_tuple_option : tuple_expr list -> tuple_expr option = function
  | [] -> None
  | [x] -> Some x
  | lst -> Some (TTup lst)

let make_assignable_msg (m : msg_id) : msg_id = match m with
  | ID_MethodName s -> ID_Assign s

  | ID_Assign _ ->
      (*Log.fatal Log.empty*)
      failwith "make_assignable_msg: already assignable????"

  | ID_Operator Op_ARef -> ID_Operator Op_ASet

  | ID_Operator _
  | ID_UOperator _ ->
      Log.fatal Log.empty "make_assignable_msg: non [] operator????"

  | ID_Super -> Log.fatal Log.empty "make_assignable_msg: super??"


let _replace_last f = function
  | [] -> []
  | l -> match List.rev l with
    | [] -> assert false
    | hd::tl -> List.rev ( (f hd):: tl)

let _method_formal_of_id : identifier -> method_formal_param = function
  | Var(Local,s) -> Formal_meth_id(s)
  | _ -> Log.fatal Log.empty "method_formal_of_id: non-id"

let _block_formal_of_id : identifier -> block_formal_param = function
  | Var(k,s) -> Formal_block_id(k,s)
  | _ ->
      Log.fatal Log.empty "block_formal_of_id: non-id"

(* convert the last value in the statement given by add_f *)
let rec convert_to_return (add_f : tuple_expr -> stmt_node) acc stmt = match stmt.snode with
  | ExnBlock eb ->
      let q_to_stmt q = C.seq (DQueue.to_list q) stmt.pos in
      let convert_rescue rb =
        {rb with rescue_body = q_to_stmt (convert_to_return add_f acc rb.rescue_body)}
      in
      (* body doesn't return if there's an else stmt *)
      let body = match eb.exn_else with
        | Some(_) -> eb.exn_body
        | None -> q_to_stmt (convert_to_return add_f acc eb.exn_body)
      in
      let rescue = List.map convert_rescue eb.exn_rescue in
      let ensure = eb.exn_ensure in (* ensure doesn't return a value *)
      let eelse = Option.map q_to_stmt
          (Option.map (convert_to_return add_f acc) eb.exn_else)
      in
      let eblk = C.exnblock body rescue ?ensure ?eelse stmt.pos in
      DQueue.enqueue eblk DQueue.empty

  | Case cb ->
      let whens' =
        List.map
          (fun (gs,bs) ->
             let q = convert_to_return add_f acc bs in
             let bs' = C.seq (DQueue.to_list q) bs.pos in
             (gs,bs')
          ) cb.case_whens
      in
      let else' = Option.map
          (fun s ->
             C.seq (DQueue.to_list (convert_to_return add_f acc s)) s.pos
          ) cb.case_else
      in
      let cb' = {cb with case_whens=whens'; case_else = else'} in
      DQueue.enqueue (mkstmt (Case cb') stmt.pos) DQueue.empty

  | For(_params,guard,_body) ->
      let q = DQueue.enqueue stmt DQueue.empty in
      let guard' = TE guard in
      let ret = mkstmt (add_f (guard' : tuple_expr)) stmt.pos in
      DQueue.enqueue ret q

  | If(g,t,f) ->
      let tq = DQueue.to_list (convert_to_return add_f acc t) in
      let t' = C.seq tq stmt.pos in
      let fq = DQueue.to_list (convert_to_return add_f acc f) in
      let f' = C.seq fq stmt.pos in
      let if'  = mkstmt (If(g,t',f')) stmt.pos in
      DQueue.enqueue if' DQueue.empty

  | Return _ -> DQueue.enqueue stmt DQueue.empty

  | Seq slist ->
      let q = DQueue.from_list slist in
      let q,last = DQueue.pop_back q in
      DQueue.append q (convert_to_return add_f acc last)

  | I Expression e ->
      let e' = TE e in
      let ret = mkstmt (add_f (e' : tuple_expr)) stmt.pos in
      DQueue.enqueue ret DQueue.empty

  | Yield(None,args) ->
      (* we don't need to track v here since its immediately dead
         after the return *)
      let _, v = fresh (acc_emptyq acc) in
      let yield = C.yield ~lhs:v ~args stmt.pos in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in
      let v'' = TE (EId v') in
      let ret = mkstmt (add_f v'') stmt.pos in
      let q = DQueue.enqueue yield DQueue.empty in
      DQueue.enqueue ret q

  | I Call(None,mc) ->
      (* we don't need to track v here since its immediately dead
         after the return *)
      let _, v = fresh (acc_emptyq acc) in
      let meth = mkstmt (I (Call(Some v,mc))) stmt.pos in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in
      let v'' = TE (EId v') in
      let ret = mkstmt (add_f v'') stmt.pos in
      let q = DQueue.enqueue meth DQueue.empty in
      DQueue.enqueue ret q

  | I Assign(lhs,_)
  | I Call(Some lhs,_)
  | Yield(Some lhs,_) ->
      let ret = mkstmt (add_f (tuple_of_lhs lhs stmt.pos)) stmt.pos in
      let q = DQueue.enqueue stmt DQueue.empty in
      DQueue.enqueue ret q

  | D Defined(id,_s) ->
      let id' = TE (EId id) in
      let ret = mkstmt (add_f (id' : tuple_expr)) stmt.pos in
      let q = DQueue.enqueue stmt DQueue.empty in
      DQueue.enqueue ret q

  | Break _ | Redo | Retry | Next _ ->
      DQueue.enqueue stmt DQueue.empty (* control jumps elsewhere *)

  | D Undef _
  | While _
  | D ModuleDef _
  | D MethodDef _
  | D ClassDef _
  | D Alias _ ->
      let q = DQueue.enqueue stmt DQueue.empty in
      let ret = mkstmt (add_f (TE (EId Nil))) stmt.pos in
      DQueue.enqueue ret q

  | Begin _ | End _ ->
      (*Log.fatal (Log.of_tok stmt.pos)*) failwith
        "BEGIN or END block can not be used inside a method"

open Visitor
class proc_transformer = object
  inherit default_visitor
  method! visit_stmt s = match s.snode with
    | Return args -> ChangeTo (update_stmt s (Next args))

    | D ModuleDef _ | D MethodDef _ | D ClassDef _ (* new scope *)
    | While _ | For _ | I Call _ (* nested blocks *) ->
        SkipChildren

    | _ -> DoChildren

end

let proc_transform block = match block with
  | CB_Arg _ -> block
  | CB_Block(params,body) ->
      let visitor = new proc_transformer in
      let body' = visit_stmt visitor body in
      CB_Block(params,body')

let add_final_return return_f acc pos =
  if DQueue.is_empty acc.q
  then acc_enqueue (mkstmt (return_f (TE (EId Nil))) pos) acc
  else
    let q,last = DQueue.pop_back acc.q in
    let last_q = convert_to_return return_f acc last in
    (* adding the final return shouldn't affect the seen set *)
    {acc with q = DQueue.append q last_q}

let refactor_formal_list f acc lst pos =
  let acc, rlst = List.fold_left
      (fun (acc,lst) formal ->
         let acc,formal' = f acc formal pos in
         acc, formal'::lst
      ) (acc,[]) lst
  in acc, List.rev rlst

let rec refactor_expr (acc:stmt acc) (e : Ast.expr) : stmt acc * Il_ruby.expr =
  match e with
  | Ast.Splat _
  | Ast.Ellipsis _ | Ast.DeepEllipsis _ | Ast.TypedMetavar _
    ->
      raise Todo
  | Ast.Binop(_, (Ast.Op_OP_ASGN _, _), _)
  | Ast.S Ast.If _ | Ast.S Ast.Yield _ | Ast.S Ast.Return _
  | Ast.S Ast.Break _ | Ast.S Ast.Next _ | Ast.S Ast.Redo _ | Ast.S Ast.Retry _

  | Ast.S Ast.Case _ | Ast.S Ast.ExnBlock _
  | Ast.D Ast.EndBlock _ | Ast.D Ast.BeginBlock _
  | Ast.CodeBlock _ | Ast.Lambda _
  | Ast.D Ast.MethodDef _
  | Ast.S Ast.For _
  | Ast.S Ast.Unless _ | Ast.S Ast.Until _
  | Ast.S Ast.While _
  | Ast.Ternary _ | Ast.D Ast.Alias _ | Ast.D Ast.Undef _ as s ->
      (* These cases are where stmts are embedded in expressions,
         like x = (if g then 1 else 2 end) To handle this, we create
         a fresh variable are store the result of each possible
         return of the stmt.  e.g.
         if g then t = 1 else t = 2 end; x = t
      *)
      let acc' = refactor_stmt acc s in
      (* grab the last statement, think x = (s1();s2()) *)
      let rest,s' = DQueue.pop_back acc'.q in
      let acc = {acc' with q = rest} in
      let acc, v = fresh acc in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in
      (* move the assignment expression into the statement *)
      let ss = add_last_assign ~do_break:false v' s' in
      let acc = acc_enqueue ss acc in
      acc, EId v'

  | Ast.D Ast.ClassDef _
  | Ast.D Ast.ModuleDef _ ->
      let acc, v = fresh acc in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in
      let st_acc = refactor_stmt (acc_emptyq acc) e in
      let acc = acc_seen acc st_acc in
      let rest,s = DQueue.pop_back st_acc.q in
      let acc = acc_append acc {acc with q=rest} in
      let s' =
        match s.snode with
        | D ClassDef(None,ck,body) ->
            mkstmt (D (ClassDef(Some v,ck,body))) s.pos
        | D ModuleDef(None,name,body) ->
            mkstmt (D (ModuleDef(Some v,name,body))) s.pos
        | _ ->
            Log.fatal (Log.of_tok (tok_of e))
              "[BUG] Class/module? xlate error: %s(%s)"
              (CodePrinter.show_stmt s)
              (CodePrinter.show_identifier v')
      in
      acc_enqueue s' acc, EId v'


  | Ast.ScopedId(Ast.TopScope(pos,e)) ->
      let acc,e' = refactor_id acc (Ast.Id e) in
      let s = match e' with
        | Var(Constant, s) -> s
        | _ -> Log.fatal (Log.of_tok pos) "unknown right hand of uscope: %s"
                 (CodePrinter.show_identifier e')
      in
      acc, EId (UScope s)

  | Ast.Unary((Ast.U Ast.Op_UMinus,_pos), Ast.Literal(Ast.Num (i,_))) ->
      acc, ELit (Num ("-"^i))


  | Ast.Unary((Ast.U Ast.Op_UMinus, _pos), Ast.Literal(Ast.Float(s,_))) ->
      assert(s.[0] <> '-');
      acc, ELit (Float("-" ^ s))

  | Ast.Unary(((Ast.U Ast.Op_UBang | Ast.Op_UNot),pos),e) ->
      let acc,v = fresh acc in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in
      let t = C.assign v (TE (EId True)) pos in
      let f = C.assign v (TE (EId False)) pos in
      let acc,e' = refactor_expr acc e in
      let acc = acc_enqueue (C.if_s e' ~t:f ~f:t pos) acc in
      acc, EId v'

  | Ast.Unary((uop,pos),e) ->
      let acc,e' = refactor_expr acc e in
      let msg = ID_UOperator (refactor_uop pos uop) in
      make_call_expr acc (Some e') msg [] None pos

  (* A::m is really a method call *)
  | Ast.ScopedId(Ast.Scope(_e1BUG, pos, Ast.SV (_, Ast.ID_Lowercase))) ->
      let acc,v = fresh acc in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in
      let acc = seen_lhs acc v in
      let e' = Ast.Call(e,fb pos [], None) in
      (refactor_method_call_assign acc (Some v) e'), EId v'

  | Ast.ScopedId(Ast.Scope(e1,pos,_e2_var_or_method_name_now)) ->
      let _acc,e1' = refactor_id acc e1 in
      let acc,e2' = (* refactor_id acc e2 *) raise Todo in
      let s = match e2' with
        | Var(Constant, s) -> s
        | _ -> Log.fatal (Log.of_tok pos) "unknown right hand of scope: %s"
                 (CodePrinter.show_identifier e2')
      in
      acc, EId (Scope(e1', s))

  | Ast.Binop(e1,(Ast.B Ast.Op_NEQ,pos),e2) ->
      let acc, t1 = fresh acc in
      let t1' = match t1 with LId id -> id | _ -> failwith "Impossible" in

      let acc, t2 = fresh acc in
      let t2' = match t2 with LId id -> id | _ -> failwith "Impossible" in

      let acc = refactor_binop_into_mc acc (Some t1) e1 (Ast.B Ast.Op_EQ) e2 pos in
      let t = C.assign t2 (TE (EId True)) pos in
      let f = C.assign t2 (TE (EId False)) pos in
      let acc = acc_enqueue (C.if_s (EId t1') ~t:f ~f:t pos) acc in
      acc, (EId t2')

  | Ast.Binop(e1,(Ast.B Ast.Op_NMATCH,pos),e2) ->
      let acc, t1 = fresh acc in
      let t1' = match t1 with LId id -> id | _ -> failwith "Impossible" in
      let acc, t2 = fresh acc in
      let t2' = match t2 with LId id -> id | _ -> failwith "Impossible" in
      let acc = refactor_binop_into_mc acc (Some t1) e1 (Ast.B Ast.Op_MATCH) e2 pos in
      let t = C.assign t2 (TE (EId True)) pos in
      let f = C.assign t2 (TE (EId False)) pos in
      let acc = acc_enqueue (C.if_s (EId t1') ~t:f ~f:t pos) acc in
      acc, (EId t2')

  | Ast.DotAccess(_,(pos),_) ->
      Log.fatal (Log.of_tok pos) "refactor_expr got dot expr??"

  | Ast.Binop(e1, ((Ast.Op_AND|Ast.Op_kAND),pos), e2) ->
      refactor_and_if acc e1 e2 pos
  | Ast.Binop(e1, ((Ast.Op_OR|Ast.Op_kOR), pos), e2) ->
      refactor_or_if acc e1 e2 pos

  | Ast.Binop(e1,(Ast.Op_ASSOC,_pos),e2) ->
      let acc,e1' = refactor_expr acc e1 in
      let acc,e2' = refactor_expr acc e2 in
      acc, ELit (Hash [ e1', e2'])

  | Ast.Binop(e1,(Ast.B Ast.Op_DOT2, _pos),e2) ->
      let acc,e1' = refactor_expr acc e1 in
      let acc,e2' = refactor_expr acc e2 in
      acc, ELit (Range(false, e1', e2'))

  | Ast.Binop(e1,(Ast.Op_DOT3, _pos),e2) ->
      let acc,e1' = refactor_expr acc e1 in
      let acc,e2' = refactor_expr acc e2 in
      acc, ELit (Range(true, e1', e2'))

  | Ast.Binop(e1,(Ast.Op_ASSIGN,pos),e2) ->
      let acc,e1',after = refactor_lhs acc e1 in
      let acc,e2' = refactor_tuple_expr acc e2 in
      let acc = acc_enqueue (C. assign e1' e2' pos) acc in
      let acc = acc_append acc after in
      let e1_id = match e1' with
        | LId id -> id
        | _ -> Log.fatal Log.empty "nested multi-assign in refactor_expr"
      in
      acc, EId e1_id

  | Ast.Binop(e1,(bop,pos),e2) ->
      let acc, v = fresh acc in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in

      let acc = refactor_binop_into_mc acc (Some v) e1 bop e2 pos in
      acc, EId v'

  | Ast.Call(Ast.Id(("defined?", pos), Ast.ID_Lowercase), args, cb) ->
      let _ = Option.map (fun _ -> Log.fatal (Log.of_tok pos) "cb for 'defined?' ??") cb in
      let acc, v = fresh acc in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in

      refactor_defined acc v args pos, EId v'

  | Ast.Call(_msgBUG, _argsBUG, _cb) ->
      let acc,v = fresh acc in
      let v' = match v with LId id -> id | _ -> failwith "Impossible" in

      let acc = seen_lhs acc v in
      (refactor_method_call_assign acc (Some v) e), EId v'

  | Ast.Id((s,pos), Ast.ID_Lowercase) as e ->
      if is_special s then acc, (special_of_string pos s)
      else if StrSet.mem s acc.seen
      then acc, EId (Var(Local, s))
      else if s = "super"
      then
        let acc, lhs = fresh acc in
        let lhs' = match lhs with LId id -> id | _ -> failwith "Impossible" in

        (refactor_super acc (Some lhs) pos), EId lhs'
      else refactor_expr acc (Ast.Call(e, fb pos [],None))

  (* handles $0 *)
  | Ast.Id(("0", _pos), Ast.ID_Global) ->
      acc, ELit (String (*Config.conf.Config.ruby_file*)"TODO:ruby_file")

  | Ast.Id(_, Ast.ID_Self) -> acc, EId (Self)
  | Ast.Id(_, Ast.ID_Super) -> failwith "TODO"

  | Ast.Id((s, pos),ik) ->
      if is_special s then acc, (special_of_string pos s)
      else acc, EId (Var(refactor_id_kind pos ik, s))

  | Ast.Literal(l) -> refactor_lit acc l
  | Ast.Atom(l) -> refactor_atom acc l

  | Ast.Tuple(l) ->
      let acc,l' = refactor_list refactor_star_expr (acc,DQueue.empty) l in
      acc, ELit (Array (DQueue.to_list l'))
  | Ast.Array(_, l,_) ->
      let l = Ast.args_to_exprs l in
      let acc,l' = refactor_list refactor_star_expr (acc,DQueue.empty) l in
      acc, ELit (Array (DQueue.to_list l'))

  | Ast.Hash(_b,(pos, l, _)) ->
      let acc, hl = refactor_hash_list acc l pos in
      acc, ELit (Hash hl)

  | Ast.S Ast.Block(_, l, _) -> begin match List.rev l with
    | [] -> Log.fatal Log.empty "refactor_expr: empty block???"
    | last::rest ->
        let acc = refactor_stmt_list acc (List.rev rest) in
        refactor_expr acc last
  end

and refactor_defined acc lhs args pos =
  let lhs' = match lhs with LId id -> id | _ -> failwith "Impossible" in

  let arg = match args with
    | (_, [x], _) -> x |> Ast.arg_to_expr
    | _ -> Log.fatal (Log.of_tok pos) "multiple args to defined?"
  in
  let inside_acc = refactor_stmt (acc_emptyq acc) arg in
  let acc = acc_seen acc inside_acc in
  let s = C.seq (DQueue.to_list inside_acc.q) pos in
  acc_enqueue (C.defined lhs' s pos) acc

and refactor_super acc lhs pos = match acc.super_args with
  | None -> Log.fatal (Log.of_tok pos) "super called outside of method"
  | Some(args,None) ->
      acc_enqueue (C.mcall ?lhs ID_Super args ()) acc
  | Some(args,Some e) ->
      acc_enqueue (C.mcall ?lhs ID_Super args ~cb:(CB_Arg e) ()) acc

(* turn /foo#{bar}/mods into a call to Regexp.new *)
and construct_explicit_regexp acc pos re_interp mods : stmt acc * expr =
  let acc,re_opts,lang,once = parse_regexp_options acc mods pos in
  let build_call acc lhs =
    let re_interp = List.map
        (function
          | Ast.StrExpr _ as c -> c
          | Ast.StrChars (s, t) ->
              Ast.StrChars (escape_chars s ['\\'], t)
        ) re_interp
    in
    let acc,str = refactor_interp_string acc re_interp pos in
    let new_opts = match lang with
      | None -> []
      | Some c -> [SE (ELit (String (String.make 1 c)))]
    in
    let new_opts = match re_opts with
      | None -> (SE str)::(SE (ELit (Num "0")))::new_opts
      | Some v -> (SE str)::(SE v)::new_opts
    in
    let call = C.mcall ~lhs ~targ:(EId (UScope "Regexp"))
        (ID_MethodName "new") new_opts ()
    in
    acc_enqueue call acc
  in
  if once then begin
    let glob = fresh_global pos in
    let f_acc = acc_emptyq acc in
    let f_acc = build_call f_acc (LId glob) in
    let acc = acc_seen acc f_acc in
    let if_e =
      C.if_s (EId glob) pos
        ~t:(C.expr (EId glob) pos)
        ~f:(C.seq (DQueue.to_list f_acc.q) pos)
    in
    acc_enqueue if_e acc, EId glob
  end else
    let acc, lhs = fresh acc in
    let lhs' = match lhs with LId id -> id | _ -> failwith "Impossible" in
    build_call acc lhs, EId lhs'

and refactor_interp_string acc istr pos =
  let refactor_contents acc : Ast.interp -> stmt acc * expr = function
    | Ast.StrChars (s, _t) -> acc, ELit (String s)
    | Ast.StrExpr (_l, ast_e, _r) ->
        let acc, e = refactor_expr acc ast_e in
        make_call_expr acc (Some e) (ID_MethodName "to_s") [] None pos
  in
  let rec helper acc expr_acc l = match l with
    | [] -> acc, expr_acc
    | hd::tl ->
        let acc, e = refactor_contents acc hd in
        let acc, expr_acc =
          make_call_expr acc (Some expr_acc) (ID_Operator Op_Plus) [SE e] None pos
        in
        helper acc expr_acc tl
  in
  (* unfold once to get start of expr_acc *)
  match istr with
  | [] -> acc, ELit (String "")
  | hd::tl ->
      let acc, e = refactor_contents acc hd in
      helper acc e tl

and refactor_atom acc (l : Ast.atom) : stmt acc * expr =
  let (_tk, kind) = l in
  match kind with
  | Ast.AtomSimple (s, _pos) -> acc, ELit (Atom s)
  | Ast.AtomFromString (_, [Ast.StrChars (s, _t)], _) -> acc, ELit (Atom s)
  | Ast.AtomFromString (lpos, istr, _rpos) ->
      let acc, str = refactor_interp_string acc istr lpos in
      make_call_expr acc (Some str) (ID_MethodName "to_sym") [] None lpos

and refactor_lit acc (l : Ast.literal) : stmt acc * expr = match l with
  | Ast.Char _ | Ast.Complex _ | Ast.Rational _ -> raise Todo

  | Ast.Num (i, _pos) -> acc, ELit (Num i)
  | Ast.Float(s, _pos) -> acc, ELit (Float(s))
  | Ast.String(Ast.Single (s, _pos)) ->
      acc, ELit (String (unescape_single_string s))

  | Ast.String(Ast.Double (lpos, xs, _rpos)) ->
      refactor_interp_string acc xs lpos
  | Ast.String(Ast.Tick (lpos, xs, _rpos)) ->
      let acc, e = refactor_interp_string acc xs lpos in
      make_call_expr acc None (ID_MethodName "__backtick") [SE e] None lpos


  | Ast.Regexp((_, [Ast.StrChars (s1, _t1)],_), s2opt) ->
      let s1' = escape_regexp s1 in
      let s2 = match s2opt with None -> "" | Some (s, _) -> s in
      acc, ELit (Regexp(s1',s2))

  | Ast.Regexp((pos, xs, _), s2opt) ->
      let s2 = match s2opt with None -> "" | Some (s, _) -> s in
      construct_explicit_regexp acc pos xs s2

  | Ast.Nil _ -> acc, EId (Nil)
  | Ast.Bool (true,_) -> acc, EId (True)
  | Ast.Bool (false,_) -> acc, EId (False)

and refactor_star_expr (acc:stmt acc) e : stmt acc * star_expr = match e with
  | Ast.Splat(_pos, Some e) ->
      let acc, e' = refactor_expr acc e in
      acc, SStar (e')
  | e ->
      let acc, e' = refactor_expr acc e in
      acc, (SE e' :> star_expr)

and refactor_method_arg_no_cb acc e : stmt acc * star_expr = match e with
  | Ast.Unary((Ast.Op_UAmper,pos), _eBUG) ->
      Log.fatal (Log.of_tok pos) "unexpected & arg in method arguments"
  | e -> refactor_star_expr acc e

and refactor_method_args_and_cb acc (_lp, arg_list, _rp) cb =
  let arg_list = Ast.args_to_exprs arg_list in
  let acc, lst, cb_arg = match List.rev arg_list, cb with
    | Ast.Unary((Ast.Op_UAmper, _pos), e)::rest, None ->
        let acc, e' = refactor_expr acc e in
        acc, (List.rev rest), Some (CB_Arg e')
    | (Ast.Unary((Ast.Op_UAmper, pos), _)::_), Some _ ->
        Log.fatal (Log.of_tok pos)
          "method can't have both an &-argument and code block"
    | _lst, Some cb_e -> acc, arg_list, Some (refactor_codeblock acc cb_e)
    | _lst, None -> acc, arg_list, None
  in
  let acc,final_rev_args = List.fold_left
      (fun (acc,lst) x ->
         let acc, arg = refactor_method_arg_no_cb acc x in
         acc, arg::lst
      ) (acc,[]) lst
  in
  acc, (List.rev final_rev_args), cb_arg

and refactor_hash_list acc l pos =
  let rec pair_list acc = function
    | [] -> acc
    | x::[] ->
        Log.fatal (Log.of_tok pos) "odd number of elements in hash: %s"
          (CodePrinter.show_expr x)
    | x::y::tl -> pair_list ((x,y)::acc) tl
  in
  match l with
  | [] -> acc, []
  | (Ast.Binop(_,(Ast.Op_ASSOC,_),_))::_ ->
      let acc,rlst =
        List.fold_left
          (fun (acc,lst) -> function
             | Ast.Binop(e1,Ast.(Op_ASSOC,_pos),e2) ->
                 let acc,e1' = refactor_expr acc e1 in
                 let acc,e2' = refactor_expr acc e2 in
                 acc, (e1', e2')::lst
             | _ -> Log.fatal (Log.of_tok pos) "non assoc expression in hash list?"
          ) (acc,[]) l
      in
      acc, List.rev rlst

  | _::_ ->
      let acc,l' = refactor_list refactor_expr (acc,DQueue.empty) l in
      let lst = List.rev (pair_list [] (DQueue.to_list l')) in
      acc, lst

and refactor_binop_into_mc acc res e1 bop e2 pos =
  let acc,e1' = refactor_expr acc e1 in
  let acc,e2' = refactor_method_arg_no_cb acc e2 in
  let call = C.mcall ?lhs:res ~targ:e1' (ID_Operator (refactor_binop pos bop))
      [e2'] ()
  in acc_enqueue call acc

and refactor_and_if (acc:stmt acc) l r pos : stmt acc * expr =
  let acc,l' = refactor_expr acc l in
  (* we use a separate accumulator since we only want the refactored
     expressions to execute if the initial condition was true.  Thus we
     place all refactoring code inside the true branch of the If *)
  let r_acc = refactor_stmt (acc_emptyq acc) r in
  let acc, v = fresh acc in
  let v' = match v with LId id -> id | _ -> failwith "Impossible" in
  let vl = C.assign v (TE l') pos in
  let v_acc = add_final_return (fun t -> I (Assign(v,t))) r_acc pos in
  let vr = C.seq (DQueue.to_list v_acc.q) pos in
  (* the && operator returns the value of the last expression evaluated,
     thus "[not true] && _" returns [not true]
     and  "[not false] && x" returns x
  *)
  let acc = acc_enqueue (C.if_s l' ~t:vr ~f:vl pos) acc in
  (acc_seen acc v_acc), EId v'

and refactor_or_if acc l r pos =
  let acc,l' = refactor_expr acc l in
  (* Like the and refactoring above, we use a separate accumulator
     since we only want the refactored expressions to execute if the
     initial condition was false.  Thus we place all refactoring code
     inside the true branch of the If *)
  let acc, v = fresh acc in
  let v' = match v with LId id -> id | _ -> failwith "Impossible" in
  let vl = C.assign v (TE l') pos in
  let return_f t = I (Assign(v,t)) in
  let r_acc = refactor_stmt (acc_emptyq acc) r in
  let v_acc = add_final_return return_f r_acc pos in
  let vr = C.seq (DQueue.to_list v_acc.q) pos in
  (* the || operator returns the value of the last expression evaluated,
     thus "[not true] || x" returns x
     and  "[not false] || x" returns [not false]
  *)
  let acc = acc_enqueue (C.if_s l' ~t:vl ~f:vr pos) acc in
  (acc_seen acc v_acc), EId v'

and refactor_tuple_expr (acc:stmt acc) (e : Ast.expr) : stmt acc * Il_ruby.tuple_expr =
  match e with
  | Ast.Tuple(l) ->
      let acc,l' = refactor_list refactor_tuple_expr (acc,DQueue.empty) l in
      acc, TTup (DQueue.to_list l')

  | Ast.Splat(_pos, Some e) ->
      let acc, e' = refactor_tuple_expr acc e in
      begin match e' with
        | (TE _ | TTup _) as e' -> acc, TStar (e')
        | TStar _ ->
            Log.fatal Log.empty "refactor_tuple_expr: nested / double star expression"
      end
  | _ ->
      let acc, expr = refactor_expr acc e in
      let expr' = TE expr in
      let expr = (expr' : tuple_expr) in
      acc, expr

and refactor_lhs acc e : (stmt acc * lhs * stmt acc) =
  let acc,lhs,after = match e with
    | Ast.Tuple(l) ->
        let rec work (acc,lst,after) = function
          | [] -> acc, lst,after
          | hd::tl ->
              let acc,e,after' = refactor_lhs acc hd in
              let after = acc_append after after' in
              let es = DQueue.enqueue e lst in
              work (acc,es,after) tl
        in
        let acc,l',after = work (acc,DQueue.empty,acc_emptyq acc) l in
        acc, LTup (DQueue.to_list l'), after

    | Ast.Splat(_pos, None) ->
        let acc, v = fresh acc in
        let v' = match v with LId id -> id | _ -> failwith "Impossible" in

        acc, LStar (v'), acc_emptyq acc

    | Ast.Splat(pos, Some e) ->
        let acc, e',after = refactor_lhs acc e in
        begin match e' with
          | LId (id) -> acc, LStar id, after
          | LTup _| LStar ( _) ->
              Log.fatal (Log.of_tok pos) "refactor_lhs: nested star?"
        end

    | Ast.Id((s, _pos), Ast.ID_Lowercase)  ->
        if is_literal s then Log.fatal Log.empty "lhs literal?"
        else acc, LId (Var(Local, s)), acc_emptyq acc

    | Ast.Id((s, pos),ik) ->
        if is_literal s then Log.fatal Log.empty "lhs literal?"
        else acc, LId (Var(refactor_id_kind pos ik, s)), acc_emptyq acc

    | Ast.DotAccess(_e1,( _pos),_e2) ->
        Log.fatal Log.empty "dot expression on lhs?"

    | Ast.Call(Ast.DotAccess(targ,(_pos),msg),args,None) ->
        let after = acc_emptyq acc in
        let after,targ' = refactor_expr after targ in
        let after,msg' = refactor_msg2 after msg in
        let msg' = make_assignable_msg msg' in
        let after, args', cb = refactor_method_args_and_cb after args None in
        let after, last_arg = fresh after in
        let last_arg' = match last_arg with LId id -> id | _ -> failwith "Impossible" in
        let last_arg'' = SE (EId last_arg') in

        let args' = args' @ [last_arg''] in
        let mc_stmt = C.mcall ~targ:targ' msg' args' ?cb () in
        let after = acc_enqueue mc_stmt after in
        acc, last_arg, after

    | Ast.Call _ ->
        Log.fatal Log.empty "unhandled methodcall on lhs?"

    | _ ->
        match refactor_expr acc e with
        | acc, EId id -> acc, LId id, acc_emptyq acc
        | _acc, ELit _ -> Log.fatal Log.empty "lhs literal?"
  in
  let acc = seen_lhs acc lhs in
  acc, lhs,after

and refactor_id (acc:stmt acc) e : stmt acc * identifier =
  match refactor_expr acc e with
  | (acc,EId id) -> acc, id
  | _,ELit l ->
      Log.fatal (Log.of_tok (tok_of e)) "lhs_of_expr: literal %s"
        (CodePrinter.show_literal l)

and string_of_lit_kind _kind = "TODO"

and refactor_msg (acc:stmt acc) msg : stmt acc * msg_id = match msg with
(*
  | Ast.Operator(bop, pos) ->  acc, ID_Operator (refactor_binop pos bop)
  | Ast.UOperator(uop, pos) -> acc, ID_UOperator (refactor_uop pos uop)
*)

  | Ast.Id((s, _pos), (Ast.ID_Lowercase | Ast.ID_Uppercase )) ->
      acc, ID_MethodName s


  | Ast.Literal((Ast.Nil _ | Ast.Bool _) as lk) ->
      let lks = string_of_lit_kind lk in
      acc, ID_MethodName lks
  | e ->
      Log.fatal (Log.of_tok (tok_of e)) "refactor_msg unknown msg: %s\n"
        (Ast_ruby.show_expr e)

and refactor_msg2 (acc:stmt acc) msg : stmt acc * msg_id = match msg with
  | Ast.MethodOperator(bop, pos) ->  acc, ID_Operator (refactor_binop pos (Ast.B bop))
  | Ast.MethodUOperator(uop, pos) -> acc, ID_UOperator (refactor_uop pos (Ast.U uop))

  | Ast.MethodId((s, _pos), (Ast.ID_Lowercase | Ast.ID_Uppercase )) ->
      acc, ID_MethodName s

  | Ast.MethodIdAssign((s, _pos), _, (Ast.ID_Lowercase))
  | Ast.MethodIdAssign((s, _pos), _, (Ast.ID_Uppercase)) ->
      acc, ID_Assign s

(*
  | Ast.Literal((Ast.Nil _ | Ast.Bool _) as lk) ->
      let lks = string_of_lit_kind lk in
        acc, ID_MethodName lks
*)
  | e ->
      Log.fatal (Log.of_tok (tok_of e)) "refactor_msg unknown msg: %s\n"
        (Ast_ruby.show_method_name e)

and refactor_symbol_or_msg (acc:stmt acc) sym_msg =
  match sym_msg with
  | Ast.MethodAtom(_tcolon, Ast.AtomSimple(s,_pos)) ->
      acc, msg_id_from_string s
  | Ast.MethodAtom(_tcolon, Ast.AtomFromString(lt, interp, _rt)) ->
      let acc, e = refactor_interp_string acc interp lt in
      begin match e with
        | ELit (String s) -> acc, msg_id_from_string s
        | _ -> Log.fatal Log.empty "alias with symbol interp string?"
      end
  | msg -> refactor_msg2 acc msg

and refactor_codeblock acc : Ast.expr -> codeblock = function
  | Ast.CodeBlock((pos,_,_),formals,body) ->
      let formals = match formals with
        | None -> [Ast.Formal_rest pos (* TODO *)]
        | Some lst -> lst
      in
      let acc, formals = refactor_block_formal_list acc formals pos in
      let body_acc = refactor_stmt_list (acc_emptyq acc) body in
      let add_next e = Next(Some e) in
      let body_acc = add_final_return add_next body_acc pos in
      CB_Block(formals, C.seq (DQueue.to_list body_acc.q) pos)
  | _ ->
      Log.fatal Log.empty "refactor_codeblock: non-codeblock"

and _map_codeblock acc cb_o = Option.map (refactor_codeblock acc) cb_o

(* assign the last value of the stmt to [id].
   e.g., add_last_assign "x" [if_stmt] becomes
   if (guard) then x = result else x = result end
*)
and add_last_assign ~do_break (id:identifier) (s : stmt) : stmt =
  let lhs = (LId id : lhs) in match s.snode with
  | Seq(lst) -> begin match List.rev lst with
    (* empty evaluates to nil *)
    | [] -> C.assign lhs (TE (EId Nil)) s.pos
    | last::rest ->
        let last' = add_last_assign ~do_break id last in
        C.seq (List.rev (last'::rest)) s.pos
  end

  | I Expression(e) -> (* x becomes id = x *)
      C.assign lhs (TE e) s.pos

  | I Assign(e1,_) -> (* id2 = e becomes id2 = e; id = id2 *)
      let new_s = C.assign lhs (tuple_of_lhs e1 s.pos) s.pos in
      C.seq [s;new_s] s.pos

  | I Call(None, mc) -> (* x.f() becomes id = x.f() *)
      mkstmt (I (Call(Some lhs, mc))) s.pos
  | Yield(None,es) -> C.yield ~lhs ~args:es s.pos

  | I Call(Some id2, _) (* id2=x.f() becomes id2=x.(); id=id2 *)
  | Yield(Some id2,_) ->
      let new_s = C.assign lhs (tuple_of_lhs id2 s.pos) s.pos in
      C.seq [s;new_s] s.pos

  | If(g,t,f) ->
      let t' = add_last_assign ~do_break id t in
      let f' = add_last_assign ~do_break id f in
      C.if_s g ~t:t' ~f:f' s.pos

  | While(g, body) ->
      let body' = add_last_assign ~do_break:true id body in
      (* the while body may never execute, so put a nil assign first *)
      let nilassgn = C.assign lhs (TE (EId Nil)) s.pos in
      C.seq [nilassgn;(C.while_s g body' s.pos)] s.pos

  | For(flist,g,body) ->
      let new_s = C.assign (LId id) (TE g) s.pos in
      let body' = add_last_assign ~do_break:true id body in
      C.seq [new_s;C.for_s flist (EId id:expr) body' s.pos] s.pos

  | D Defined(id2,_) ->
      let new_s = C.assign (LId id) (TE (EId id2)) s.pos in
      C.seq [s;new_s] s.pos

  | ExnBlock(exn) ->
      let body = add_last_assign ~do_break id exn.exn_body in
      let rescue =
        List.map (fun r -> C.rblock r.rescue_guards
                     (add_last_assign ~do_break id r.rescue_body)
                 ) exn.exn_rescue;
      in
      let ensure = exn.exn_ensure in (* ensure does not return a value *)
      let eelse = Option.map (add_last_assign ~do_break id) exn.exn_else in
      C.exnblock body rescue ?ensure ?eelse s.pos

  | Case(cb) ->
      let cb' = {
        case_guard = cb.case_guard;
        case_whens =
          List.map
            (fun (gs,ss) ->
               gs, (add_last_assign ~do_break id ss)
            ) cb.case_whens;
        case_else = match cb.case_else with
          | None -> None
          | Some x -> Some (add_last_assign ~do_break id x)
      } in mkstmt (Case cb') s.pos


  | Return _ -> s (* no need to assign the result of a return statement *)

  | Break o when do_break ->
      let rhs = default_opt (TE (EId Nil)) o in
      C.seq [C.assign (LId id) rhs s.pos;C.break s.pos] s.pos

  | Break _ | Redo | Retry | Next _ -> s (* control jumps elsewhere *)

  | D Undef _
  | D ModuleDef _ (* All of these return nil *)
  | D MethodDef _
  | D ClassDef _
  | D Alias _
  | Begin _
  | End _ ->
      let new_s = C.assign lhs (TE (EId Nil)) s.pos in
      C.seq [s;new_s] s.pos

and refactor_method_call_assign (acc:stmt acc) (lhs : lhs option) = function
  | Ast.Call(Ast.Id(("retry",p1),Ast.ID_Lowercase), (_, [], _), None) ->
      acc_enqueue (C.retry p1) acc
  | Ast.Call(Ast.Id(("redo",p1),Ast.ID_Lowercase), (_, [], _), None) ->
      acc_enqueue (C.redo p1) acc

  | Ast.Call(Ast.Id(("next",p1),Ast.ID_Lowercase), (_, args, _), None) ->
      let args = Ast.args_to_exprs args in
      let _envBUG, es = refactor_list refactor_tuple_expr (acc, DQueue.empty) args in
      let tup = make_tuple_option (DQueue.to_list es) in
      acc_enqueue (C.next ?v:tup p1) acc

  | Ast.Call(Ast.Id(("break",p1),Ast.ID_Lowercase), (_, args, _), None) ->
      let args = Ast.args_to_exprs args in
      let _env, es = refactor_list refactor_tuple_expr (acc, DQueue.empty) args in
      let tup = make_tuple_option (DQueue.to_list es) in
      acc_enqueue (C.break ?v:tup p1) acc


  | Ast.Call(Ast.Id(("undef",p1),Ast.ID_Lowercase), _args, None) ->
      Log.fatal (Log.of_tok p1) "undef as method?"

  | Ast.Call(Ast.Id(("defined?", pos),Ast.ID_Lowercase), args, cb) ->
      let _ = Option.map (fun _ -> Log.fatal (Log.of_tok pos) "cb for 'defined?' ??") cb in
      let acc, v = match lhs with
        | None -> fresh acc
        | Some (LId id) -> acc, LId id
        | Some _ -> Log.fatal (Log.of_tok pos) "non-ident = defined?"
      in
      refactor_defined acc v args pos

  | Ast.Call(Ast.Id((("proc"|"lambda" as m),_pos),Ast.ID_Lowercase),(_, [], _),Some cb)->
      let cb' = refactor_codeblock acc cb in
      let cb' = proc_transform cb' in
      let call = C.mcall ?lhs (ID_MethodName m) [] ~cb:cb' () in
      acc_enqueue call acc

  | Ast.Call(Ast.Id(("define_method",pos),Ast.ID_Lowercase),
             (_, [Ast.Arg (Ast.Atom(_tk, Ast.AtomSimple (mname,atompos)))], _),
             Some(Ast.CodeBlock(_,params_o,cb_body))) ->
      let params = Utils.default_opt [] params_o in
      let name = (* H.msg_of_str *) Ast.MethodId ((mname,atompos), Ast.ID_Lowercase) in
      let body = {Ast.body_exprs = cb_body;rescue_exprs=[];ensure_expr=None;else_expr=None} in
      let e' = Ast.D (Ast.MethodDef(pos, Ast.M name,params,body)) in
      refactor_stmt acc e'

  | Ast.Call(Ast.DotAccess(targ,(_pos2),msg), args, cb) ->
      let acc,targ' = refactor_expr acc targ in
      let acc,msg' = refactor_msg2 acc msg in
      let acc,args',cb' = refactor_method_args_and_cb acc args cb in
      let call = C.mcall ?lhs ~targ:targ' msg' args' ?cb:cb' () in
      acc_enqueue call acc
  (* todo: can factorize with prev case when msg is method_name in both case*)
  | Ast.Call(Ast.ScopedId(Ast.Scope(targ,_pos2,_msg_var_or_methodname_now)), args, cb) ->
      let _acc,targ' = refactor_expr acc targ in
      let acc,msg' = (* refactor_msg acc msg *) raise Todo in
      let acc,args',cb' = refactor_method_args_and_cb acc args cb in
      let call = C.mcall ?lhs ~targ:targ' msg' args' ?cb:cb' () in
      acc_enqueue call acc

  | Ast.Call(msg, args, cb) ->
      let () = match msg, args with
        | Ast.Id((("proc"|"lambda"),pos),Ast.ID_Lowercase),
          (_, [Ast.Arg (Ast.Unary((Ast.Op_UAmper,_),_))], _) ->
            Log.err ~ctx:(Log.of_tok pos) "unsupported proc/lambda with &block param"
        | _ -> ()
      in
      let acc,msg' = match msg with
        | Ast.Id(("super", _),Ast.ID_Lowercase) ->
            acc, ID_Super
        | _ -> refactor_msg acc msg
      in
      let acc,args',cb' = refactor_method_args_and_cb acc args cb in
      let call = C.mcall ?lhs msg' args' ?cb:cb' () in
      acc_enqueue call acc

  | _ -> raise (Invalid_argument "CFG:refactor_method_call_assign")

(* return the accumulator and resulting lhs expression for an assignment *)
and refactor_assignment (acc: stmt acc) (lhs: Ast.expr) (rhs: Ast.expr)
    (pos: Ast.tok) : stmt acc =
  match lhs,rhs with
  (* x[y] = z is really x.[]=(y,z) *)
  | Ast.Call(
    Ast.DotAccess(targ, (_),Ast.MethodOperator(Ast.Op_AREF,_)), (_, args, _),None),
    _ ->
      let acc,targ' = refactor_expr acc targ in
      let acc,rhs_arg = refactor_star_expr acc rhs in
      let acc,(lhs_args: star_expr DQueue.t) = refactor_list refactor_star_expr (acc,DQueue.empty) (args |> Ast.args_to_exprs) in
      let lhs_list = DQueue.to_list lhs_args in
      (* We need to be careful here because the lhs arguments can
         contain a star expression (x[*y] = z) and x.[]=( *y,z) is not
         a valid method call, thus we contruct a separate array for
         the arguments in this case. *)
      let acc,args =
        if List.exists (function SStar _ -> true| _ -> false) lhs_list
        then begin
          (* construct tmp = [*lhs] + [rhs] *)
          let lhs_ary = Array lhs_list in
          let rhs_ary = Array [rhs_arg] in
          let acc, tmp = fresh acc in
          let tmp' = match tmp with LId id -> id | _ -> failwith "Impossible" in

          let call = C.mcall ~lhs:tmp ~targ:(ELit lhs_ary)
              (ID_Operator Op_Plus) [SE (ELit rhs_ary)] ()
          in
          (acc_enqueue call acc), [SStar (EId tmp')]
        end
        else
          (* lhs has no *-exprs, so safe to just concat args *)
          acc, DQueue.to_list (DQueue.enqueue rhs_arg lhs_args)
      in
      let call = C.mcall ~targ:targ' (ID_Operator Op_ASet) args () in
      acc_enqueue call acc

  (* handle x.y = e specially to avoid duping the rhs into a temp *)
  | Ast.Call(Ast.DotAccess(targ,(_),msg),(_, args, _),None), _ ->
      if args <> [] then Log.fatal (Log.of_tok pos) "args on lhs of assignable?";
      let acc,targ' = refactor_expr acc targ in
      let acc,msg' = refactor_msg2 acc msg in
      let msg' = make_assignable_msg msg' in
      let acc,arg = refactor_method_arg_no_cb acc rhs in
      acc_enqueue (C.mcall ~targ:targ' msg' [arg] ()) acc

  | _, Ast.Tuple _ | Ast.Tuple _, _ ->
      let acc,rhs' = refactor_tuple_expr acc rhs in
      let acc,lhs',after = refactor_lhs acc lhs in
      let acc = acc_enqueue (C.assign lhs' rhs' pos) acc in
      acc_append acc after

  (* optimization to eliminate an extra temporary using method assign *)
  | _, (Ast.Call _ as m) ->
      let acc,lhs',after = refactor_lhs acc lhs in
      let acc = refactor_method_call_assign acc (Some lhs') m in
      acc_append acc after

  | e1,        (Ast.Id((s, pos'),Ast.ID_Lowercase) as e)
    when not (StrSet.mem s acc.seen || is_special s) ->
      refactor_assignment acc e1 (Ast.Call(e,fb pos' [],None)) pos

  | _ ->
      let acc,lhs',after = refactor_lhs acc lhs in
      let acc,rhs' = refactor_tuple_expr acc rhs in
      let acc = acc_enqueue (C.assign lhs' rhs' pos) acc in
      acc_append acc after

and refactor_stmt (acc: stmt acc) (e:Ast.expr) : stmt acc =
  match e with
  | Ast.Splat _
  | Ast.Ellipsis _ | Ast.DeepEllipsis _ | Ast.TypedMetavar _
    -> raise Todo
  | Ast.D Ast.Alias(p3, Ast.MethodId((s1, p1),((Ast.ID_Global) as k1)),
                    Ast.MethodId((s2, p2),((Ast.ID_Global) as k2))) ->
      let g1 = (refactor_builtin_or_global p1 k1,s1) in
      let g2 = (refactor_builtin_or_global p2 k2,s2) in
      acc_enqueue (C.alias_g ~link:g1 ~orig:g2 p3) acc

  | Ast.D Ast.Alias(pos, e1,e2) ->
      let acc,e1' = refactor_symbol_or_msg acc e1 in
      let acc,e2' = refactor_symbol_or_msg acc e2 in
      acc_enqueue (C.alias_m ~link:e1' ~orig:e2' pos) acc

  | Ast.D Ast.Undef(pos, e1) ->
      let acc,e1' = refactor_list refactor_symbol_or_msg (acc,DQueue.empty) e1 in
      acc_enqueue (C.undef (DQueue.to_list e1') pos) acc

  | Ast.Ternary(g,pos, t, _, f) ->
      let acc,g' = refactor_expr acc g in
      let tacc = refactor_stmt (acc_emptyq acc) t  in
      let ts = C.seq (DQueue.to_list tacc.q) pos in
      (* propagate seen from tacc to facc for below *)
      let facc = refactor_stmt (acc_emptyq tacc) f  in
      let fs = C.seq (DQueue.to_list facc.q) pos in
      let acc = acc_seen acc facc in
      acc_enqueue (mkstmt (If(g',ts,fs)) pos) acc

  | Ast.S Ast.If(pos, g,t,f) ->
      let f = Ast.opt_stmts_to_stmts f in
      let acc,g' = refactor_expr acc g in
      let tacc = refactor_stmt_list (acc_emptyq acc) t in
      let ts = C.seq (DQueue.to_list tacc.q) pos in
      (* propagate seen from tacc to facc for below *)
      let facc = refactor_stmt_list (acc_emptyq tacc) f in
      let fs = C.seq (DQueue.to_list facc.q) pos in
      let acc = {acc with seen = StrSet.union acc.seen facc.seen} in
      acc_enqueue (C.if_s g' ~t:ts ~f:fs pos) acc

  | Ast.S Ast.Unless(pos, g,f,t) ->
      let t = t |> Ast.opt_stmts_to_stmts in
      let f = Some (pos, f) in
      refactor_stmt acc (Ast.S (Ast.If (pos, g, t, f)))

  | Ast.S Ast.Case(pos, case) -> refactor_case acc case pos

  | Ast.S Ast.Return(pos, args) ->
      let acc,args' = refactor_list refactor_tuple_expr (acc,DQueue.empty)
          (args |> Ast.args_to_exprs) in
      let tup = make_tuple_option (DQueue.to_list args') in
      acc_enqueue (C.return ?v:tup pos) acc

  | Ast.S Ast.Yield(pos, args) ->
      let acc,args' = refactor_list refactor_method_arg_no_cb (acc,DQueue.empty) (args |> Ast.args_to_exprs) in
      acc_enqueue (C.yield ~args:(DQueue.to_list args') pos) acc
  | Ast.S Ast.Break _
  | Ast.S Ast.Next _
  | Ast.S Ast.Redo _
  | Ast.S Ast.Retry _
    -> failwith "TODO"

  | Ast.S Ast.Block(_, el, _) ->
      let blk_acc = refactor_stmt_list (acc_emptyq acc) el in
      let acc = {acc with seen = StrSet.union acc.seen blk_acc.seen} in
      let pos = raise Todo in
      acc_enqueue (C.seq (DQueue.to_list blk_acc.q) pos) acc

  | Ast.Binop(e1,(Ast.Op_ASSIGN, pos2),(Ast.S Ast.Yield(_pos1, args))) ->
      let acc,e1',after = refactor_lhs acc e1 in
      let acc,args' = refactor_list refactor_method_arg_no_cb (acc,DQueue.empty) (args |> Ast.args_to_exprs) in
      let yield_s = C.yield ~lhs:e1' ~args:(DQueue.to_list args') pos2 in
      let acc = acc_enqueue yield_s acc in
      acc_append acc after

  | Ast.Binop(e1,(Ast.Op_ASSIGN, pos),
              Ast.Binop(l,
                        (Ast.B ( Ast.Op_PLUS | Ast.Op_MINUS | Ast.Op_TIMES
                               | Ast.Op_REM  | Ast.Op_DIV   | Ast.Op_CMP
                               | Ast.Op_EQ   | Ast.Op_EQQ   (*| Ast.Op_NEQ*)
                               | Ast.Op_GEQ  | Ast.Op_LEQ   | Ast.Op_LT
                               | Ast.Op_GT   | Ast.Op_BAND  | Ast.Op_BOR
                               | Ast.Op_MATCH (*| Ast.Op_NMATCH*) | Ast.Op_XOR
                               | Ast.Op_POW  | Ast.Op_AREF  | Ast.Op_ASET
                               | Ast.Op_LSHIFT | Ast.Op_RSHIFT
                               as op), _pos1),
                        r)) ->
      let acc,e1',after = refactor_lhs acc e1 in
      let acc = refactor_binop_into_mc acc (Some e1') l (Ast.B op) r pos in
      acc_append acc after

  | Ast.Binop(lhs,(Ast.Op_ASSIGN,pos),rhs) ->
      refactor_assignment acc lhs rhs pos

  (* A::m is really a method call *)
  | Ast.ScopedId(Ast.Scope(_e1,pos,(Ast.SV((_,_),Ast.ID_Lowercase)))) ->
      refactor_method_call_assign acc None (Ast.Call(e,fb pos [], None))

  | Ast.Call _ as m ->
      refactor_method_call_assign acc None m

  (* special case for 'x &&= e' when this is the first assignment to x.
     In this case, x is always nil (the && can't succeed) and the rhs is
     dead code.
  *)
  | Ast.Binop((Ast.Id((s, _pos'),Ast.ID_Lowercase),
               (Ast.Op_OP_ASGN Ast.Op_AND, pos), rhs))
    when not (StrSet.mem s acc.seen || is_special s) ->
      let id' = Var(Local, s) in
      let acc = add_seen s acc in
      Log.err ~ctx:(Log.of_tok pos)
        "removing dead code: %s" (Ast_ruby.show_expr rhs);
      acc_enqueue (C.assign (LId id') (TE (EId Nil)) pos) acc

  (* special case for 'x ||= e' when this is the first assignment to x.
     In this case, x is always set to e (the || can never fail) *)
  | Ast.Binop((Ast.Id((s, _pos'),Ast.ID_Lowercase) as lhs,
               (Ast.Op_OP_ASGN Ast.Op_OR, pos), rhs))
    when not (StrSet.mem s acc.seen || is_special s) ->
      refactor_assignment acc lhs rhs pos

  | Ast.Binop(lhs, (Ast.Op_OP_ASGN Ast.Op_AND, pos), rhs) ->
      let acc, lhs_id = refactor_id acc lhs in
      let asgn_acc = refactor_assignment (acc_emptyq acc) lhs rhs pos in
      let t_branch = C.seq (DQueue.to_list asgn_acc.q) pos in
      let f_branch = C.expr (EId lhs_id) pos in
      let s = C.if_s (EId lhs_id) ~t:t_branch ~f:f_branch pos in
      acc_enqueue s (acc_seen acc asgn_acc)

  | Ast.Binop(lhs, (Ast.Op_OP_ASGN Ast.Op_OR, pos), rhs) ->
      let acc, lhs_id = refactor_id acc lhs in
      let asgn_acc = refactor_assignment (acc_emptyq acc) lhs rhs pos in
      let t_branch = C.expr (EId lhs_id) pos in
      let f_branch = C.seq (DQueue.to_list asgn_acc.q) pos in
      let s = C.if_s (EId lhs_id) ~t:t_branch ~f:f_branch pos in
      acc_enqueue s (acc_seen acc asgn_acc)

  | Ast.Binop(lhs, (Ast.Op_OP_ASGN op, pos), rhs) ->
      refactor_assignment acc lhs (Ast.Binop(lhs,(op,pos),rhs)) pos

  | Ast.Id((s, pos),Ast.ID_Lowercase)
    when not (StrSet.mem s acc.seen || is_special s) ->
      if s = "super" then refactor_super acc None pos
      else refactor_stmt acc (Ast.Call(e,fb pos [],None))

  | Ast.Id _
  | Ast.Literal _ | Ast.Atom _
  | Ast.Tuple _
  | Ast.Hash _
  | Ast.Array _
  | Ast.Unary _
  | Ast.DotAccess _ | Ast.ScopedId _ (* TODO *)
  | Ast.Binop _ as e ->
      let acc,e' = refactor_expr acc e in
      acc_enqueue (C.expr e' (tok_of e)) acc

  | Ast.S Ast.While(pos, true,g,body) -> (* do .. while *)
      let gpos = tok_of g in
      let body_acc = refactor_stmt_list (acc_emptyq acc) body in
      let body_acc,g' = refactor_expr body_acc g in
      let guard = C.if_s g' (C.next gpos) (C.break gpos) gpos in
      let body_acc = acc_enqueue guard body_acc in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      let acc = acc_seen acc body_acc in
      acc_enqueue (C.while_s (EId True) body' pos) acc

  | Ast.S Ast.While(pos, false,g,body) -> (* while .. do *)
      let gpos = tok_of g in
      let while_acc,g' = refactor_expr (acc_emptyq acc) g in
      let body_acc = refactor_stmt_list (acc_emptyq while_acc) body in
      if (DQueue.is_empty while_acc.q) then
        (* preserve guards that are already just expressions *)
        let while_body = C.seq (DQueue.to_list body_acc.q) pos in
        acc_enqueue (C.while_s g' while_body pos) (acc_seen acc body_acc)
      else
        let body' = C.seq (DQueue.to_list body_acc.q) pos in
        let guard = C.if_s g' ~t:body' ~f:(C.break gpos) gpos in
        let while_acc = acc_enqueue guard while_acc in
        let while_body = C.seq (DQueue.to_list while_acc.q) pos in
        let acc = acc_seen acc body_acc in
        acc_enqueue (C.while_s (EId True) while_body pos) acc

  | Ast.S Ast.Until(pos, b,g,body) ->
      refactor_stmt acc (Ast.S (Ast.While(pos, b,Ast.Unary((Ast.Op_UNot,pos),g),body)))

  | Ast.S Ast.For(pos, _pattern,_in, guard, body) ->
      let formals = (* pattern_to_params formals *) raise Todo in
      let acc, formals = refactor_block_formal_list acc formals pos in
      let acc, g_expr = refactor_expr acc guard in
      let body_acc = refactor_stmt_list (acc_emptyq acc) body in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      let acc = acc_seen acc body_acc in
      acc_enqueue (C.for_s formals g_expr body' pos) acc

  | Ast.D Ast.ModuleDef(pos, m, body) ->
      let acc,m' = refactor_id acc (Ast.cmn m) in
      let body_acc = refactor_body (acc_empty acc) body pos in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      acc_enqueue (C.module_s m' body' pos) acc

  | Ast.D Ast.MethodDef(pos, meth,params,body) ->
      let acc,mn = refactor_method_name acc meth in
      let in_acc, params' =
        refactor_method_formal_list (acc_empty acc) params pos
      in
      let body_acc = {(acc_emptyq in_acc) with
                      super_args=gen_super_args params'}
      in
      let body_acc = refactor_body body_acc body pos in
      let body_acc =
        if DQueue.is_empty body_acc.q
        then acc_enqueue (C.expr (EId Nil) pos) body_acc
        (* add an extra nil to the end of the initialization block.
           Otherwise if the method has no body, the result would be the
           last default argument *)
        else body_acc
      in
      let body_acc = {q = DQueue.append in_acc.q body_acc.q;
                      seen = StrSet.union in_acc.seen body_acc.seen;
                      super_args=body_acc.super_args}
      in
      let add_return e = Return (Some e) in
      let body_acc = add_final_return add_return body_acc pos in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      acc_enqueue (C.meth mn params' body' pos) acc

  | Ast.D Ast.ClassDef(pos, Ast.C (clazz, inh), body) ->
      let body_acc = refactor_body (acc_empty acc) body pos in
      begin match inh with
        | None ->
            let body' = C.seq (DQueue.to_list body_acc.q) pos in
            let acc,clazz' = refactor_id acc (Ast.cmn clazz) in
            acc_enqueue (C.nameclass clazz' body' pos) acc

        | Some (_, e) ->
            let body' = C.seq (DQueue.to_list body_acc.q) pos in
            let acc,clazz' = refactor_id acc (Ast.cmn clazz) in
            let acc,e' = refactor_id acc e in
            acc_enqueue (C.nameclass clazz' ~inh:e' body' pos) acc
      end

  | Ast.D Ast.ClassDef(pos, Ast.SingletonC (_, e), body) ->
      let body_acc = refactor_body (acc_empty acc) body pos in

      let acc,e' = refactor_id acc e in
      let body_lst = DQueue.to_list body_acc.q in
      let body' = C.seq body_lst pos in
      acc_enqueue (C.metaclass e' body' pos) acc


  | Ast.D Ast.BeginBlock(pos, lst) ->
      let body_acc = refactor_stmt_list (acc_empty acc) (PI.unbracket lst) in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      acc_enqueue (mkstmt (Begin body') pos) acc

  | Ast.D Ast.EndBlock(pos, lst) ->
      let body_acc = refactor_stmt_list (acc_empty acc) (PI.unbracket lst) in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      acc_enqueue (mkstmt (End body') pos) acc

  | Ast.S Ast.ExnBlock(body) ->
      let pos = raise Todo in
      refactor_body acc body pos

  | Ast.CodeBlock _
  | Ast.Lambda _ as s ->
      Log.fatal (Log.of_tok (tok_of s))
        "refactor_stmt: unknown stmt to refactor: %s\n"
        (Ast_ruby.show_expr s)

and refactor_method_name (acc:stmt acc) e : stmt acc * def_name = match e with
  (* todo: can factorize with next case when same msg type *)
  | Ast.SingletonM (Ast.ScopedId(Ast.Scope(targ,_pos,_msg_var_or_methodnamenow))) ->
      let _acc,targ' = refactor_id acc targ in
      let acc,msg' = (* refactor_msg acc msg *) raise Todo in
      acc, (Singleton_Method (targ',msg'))

  | Ast.SingletonM (Ast.DotAccess(targ,(_pos),msg)) ->
      let acc,targ' = refactor_id acc targ in
      let acc,msg' = refactor_msg2 acc msg in
      acc, (Singleton_Method (targ',msg'))

  | Ast.SingletonM e ->
      Log.fatal (Log.of_tok (tok_of e)) "refactor_method_name unknown msg: %s\n"
        (Ast_ruby.show_expr e)

  | Ast.M (Ast.MethodAtom (_tcolon, Ast.AtomSimple (s, _pos))) ->
      acc, (Instance_Method (ID_MethodName s))
  | Ast.M (Ast.MethodAtom (_tcolon, Ast.AtomFromString (_, [Ast.StrChars (s,_t)],_)))->
      acc, (Instance_Method (ID_MethodName s))

  | Ast.M (Ast.MethodAtom (_tcolon, Ast.AtomFromString (pos, _, _))) ->
      Log.fatal (Log.of_tok pos) "interpreted atom string in method name?"
  | Ast.M e ->
      let acc,id = refactor_msg2 acc e in
      acc, (Instance_Method id)

and refactor_body (acc:stmt acc) b pos : stmt acc =
  if b.Ast.rescue_exprs = [] && b.Ast.ensure_expr = None &&
     b.Ast.else_expr = None
  then refactor_stmt_list acc b.Ast.body_exprs
  else begin
    (* thread the seen set through all parts of the body *)
    let body_acc = refactor_stmt_list (acc_emptyq acc) b.Ast.body_exprs in
    let rescue_acc,resc_rlist =
      List.fold_left
        (fun (acc,lst) resc ->
           let acc, resc' = refactor_rescue pos acc resc in
           acc, resc'::lst
        ) ((acc_emptyq body_acc),[]) b.Ast.rescue_exprs
    in
    let rescue_list = List.rev resc_rlist in
    let ensure_acc =
      refactor_stmt_list (acc_emptyq rescue_acc)
        (b.Ast.ensure_expr |> Ast.opt_stmts_to_stmts)
    in
    let else_acc =
      refactor_stmt_list (acc_emptyq ensure_acc)
        (b.Ast.else_expr  |> Ast.opt_stmts_to_stmts)
    in
    let acc = acc_seen acc else_acc in
    let body = C.seq (DQueue.to_list body_acc.q) pos in
    let eelse =
      if DQueue.is_empty else_acc.q then None
      else Some (C.seq (DQueue.to_list else_acc.q) pos);
    in
    let ensure =
      if DQueue.is_empty ensure_acc.q then None
      else Some (C.seq (DQueue.to_list ensure_acc.q) pos);
    in
    acc_enqueue (C.exnblock body rescue_list ?eelse ?ensure pos) acc
  end

and refactor_rescue pos acc (_, gs, exnvar_opt, rescue_body) : stmt acc * rescue_block =
  let guard_exprs = gs in
  let acc_just_set, rev_guards =
    List.fold_left
      (fun (acc,gl) e ->
         let set, g = refactor_rescue_guard acc e exnvar_opt in
         {acc with seen=set}, g::gl
      ) (acc_emptyq acc,[]) guard_exprs
  in
  let set = acc_just_set.seen in
  let guards = List.rev rev_guards in
  let body_acc = acc_seen {q=DQueue.empty; seen = set;
                           super_args=acc.super_args} acc in
  let body_acc = refactor_stmt_list body_acc rescue_body in
  let body = C.seq (DQueue.to_list body_acc.q) pos in
  let resc_blk = {rescue_guards = guards;rescue_body = body} in
  (acc_seen acc body_acc), resc_blk

and refactor_rescue_guard acc (e:Ast.expr) (exnvar_opt: Ast.exception_variable option) : StrSet.t * rescue_guard =
  match exnvar_opt with
  | Some (pos, bind_e) ->
      let obj = Ast.Id(("StandardError", pos),Ast.ID_Uppercase) in
      refactor_rescue_guard acc (Ast.Binop(obj,(Ast.Op_ASSOC,pos),bind_e))
        exnvar_opt
  | None ->
      (match e with

       | Ast.Binop(exn_e,(Ast.Op_ASSOC,pos),bind_e) ->
           let acc, exn = refactor_tuple_expr acc exn_e in
           let acc, bind_lhs,after = refactor_lhs acc bind_e in
           let () =
             if not (DQueue.is_empty after.q)
             then Log.fatal (Log.of_tok pos) "methodcall in rescue binder??"
           in
           let bind = match bind_lhs with
             | LId id -> id
             | _ -> Log.fatal (Log.of_tok pos) "non-identifier in rescue binder"
           in
           if not (DQueue.is_empty acc.q)
           then begin
             Log.fatal (Log.of_tok pos) "rescue binding created hoisted expression?"
           end;
           acc.seen, Rescue_Bind(exn,bind)

       | Ast.Id(_, Ast.ID_Uppercase)
       | Ast.ScopedId(Ast.Scope(_,(_),_))
       | Ast.ScopedId(Ast.TopScope(((_),_))) ->
           let acc, e' = refactor_tuple_expr acc e in
           if not (DQueue.is_empty acc.q)
           then begin
             DQueue.iter
               (fun s ->
                  Printf.eprintf "resc: %s\n"
                    (CodePrinter.show_stmt s)
               ) acc.q;
             Log.fatal (Log.of_tok (tok_of e))
               "rescue1 guard created hoisted expression?"
           end;
           acc.seen, Rescue_Expr e'

       | e ->
           Log.fixme ~ctx:(Log.of_tok (tok_of e))
             "rescue guard: %s" (Ast_ruby.show_expr e);
           let acc, e' = refactor_tuple_expr acc e in
           if (not (DQueue.is_empty acc.q)) || not (StrSet.is_empty acc.seen)
           then Log.fatal (Log.of_tok (tok_of e))
               "rescue guard created hoisted expression?";
           acc.seen, Rescue_Expr e'
      )

and refactor_method_formal (acc:stmt acc) t _pos : stmt acc * method_formal_param =
  match t with
  | Ast.Formal_hash_splat _
  | Ast.Formal_kwd _
  | Ast.ParamEllipsis _
    -> failwith "TODO"

  | Ast.Formal_id (str,_pos) ->
      let acc = {acc with seen = StrSet.add str acc.seen} in
      acc, Formal_meth_id(str)

  | Ast.Formal_amp (_, (s,_)) ->
      {acc with seen = StrSet.add s acc.seen}, Formal_amp s

  | Ast.Formal_star(_, (str, _)) ->
      let acc = {acc with seen = StrSet.add str acc.seen} in
      acc, Formal_star(str)


  | Ast.Formal_rest _ ->
      let acc, id = fresh acc in
      let s = match id with
        | LId (Var(Local,s)) -> s
        | _ -> assert false
      in
      acc, Formal_star(s)

  | Ast.Formal_tuple(_f_lst) -> Log.fatal Log.empty "refactor_method_formal: formal_tuple?"

  | Ast.Formal_default ((f,_),_, s) ->
      let pos = tok_of s in
      let default_acc = refactor_stmt (acc_emptyq acc) s in
      let acc = acc_seen acc default_acc in
      let acc = {acc with seen = StrSet.add f acc.seen} in
      let s' = C.seq (DQueue.to_list default_acc.q) pos
      in
      match s'.snode with
      | I Expression e ->
          let e' = TE e in
          acc, Formal_default(f, (e' : tuple_expr))
      | _ ->
          let def = Atom (sprintf "__rat_default_%d" (fresh_formal())) in
          let eql = ID_MethodName "eql?" in
          let acc, v = fresh acc in
          let v' = match v with LId id -> id | _ -> failwith "Impossible" in

          let formal_id = (Var(Local, f)) in
          let acc = seen_lhs acc (LId formal_id) in
          let s'' = add_last_assign ~do_break:false formal_id s' in
          let blk = [
            C.mcall ~lhs:v ~targ:(EId (C.local f)) eql [SE (ELit def)] ();
            C.if_s (EId v') ~t:s'' ~f:(C.expr (EId Nil) pos) pos;
          ]
          in
          let pre = C.seq blk pos in
          let acc = acc_enqueue pre acc in
          let def' = TE (ELit def) in
          acc, Formal_default (f, def')

and refactor_block_formal acc t pos : stmt acc * block_formal_param = match t with
  | Ast.Formal_hash_splat _
  | Ast.Formal_kwd _
  | Ast.ParamEllipsis _
    -> failwith "TODO"

  | Ast.Formal_id (str,pos) ->
      (add_seen str acc), Formal_block_id(refactor_id_kind pos Ast.ID_Lowercase,str)

  | Ast.Formal_star(_,(s, _)) -> (add_seen s acc), Formal_star2(s)
  | Ast.Formal_rest _ ->
      let acc, id = fresh acc in
      let s = match id with
        | LId (Var(Local,s)) -> s
        | _ -> assert false
      in
      acc, Formal_star2(s)

  | Ast.Formal_tuple(f_lst) ->
      let acc, lst = refactor_block_formal_list acc (PI.unbracket f_lst) pos in
      acc, Formal_tuple lst
  | Ast.Formal_amp _ -> Log.fatal (Log.of_tok pos) "refactor_block_formal: & arg?"
  | Ast.Formal_default _ -> Log.fatal (Log.of_tok pos) "refactor_block_formal: default arg?"

and refactor_block_formal_list acc lst pos =
  refactor_formal_list refactor_block_formal acc lst pos

and refactor_method_formal_list acc lst pos =
  refactor_formal_list refactor_method_formal acc lst pos

and refactor_case acc case pos =
  let acc,g' = match case.Ast.case_guard with
    | None -> acc, EId True
    | Some e -> refactor_expr acc e
  in
  let acc, whens' =
    List.fold_left
      (fun (acc,whens) (_, g,body) ->
         let acc, glist =
           refactor_list refactor_tuple_expr (acc,DQueue.empty) g
         in
         let g' = match DQueue.to_list glist with
           | [] -> assert false
           | [x] -> x
           | lst -> TTup lst
         in
         let body_acc = refactor_stmt_list (acc_emptyq acc) body in
         let body' = C.seq (DQueue.to_list body_acc.q) pos in
         let acc = acc_seen acc body_acc in
         acc, acc_enqueue (g',body') whens
      ) (acc,acc_emptyq acc) case.Ast.case_whens
  in
  let else' = refactor_stmt_list (acc_emptyq whens')
      (case.Ast.case_else |> Ast.opt_stmts_to_stmts) in
  let default = if DQueue.is_empty else'.q
    then None else Some (C.seq (DQueue.to_list else'.q) pos)
  in
  let case = C.case g' (DQueue.to_list whens'.q) ?default pos in
  acc_enqueue case (acc_seen acc else')

and refactor_stmt_list (acc:stmt acc) lst : stmt acc =
  List.fold_left refactor_stmt acc lst


let refactor_ast ?env ast =
  let seen = default_opt StrSet.empty env in
  let acc = {q = DQueue.empty; seen = seen;super_args=None} in
  let acc = refactor_stmt_list acc ast in
  match DQueue.to_list acc.q with
  | [] -> empty_stmt ()
  | (hd::_) as lst -> C.seq lst (pos_of hd)




(*
val re_init : unit -> unit
  (** Re-initialize the global state of the module that is used to
      provide unique identifier names.  Should only be necessary for
      testing. *)

val reparse : ?env:Utils_ruby.StrSet.t -> ?filename:string -> ?lineno:int ->
  (Il_ruby.stmt,'a) Il_ruby_printer.CodeUnparser.fmt -> 'a
  (** This function takes an unparsing descript as described in
      Cfg_printer and converts the unparsed string back into a
      Cfg.stmt, allowing for easy tree rewriting as describedy by
      Demaille, Levillain, and Sigoure's SAC'09 paper.  For example,

      open Cfg_printer.CodeUnparser
      reparse (s"x = " ++ expr) my_expr

      returns the statement that assigns [my_expr] into the variable
      [x].
  *)

val kreparse : ?env:Utils_ruby.StrSet.t -> ?filename:string -> ?lineno:int ->
  (Il_ruby.stmt -> 'a) -> ('a,'b) Il_ruby_printer.CodeUnparser.fmt -> 'b
  (** Similar to reparse, but passes the final stmt to the supplied
      continuation *)

val freparse : ?env:Utils_ruby.StrSet.t -> ?filename:string -> ?lineno:int ->
  ('a, Format.formatter, unit, Il_ruby.stmt) format4 -> 'a
  (** Similar to reparse, but uses a format string instead *)

val kfreparse : ?env:Utils_ruby.StrSet.t -> ?filename:string -> ?lineno:int ->
  (Il_ruby.stmt -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** Similar to kreparse, but uses a format string instead *)
*)


(*
let kreparse ?env ?filename ?lineno cont k =
  let module U = Il_ruby_printer.CodeUnparser in
  let cont str =
    let ast = Parse_ruby.parse_string ?env ?filename ?lineno str in
      cont (refactor_ast ast ?env)
  in
    U.ksformat cont k

let reparse ?env ?filename ?lineno k =
  kreparse ?env ?filename ?lineno Utils.id k

let kfreparse ?env ?filename ?lineno cont =
  Log.kfsprintf
    (fun str ->
       let ast = Parse_ruby.parse_string ?env ?filename ?lineno str in
         cont (refactor_ast ast ?env)
    )

let freparse ?env ?filename ?lineno =
  kfreparse ?env ?filename ?lineno Utils.id
*)

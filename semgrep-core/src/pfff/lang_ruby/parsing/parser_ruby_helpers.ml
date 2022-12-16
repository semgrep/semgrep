open Ast_ruby
module Utils = Utils_ruby
module Ast_printer = Ast_ruby

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Generic helpers (could be in common.ml or utils_ruby.ml) *)
(*****************************************************************************)

let uniq_list cmp lst =
  let rec u = function
    | [] -> []
    | [x] -> [x]
    | x1::x2::tl ->
        if cmp x1 x2 = 0
        then u (x1::tl)
        else x1 :: (u (x2::tl))
  in
  u (List.sort cmp lst)

let fb = Parse_info.fake_bracket

(*****************************************************************************)
(* Lexer/Parser extra state *)
(*****************************************************************************)

let state_override = ref false
let begin_override () =
  let b = !state_override in
  state_override := false;
  b

module Env = Utils.StrSet

let env_stack =
  let s = Stack.create () in
  Stack.push Env.empty s;
  s

let enter_scope _dyp =
  Stack.push Env.empty env_stack

let leave_scope _dyp =
  ignore(Stack.pop env_stack)

let clear_env () =
  Stack.clear env_stack;
  enter_scope ()

let set_env new_env =
  Stack.clear env_stack;
  Stack.push new_env env_stack

let env () = Stack.top env_stack

let assigned_id id = Env.mem id (env())

let seen_str _dyp id =
  let env = Stack.pop env_stack in
  Stack.push (Env.add id env) env_stack

let rec seen dyp = function
  | Id((s, _), ID_Lowercase) -> seen_str dyp s
  | Tuple(es) -> List.iter (seen dyp) es
  | Array(_, es,_) -> es |> args_to_exprs |> List.iter (seen dyp)
  | _ -> ()

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

(* turn %w{a b c} into ["a";"b";"c"]  *)
let bslash_spc_re = Str.regexp "\\\\ "
let ws_re = Str.regexp "[ \t\r\n]+"

let split_single_string_to_array str pos =
  let chunks =
    List.map (fun chunk -> Str.split ws_re chunk)
      (Str.split_delim bslash_spc_re str)
  in
  let strings =
    let rec reduce acc chunks = match acc, chunks with
      | _, [] -> List.rev acc (* done *)
      | [], [chunk] -> chunk  (* no / found *)
      | [], []::chunks_t -> (* space in the front??? *)
          reduce [""] chunks_t
      | [], chunks_h::chunks_t -> (* other first iter *)
          reduce (List.rev chunks_h) chunks_t
      | acc_h::acc_t, []::chunks_t ->
          reduce ((acc_h ^ " ")::acc_t) chunks_t
      | acc_h::acc_t, chunks_h::chunks_t ->
          let first = List.hd chunks_h in
          let rest_rev = List.rev(List.tl chunks_h) in
          reduce (rest_rev @ ((acc_h ^ " " ^ first)::acc_t)) chunks_t
    in reduce [] chunks
  in
  let strings = List.map
      (fun s -> Arg (Literal(String(Single (s, pos))))) strings
  in
  Array(pos, strings,pos) (* TODO pos1 *)

(* turn %W{a#{b} c} into ["a#{b}"; "c"] *)
let split_double_string_to_array sc pos =
  let ds xs = Literal(String(Double (pos, xs, pos))) in
  (* first we create a stream of tokens with the grammar of
     (Expr | Code | String | Delmi)*
     by splitting the strings on whitespace.  This stream will be
     in reverse order.
  *)
  let rec tokenize acc = function
    | [] -> acc
    | (Ast_ruby.StrExpr e)::tl -> tokenize ((`Expr e)::acc) tl
    | (Ast_ruby.StrChars (s, t))::tl ->
        let splits = Str.full_split ws_re s in
        let acc =
          List.fold_left
            (fun acc -> function
               | Str.Text s -> (`String (s,t))::acc
               | Str.Delim _ -> `Delim::acc
            ) acc splits
        in tokenize acc tl
  in
  (* then we split the (reversed) stream at the delimeters, which
     mark the entries in the array.  This produces a list in the
     correct order. *)
  let rec parse acc curr = function
    | [] ->
        if curr = []
        then acc (* delim at end *)
        else (ds curr)::acc
    | `Delim::tl ->
        if curr = [] then parse acc curr tl (* delim at start *)
        else parse ((ds curr)::acc) [] tl
    | (`Expr x)::tl -> parse acc ((Ast_ruby.StrExpr x)::curr) tl
    | (`String x)::tl -> parse acc ((Ast_ruby.StrChars x)::curr) tl
  in
  let toks = tokenize [] sc in
  let lst = parse [] [] toks in
  Array(pos, lst |> List.map (fun x -> Arg x), pos) (* TODO pos1 *)

let str_of_interp sc = match sc with
  | []  -> ""
  | [Ast_ruby.StrChars (s,_)] -> s
  | _ -> failwith "unexpected escapes in string"

let merge_string_lits s1 s2 = match s1,s2 with
  | Literal(String(s1)), Literal(String(s2)) ->
      let s' = match s1, s2 with
        | Tick _, _ | _, Tick _ -> assert false
        | Single (s1, t1), Single (s2, _t2) ->
            let t = t1 in (* TODO *)
            Single (s1 ^ s2, t)
        | Double (l, sc1, r), Double (_, sc2, _) -> Double (l, sc1 @ sc2, r)
        | Single (s, t), Double (l, sc, r) ->
            let t = t in (* TODO *)
            Double (l, (Ast_ruby.StrChars (s, t))::sc, r)
        | Double (l, sc, r), Single (s, t) ->
            let t = t in (* TODO *)
            Double (l, sc @ [Ast_ruby.StrChars (s, t)], r)
      in
      Literal(String (s'))
  | _ -> assert false

let process_user_string m str pos = match m with
  | "r" -> Literal(Regexp ((pos, str, pos),None))
  | "w" -> split_single_string_to_array (str_of_interp str) pos
  | "W" -> split_double_string_to_array str pos
  | "q" -> Literal(String((Single (str_of_interp str, pos))))
  | "Q" -> Literal(String((Double (pos, str, pos))))
  | "x" -> Literal(String((Tick (pos, str, pos))))
  | "" -> Literal(String((Double (pos, str, pos))))
  | _ -> failwith (Printf.sprintf "unhandled string modifier: %s" m)


(*****************************************************************************)
(* Well formed checking *)
(*****************************************************************************)

let rec starts_with = function
  | Binop(l,_,_) -> starts_with l
  | Call(l,_,_) -> starts_with l
  | Ternary(l,_,_,_,_) -> starts_with l
  | e -> e

let rec ends_with = function
  | Binop(_,_,r) -> ends_with r
  | Call(m,(_,[], _),None) -> ends_with m
  | Ternary(_,_,_, _, r) -> ends_with r
  | e -> e

let rec replace_end expr new_e = match expr with
  | Binop(l,(o,p),r) -> Binop(l,(o,p),replace_end r new_e)
  | Call(m,(lp, [], rp),None) -> Call(replace_end m new_e,(lp,[],rp),None)
  | Ternary(g,p1, l,p2, r) -> Ternary(g,p1, l,p2, replace_end r new_e)
  | _e -> new_e

let is_cond_modifier = function
  | S If _ | S Unless _ | S Until _ | S While _ -> true
  | _ -> false

let well_formed_do guard _body = match ends_with guard with
  | Call(_,_,Some (CodeBlock((_,false,_),_,_))) ->
      raise Dyp.Giveup
  | _ ->()

let well_formed_return args =
  let args = args_to_exprs args in
  match args with
  | [] -> ()
  | hd::_tl ->
      if is_cond_modifier (Utils.last args) then raise Dyp.Giveup;
      match starts_with hd with
      (* f(x) should be not be f(x)
         needed e.g. f(x)[y]
      *)
      | S Block _ -> raise Dyp.Giveup
      | _ -> ()

let well_formed_command _m args =
  let args = args_to_exprs args in
  match args with
  | [] -> ()
  (* f(x) should be not be f(x)
     needed e.g. f(x)[y] *)
  | [S Block _] -> raise Dyp.Giveup
  | _ -> if List.exists is_cond_modifier args then raise Dyp.Giveup

let _hash_literal_as_args args =
  let rec work acc lst = match lst with
    | [] -> acc
    | (Binop(_,(Op_ASSOC,p),_))::_tl ->
        let rec hash_args acc = function
          | [] -> acc, None
          | [Unary((Op_UAmper,_),_) as blk] -> acc, Some blk
          | (Binop(_,(Op_ASSOC,_),_) as hd)::tl ->
              hash_args (hd::acc) tl
          | _ -> raise Dyp.Giveup
        in
        let args,blk = hash_args [] lst in
        let acc = Hash(false, (p, List.rev args, p))::acc in
        let acc = match blk with
          | None -> acc
          | Some b -> b::acc
        in acc

    | hd::tl -> work (hd::acc) tl
  in
  List.rev (work [] args)

let rec methodcall m (lp, args, rp) cb =
  (* old: x.y used to be parsed as a Call (DotAccess ...) because
   * that is the semantic of Ruby, but we do not do the same
   * in Parse_ruby_tree_sitter.ml and in the semgrep context it's better
   * to not be too clever and rewrite things too much.
  *)
  if args = [] && cb = None
  then m
  (* failwith "do not use methodcall if it's not a call" *)
  else
    (* let args = hash_literal_as_args args in *)
    match m,args,cb with
    | S Return(_), [], None -> m
    | S Return(p,[]),args,None -> S (Return(p, args))
    | S Yield(_), [], None -> m
    | S Yield(p,[]),args,None -> S (Yield(p, args))
    | Literal(Bool(true,_p)), [],None
    | Literal(Bool(false,_p)),[],None
    | Id((_,_p),_),     [],None -> m

    | Literal _,_,_ -> raise Dyp.Giveup

    | ScopedId(Scope(_x,(_),_y)),[],None -> m

    | DotAccess(x,(p),y),_,_ -> Call(unfold_dot x y p, (lp, args, rp), cb)
    | _ -> Call(m,(lp, args, rp),cb)

and unfold_dot l r pos =
  match l with
  (* unfold nested a.b.c to become (a.b()).c() *)
  | DotAccess(a,(p),b) ->
      let l' = methodcall (unfold_dot a b p) (fb pos []) None in
      DotAccess(l',(pos),r)

  | _ -> DotAccess(l,(pos),r)

and check_for_dot = function
  | DotAccess(l,(p),r) -> methodcall (unfold_dot l r p) (fb p []) None
  | e -> e

and scope tk l r =
  let l = check_for_dot l in
  ScopedId(Scope(l, (tk), r))


let command_codeblock cmd cb =
  match cmd with
  | Call(c,args,None) -> Call(c,args,Some cb)
  | DotAccess(_,(p),_)
  | ScopedId(Scope(_,(p),_)) -> Call(cmd,fb p [],Some cb)
  | Id((_,p),_) -> Call(cmd,fb p [],Some cb)
  | _ -> raise Dyp.Giveup

(* sometimes the lexer gets can't properly handle x!= as x(!=) and
   erronously produces (x!)= *)
let fix_broken_neq l op r =
  let default = l, op, r in
  match op with
  | Op_ASSIGN ->
      begin match ends_with l with
        (* bugfix: do not transform $! *)
        | Id(("$!", _), ID_Global (*ID_Builtin*)) -> default

        | Id((s,p), k) ->
            let len = String.length s in
            if s.[len-1] == '!'
            then
              let s' = String.sub s 0 (len-1) in
              let l' = replace_end l (Id((s',p),k)) in
              l', B Op_NEQ, r
            else default
        | _ -> default
      end
  | _ -> default

(* sometimes the lexer gets can't properly handle x=> as x(=>) and
   erronously produces (x=)> *)
let fix_broken_assoc l op r =
  let default = l, op, r in
  match op with
  | B Op_GT -> begin match ends_with l with
(*
  | Id((s,p), ID_Assign ik) ->
      let l' = replace_end l (Id((s,p),ik)) in
        l', Op_ASSOC, r
*)
    | Atom(tk, AtomFromString(lt, sc, rt)) ->
        let (astr, t), rest = match List.rev sc with
          | (Ast_ruby.StrChars (s, t))::tl -> (s,t),tl
          | _ -> ("a", Parse_info.fake_info tk "a"),[]
        in
        let len = String.length astr in
        if astr.[len-1] == '='
        then
          let s' = String.sub astr 0 (len-1) in
          let sc' = List.rev ((Ast_ruby.StrChars (s',t))::rest) in
          let l' = replace_end l ((Atom(tk, AtomFromString(lt, sc', rt)))) in
          l', Op_ASSOC, r
        else default
    | _ -> default
  end
  | _ -> default

(*****************************************************************************)
(* Pruning *)
(*****************************************************************************)

let expr_priority = function
  | Unary((U Op_UBang,_),_) | Unary((U Op_UTilde,_),_)| Unary((U Op_UPlus,_),_) -> 2000
  | Unary((U Op_UMinus,_),_) -> 1900
  | Binop(_,(B Op_POW,_),_) -> 1800
  | Binop(_,(B Op_DIV,_),_) | Binop(_,(B Op_REM,_),_) | Binop(_,(B Op_TIMES,_),_) -> 1700
  | Binop(_,(B Op_MINUS,_),_) -> 1500
  | Binop(_,(B Op_PLUS,_),_) -> 1500
  | Binop(_,(B Op_LSHIFT,_),_) | Binop(_,(B Op_RSHIFT,_),_) -> 1400
  | Binop(_,(B Op_BAND,_),_) -> 1300
  | Binop(_,(B Op_BOR,_),_) | Binop(_,(B Op_XOR,_),_) -> 1200

  | Binop(_,(B Op_LEQ,_),_) | Binop(_,(B Op_LT,_),_)
  | Binop(_,(B Op_GEQ,_),_) | Binop(_,(B Op_GT,_),_) -> 1100

  | Binop(_,(B Op_MATCH,_),_) | Binop(_,(B Op_NMATCH,_),_) | Binop(_,(B Op_NEQ,_),_)
  | Binop(_,(B Op_CMP,_),_) | Binop(_,(B Op_EQ,_),_) | Binop(_,(B Op_EQQ,_),_) -> 1000

  | Binop(_,(B Op_DOT2,_),_) | Binop(_,(Op_DOT3,_),_) -> 800

  | Binop(_,(Op_AND,_),_) -> 750
  | Binop(_,(Op_OR,_),_) -> 700

  | Ternary _ -> 650

  | Binop(_,(Op_ASSIGN,_),_) | Binop(_,(Op_OP_ASGN _,_),_) -> 600

  | Binop(_,(Op_ASSOC,_),_) -> 400

  | Unary((Op_UNot,_),_) -> 200
  | Binop(_,(Op_kAND,_),_) | Binop(_,(Op_kOR,_),_) -> 100

  | Binop _ | Unary _ | _ -> max_int

let binop_priority = function
  | Unary _ -> max_int
  | e -> expr_priority e


let prune_uop uop arg pos =
  let e = Unary((uop,pos),arg) in
  let p = expr_priority e in
  let p' = expr_priority arg in
  if p' < p then raise Dyp.Giveup
  else e

let prune_right_assoc tk l op r =
  let l,op,r = fix_broken_neq l op r in
  let l,op,r = fix_broken_assoc l op r in
  let e = Binop(l,(op,tk),r) in
  let p = binop_priority e in
  let pl = binop_priority l in
  let pr = binop_priority r in
  if pr < p || pl <= p
  then raise Dyp.Giveup
  else e

(* right: (x - y) - z
   prune: x - (y - z)
*)
let prune_left_assoc tk l op r =
  let l,op,r = fix_broken_neq l op r in
  let l,op,r = fix_broken_assoc l op r in
  let e = Binop(l,(op, tk),r) in
  match l,op,r with
  | _, _, Binop(_,(Op_ASSIGN,_),_) ->  e

  | _ ->
      let p = binop_priority e in
      let pl = binop_priority l in
      let pr = binop_priority r in
      if pr <= p || pl < p
      then raise Dyp.Giveup
      else e

let prune_tern e1 e2 e3 pos1 pos2 =
  let e = Ternary(e1,pos1, e2,pos2, e3) in
  let p = expr_priority e in
  let p1 = expr_priority e1 in
  (*Printf.eprintf "tern: %s\n" (Ast_printer.string_of_expr e);*)
  if p1 <= p then raise Dyp.Giveup
  else e


(*****************************************************************************)
(* Generic merge helpers *)
(*****************************************************************************)

let do_fail s l to_s =
  let len = List.length l in
  if len > 1 then begin
    Printf.eprintf "<%s>: %d\n" s len;
    List.iter (fun x -> Printf.eprintf " %s\n" (to_s x)) l;
    failwith s
  end

(* wrapper to adapt to new dypgen merge interface *)
let wrap xs f =
  match xs with
  | [] -> failwith "Impossible: Empty list in merge"
  | (x,gd,od)::rest ->
      let l = x::(List.map (fun (x, _, _) -> x) rest ) in
      f l, gd, od


(*****************************************************************************)
(* Merge helpers *)
(*****************************************************************************)

let rec rhs_do_codeblock = function
  | Call(_,_,Some (CodeBlock((_,false,_),_,_))) -> true
  | Binop(_,_,r)
  | Call(r,(_,[],_),None)
  | Ternary(_,_,_, _, r) -> rhs_do_codeblock r
  | Hash(false,(_, el,_)) -> rhs_do_codeblock (Utils.last el)

  | e ->
      Printf.eprintf "got: %s\n" (Ast_printer.show_expr e);
      false

let resolve_block_delim with_cb no_cb = match with_cb,no_cb with
  | _, Call(_,(_, [], _),None) ->
      Printf.eprintf "here2??\n";[with_cb;no_cb]
  | Call(_m1',_args1,Some _do_block),
    Call(_m2',(_, args_ne, _),None) ->
      (* look for cmd arg1,...,(argn do block end) *)
      if rhs_do_codeblock (Utils.last args_ne |> arg_to_expr)
      then [with_cb]
      else [with_cb;no_cb]
  | _ -> assert false

(*****************************************************************************)
(* Merger *)
(*****************************************************************************)

let merge_binop xs =
  wrap xs (fun xs ->
    let newest, l = List.hd xs, List.tl xs in
    let l' = uniq_list compare_expr l in
    let fail () =
      let l' = uniq_list compare_expr (newest::l') in
      do_fail "binop" l' Ast_printer.show_expr;
      l'
    in
    let rec nested_assign = function
      | Binop(_,((Op_ASSIGN|Op_OP_ASGN _),_),_) -> true
      | Binop(_,_,(Binop _ as r)) -> nested_assign r
      | _ -> false
    in
    match l',newest with
    | [Binop(_,(Op_ASSIGN,_),_)], Binop(_,(Op_ASSIGN,_),_) ->
        Printf.eprintf "fail1\n";
        fail ()

    | [Binop(l,_,_)], correct when nested_assign l -> [correct]
    | [correct], Binop(l,_,_) when nested_assign l -> [correct]

    | _ -> Printf.eprintf "fail2\n";fail()
  )

let merge_topcall xs =
  wrap xs (fun xs ->
    let newest, l = List.hd xs, List.tl xs in

    let l' = uniq_list compare_expr l in
    match l',newest with
    | [(Call(_,_,Some (CodeBlock((_,false,_),_,_))) as with_cb)],
      (Call(_,_,None) as no_cb)
    | [(Call(_,_,None) as no_cb)],
      (Call(_,_,Some (CodeBlock((_,false,_),_,_))) as with_cb) ->
        (* resolve "x y{z}" vs "x y do z end" *)
        resolve_block_delim with_cb no_cb;
    | _ ->
        let l' = uniq_list compare_expr (newest::l') in
        do_fail "topcall" l' Ast_printer.show_expr;
        l'
  )

let merge_stmt xs =
  wrap xs (fun xs ->
    let newest, l = List.hd xs, List.tl xs in

    let l' = uniq_list compare_expr l in
    match l',newest with
    | [(Call(_,_,Some (CodeBlock((_,false,_),_,_))) as with_cb)],
      (Call(_,_,None) as no_cb)
    | [(Call(_,_,None) as no_cb)],
      (Call(_,_,Some (CodeBlock((_,false,_),_,_))) as with_cb) ->
        (* resolve "x y{z}" vs "x y do z end" *)
        resolve_block_delim with_cb no_cb;

    | [S ExnBlock({body_exprs = [Binop(_,(Op_ASSIGN,_),_)]; _})],
      (Binop(_,(Op_ASSIGN,_),(S ExnBlock _)) as correct)
    | ([Binop(_,(Op_ASSIGN,_),(S ExnBlock _)) as correct]),
      S ExnBlock({body_exprs = [Binop(_,(Op_ASSIGN,_),_)]; _}) ->
        (* x = y rescue 3 is a special case where the rescue binds
           solely to "y" and not the full assignment *)
        [correct]

    | [S ExnBlock({body_exprs = [Binop(_,(Op_OP_ASGN _,_),_)]; _}) as correct],
      Binop(_,(Op_OP_ASGN _,_),(S ExnBlock _))
    | [Binop(_,(Op_OP_ASGN _,_),(S ExnBlock _))],
      (S ExnBlock({body_exprs = [Binop(_,(Op_OP_ASGN _,_),_)]; _}) as correct) ->
        (* However, using any other assign-operator, reverts to the
               other semantics *)
        [correct]

    (* top-level assignment has a higher priority than any other op *)
    | [Binop(l,((Op_ASSIGN|Op_OP_ASGN _ as op),pos),r)], (Binop _ | Ternary _) ->
        let l,op,r = fix_broken_neq l op r in
        [Binop(l,(op,pos),r)]

    (* we can't use is_cond_modifier to check for a rescue modifier,
       so we do it here *)
    | [S If(_, S ExnBlock _,_,_) | S Unless(_, S ExnBlock _,_,_)
      | S Until(_, _,S ExnBlock _,_) | S While(_, _,S ExnBlock _,_)],
      (S ExnBlock _ as correct)
    | [(S ExnBlock _ as correct)],
      (S If(_, S ExnBlock _,_,_) | S Unless(_, S ExnBlock _,_,_)
      | S Until(_, _,S ExnBlock _,_) | S While(_, _,S ExnBlock _,_)) ->
        [correct]

    | _ ->
        let l' = uniq_list compare_expr (newest::l') in
        do_fail "stmt" l' Ast_printer.show_expr;
        l'
  )


let merge_expr s xs =
  wrap xs (fun xs ->
    let l' = uniq_list compare_expr xs in
    do_fail s l' Ast_printer.show_expr;
    l'
  )

let merge_argument s xs =
  wrap xs (fun xs ->
    let l' = uniq_list compare_argument xs in
    do_fail s l' Ast_printer.show_argument;
    l'
  )

let merge_method_name s xs =
  wrap xs (fun xs ->
    let l' = uniq_list compare_method_name xs in
    do_fail s l' Ast_printer.show_method_name;
    l'
  )

let merge_method_kind s xs =
  wrap xs (fun xs ->
    let l' = uniq_list compare_method_kind xs in
    do_fail s l' Ast_printer.show_method_kind;
    l'
  )


let merge_expr_list s xs =
  wrap xs (fun xs ->
    let l' = uniq_list compare_stmts (xs) in
    do_fail s l' Ast_printer.show_program;
    l'
  )

let merge_argument_list s xs =
  wrap xs (fun xs ->
    let l' = uniq_list compare_arguments (xs) in
    do_fail s l' Ast_printer.show_arguments;
    l'
  )

let merge_formal_list s xs =
  wrap xs (fun xs ->
    let l' = uniq_list compare (xs) in
    do_fail s l' (fun _x -> "??");
    l'
  )

let merge_rest s xs =
  wrap xs (fun xs ->
    let l' = xs in
    do_fail s l' (fun _x -> "??");
    l'
  )

let merge_tok_stmts_opt s xs = merge_rest s xs

let merge_rescue s xs =
  wrap xs (fun xs ->

    let cmp = Ast_ruby.compare_rescue_clause in
    let l' = uniq_list cmp xs in
    do_fail s l' Ast_ruby.show_rescue_clause;
    l'
  )

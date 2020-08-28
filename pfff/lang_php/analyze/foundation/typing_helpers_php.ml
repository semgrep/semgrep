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
open Ast_php
open Env_typing_php
module G = AST_generic

module Pp = Pretty_print_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* s =~ ".*" ^ env.marker *)
let has_marker env s =
  let marker_size = String.length env.marker in
  String.length s >= marker_size &&
  String.sub s (String.length s - marker_size) marker_size = env.marker

(* s =~ "\\(.*\\)" ^ env.marker *)
let get_marked_id env s =
  let marker_size = String.length env.marker in
  let s = String.sub s 0 (String.length s - marker_size) in
  s

(*****************************************************************************)
(* Code database *)
(*****************************************************************************)

module Classes: sig
  val add: env -> string -> Ast_php.class_def -> unit
  val get: env -> string -> Ast_php.class_def
  val mem: env -> string -> bool
  val remove: env -> string -> unit
  val iter: env -> (Ast_php.class_def -> unit) -> unit
end = struct

  let add env n x =
    env.db.classes := SMap.add n (Common2.serial x) !(env.db.classes)

  let get env n =
    let x = SMap.find n !(env.db.classes) in
    Common2.unserial x

  let remove env x =
    env.db.classes := SMap.remove x !(env.db.classes)

  let mem env n = SMap.mem n !(env.db.classes)
  let iter env f = SMap.iter (fun n _ -> f (get env n)) !(env.db.classes)
end

module Functions: sig
  val add: env -> string -> Ast_php.func_def -> unit
  val get: env -> string -> Ast_php.func_def
  val mem: env -> string -> bool
  val remove: env -> string -> unit
  val iter: env -> (Ast_php.func_def -> unit) -> unit
end = struct

  let add env n x =
    env.db.funcs := SMap.add n (Common2.serial x) !(env.db.funcs)

  let get env n =
    let x = SMap.find n !(env.db.funcs) in
    Common2.unserial x

  let remove env x =
    env.db.funcs := SMap.remove x !(env.db.funcs)

  let mem env n = SMap.mem n !(env.db.funcs)
  let iter env f = SMap.iter (fun n _ -> f (get env n)) !(env.db.funcs)


end

(*****************************************************************************)
(* TEnv, GEnv, Subst, Env *)
(*****************************************************************************)

(* global variables, functions and classes.
 * todo: constants?
 *)
module GEnv: sig

  val get_class: env -> string -> t
  val set_class: env -> string -> t -> unit

  val get_fun: env -> string -> t
  val set_fun: env -> string -> t -> unit

  val get_global: env -> string -> t

  val mem_class: env -> string -> bool
  val mem_fun: env -> string -> bool

  val remove_class: env -> string -> unit
  val remove_fun: env -> string -> unit

  val iter: env -> (string -> t -> unit) -> unit

  val save: env -> out_channel -> unit
  val load: in_channel -> env

end = struct

  let get env str =
    try SMap.find str !(env.globals)
    with Not_found ->
      (* todo: error when in strict mode? *)
      Tvar (fresh())

  let set env x t = env.globals := SMap.add x t !(env.globals)
  let unset env x = env.globals := SMap.remove x !(env.globals)
  let mem env x = SMap.mem x !(env.globals)

  let get_class env x = get env ("^Class:"^x)
  let get_fun env x = get env ("^Fun:"^x)

  let get_global env x =
    let x = "^Global:"^x in
    if SMap.mem x !(env.globals)
    then get env x
    else
      let v = Tvar (fresh()) in
      set env x v;
      v

  let set_class env x t = set env ("^Class:"^x) t
  let set_fun env x t = set env ("^Fun:"^x) t

  let remove_class env x = unset env ("^Class:"^x)
  let remove_fun env x = unset env ("^Fun:"^x)

  let mem_class env x = mem env ("^Class:"^x)
  let mem_fun env x = mem env ("^Fun:"^x)

  let iter env f = SMap.iter f !(env.globals)

  let save env oc =
    Marshal.to_channel oc env []
  let load ic =
    Marshal.from_channel ic
end

(* local variables *)
module Env = struct
  let set env x t = env.vars := SMap.add x t !(env.vars)
  let unset env x = env.vars := SMap.remove x !(env.vars)
  let mem env x = SMap.mem x !(env.vars)

  let iter env f = SMap.iter f !(env.vars)

  let get env x =
    try SMap.find x !(env.vars) 
    with Not_found ->
      let n = Tvar (fresh()) in
      set env x n;
      n
end

module TEnv = struct
  let get env x = 
    try IMap.find x !(env.tenv) 
    with Not_found -> Tsum []
  let set env x y = env.tenv := IMap.add x y !(env.tenv)
  let mem env x = IMap.mem x !(env.tenv)
end

(*ciara*)
module AEnv = struct
  let print_id id = 
    match id with 
    | (e, f, c) -> 
        Printf.printf "In %s %s, " c f;
        let stmt = Ast_php.Expr(e, G.sc) in
        let s = Ast_php.show_program [stmt] in
        Common.pr s

  let set env x a =
    let l = try AMap.find x !(env.aenv) with Not_found -> [] in
    let l = a::l in
    env.aenv := AMap.add x l !(env.aenv)
  let iter env f = AMap.iter f !(env.aenv)
  let set_fun env x = env.aenv_fun <- x
  let get_fun env = env.aenv_fun
  let set_class env x = env.aenv_class <- x
  let get_class env = env.aenv_class
 
  let set_funs env x = 
    env.aenv_funs := x::!(env.aenv_funs)
  let get_funs env = !(env.aenv_funs)

  let set_params env p = env.aenv_params := p
  let get_params env = env.aenv_params
  let create_ai_params env pl = 
    List.iter (fun x ->
      let id = (Id [(x.p_name)], get_fun env, get_class env) in
      env.aenv_params := id::!(env.aenv_params);
    ) pl

  let create_ai env e = 
    match e with 
    | Obj_get(IdSpecial(This, _), _, Id[(s, tok)]) -> 
      (Id[(("$"^s), tok)], "", get_class env)
    | _ -> (e, get_fun env, get_class env)

end

module Subst = struct

  let set env x y = env.subst := IMap.add x y !(env.subst)
  let mem env x = IMap.mem x !(env.subst)

  let rec get env x =
    let x' = try IMap.find x !(env.subst) with Not_found -> x in
    if x = x'
    then x
    else
      let x'' = get env x' in
      set env x x'';
      x''

  let rec replace env stack x y =
    if ISet.mem x stack
    then ()
    else if mem env x
    then
      let x' = get env x in
      set env x y;
      replace env (ISet.add x stack) x' y
    else
      set env x y

  let replace env x y = replace env ISet.empty x y

end

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

module Fun = struct

  let rec is_fun env stack = function
    | Tvar n ->
        let n = Subst.get env n in
        if ISet.mem n stack
        then false
        else is_fun env (ISet.add n stack) (TEnv.get env n)
    | Tsum l ->
        (try List.iter (function Tfun _ -> raise Exit | _ -> ()) l; false
        with Exit -> true)

  let rec get_args env stack t =
    match t with
    | Tvar n ->
        let n = Subst.get env n in
        if ISet.mem n stack then [] else
        let stack = ISet.add n stack in
        get_args env stack (TEnv.get env n)
    | Tsum l -> get_prim_args env stack l

  and get_prim_args env stack = function
    | [] -> []
    | Tfun (l, _) :: _ -> l
    | _ :: rl -> get_prim_args env stack rl

end

module FindCommonAncestor = struct

  exception Found of string

  let rec class_match env cand acc id =
    Classes.mem env id &&
    let c = Classes.get env id in
    match c.c_extends with
    | None -> false
    | Some ht ->
      let (s, _) = name_of_class_name ht in
      if s = cand 
      then true
      else class_match env cand acc s

  let rec get_candidates env acc id =
    let acc = SSet.add id acc in
    if not (Classes.mem env id) 
    then acc
    else
      let c = Classes.get env id in
      (match c.c_extends with 
      | None -> acc
      | Some ht ->
        let (s, _) = name_of_class_name ht in
        get_candidates env acc s
      )

  let go env ss =
    let l = SSet.fold (fun x y -> x :: y) ss [] in
    let cands = List.fold_left (get_candidates env) SSet.empty l in
    try SSet.iter (fun cand ->
      let all_match = List.fold_left (class_match env cand) false l in
      if all_match then raise (Found cand)
    ) cands;
    None
    with Found c -> Some c

end


(*****************************************************************************)
(* String of *)
(*****************************************************************************)

module Print2 = struct

  let rec ty env penv stack depth x =
    match x with
    | Tvar n ->
        let n = Subst.get env n in
        let t = TEnv.get env n in
        if ISet.mem n stack then begin
          Pp.print penv (string_of_int n);
          Pp.print penv "&";
        end
        else begin
          let stack = ISet.add n stack in
          ty env penv stack depth t
        end
    | Tsum [] -> Pp.print penv "_"
    | Tsum [x] -> prim_ty env penv stack depth x
    | Tsum l ->
        Pp.list penv (fun penv -> prim_ty env penv stack depth) "(" l " |" ")"

  and prim_ty env penv stack depth = function
    | Tabstr s -> Pp.print penv s
    | Tsstring _s -> Pp.print penv "string"
    | Tienum _
    | Tsenum _ ->
(*        let l = SSet.fold (fun x acc -> Tabstr x :: acc) s [] in *)
        Pp.print penv "enum"
    | Trecord m ->
        let depth = depth + 1 in
        Pp.print penv "array";
        if depth >= 2
        then Pp.print penv "(...)"
        else
          let l = SMap.fold (fun x y l -> (x, y) :: l) m [] in
          Pp.list penv
            (fun penv ->
              print_field env " => " penv stack depth)
            "(" l ";" ")";
    | Tarray (_, t1, t2) ->
        Pp.print penv "array(";
        Pp.nest penv (fun penv ->
          ty env penv stack depth t1;
          Pp.print penv " => ";
          Pp.nest penv (fun penv ->
            ty env penv stack depth t2));
        Pp.print penv ")";
    | Tfun (tl, t) ->
        Pp.print penv "fun ";
        Pp.list penv (
        fun penv (s, x) ->
          ty env penv stack depth x;
          if s = "" then () else
          (Pp.print penv " ";
           Pp.print penv s)
       ) "(" tl "," ")";
        Pp.print penv " -> ";
        Pp.nest penv (fun penv ->
          ty env penv stack depth t)
    | Tobject m ->
        let depth = depth + 1 in
        Pp.print penv "object";
        if depth >= 3
        then Pp.print penv "(...)"
        else
          let l = SMap.fold (fun x y l -> (x, y) :: l) m [] in
          Pp.list penv (fun penv -> print_field env ": " penv stack depth) 
            "(" l ";" ")"
    | Tclosed (s, _) ->
        if SSet.cardinal s = 1 then Pp.print penv (SSet.choose s) else
        (match FindCommonAncestor.go env s with
        | None ->
            let l = SSet.fold (fun x acc -> x :: acc) s [] in
            Pp.list penv (Pp.print) "(" l "|" ")";
        | Some s -> Pp.print penv s)

  and print_field env sep penv stack depth (s, t) =
    Pp.print penv s;
    Pp.print penv sep;
    Pp.nest penv (fun penv ->
      ty env penv stack depth t)

  let genv env =
    let penv = Pp.empty print_string in
    GEnv.iter env (
    fun x t ->
      if not (SSet.mem x !(env.builtins)) then begin
        Pp.print penv x; Pp.print penv " = ";
        ty env penv ISet.empty 0 t;
        Pp.newline penv;
      end
       )

  let penv env =
    genv env


 (* let line_of_opt_info = function 
    | None -> "Line unavailable"
    | Some(pi) -> string_of_int (Parse_info.line_of_info pi)
  
  let access a = 
    let penv = Pp.empty print_string in
    match a with
    | (l, NoIndex _ ) -> Pp.print penv ("  No Index access at
    "^(line_of_opt_info l))
    | (l, VarOrInt _) -> Pp.print penv ("  Var or int access at
    "^(line_of_opt_info l))
    | (l, Const _ ) -> Pp.print penv ("  Const access at "^(line_of_opt_info l))
    | (l, ConstantString _) -> Pp.print penv ("  Constant string access at
    "^(line_of_opt_info l))
    | (l, Declaration _) -> Pp.print penv ("  Array declaration at
    "^(line_of_opt_info l))
    | (_, DeclarationValue _) -> ()
    | (_, DeclarationKValue _) -> ()
    | (l, Value _) -> Pp.print penv ("  Value at"^(line_of_opt_info l))

    (*ciara: added, pretty prints the list of the arr accesses*)
  let arr_access a = 
    let penv = Pp.empty print_string in
    List.iter (
      fun x ->
        access x;
        Pp.newline penv;
    ) a

    (*ciara: added*)
  let print_arr_info env =
    let penv = Pp.empty print_string in
    AEnv.iter env (
      fun x a -> 
        Pp.print penv "get rep string 2 \n";(*Pp.print penv x;*) Pp.print penv " = ";
        Pp.newline penv;
        arr_access a;
    )

  (*ciara: Added, Prints all locals*)
  let aenv env = 
    let penv = Pp.empty print_string in
    Env.iter env (
      fun x t ->
          Pp.print penv x; Pp.print penv " = ";
          ty env penv ISet.empty 0 t;
          Pp.newline penv;
    )
*)
  let args o env t =
    match Fun.get_args env ISet.empty t with
    | [] -> ()
    | tl ->
        let penv = Pp.empty o in
        let stack = ISet.empty in
        let depth = 1000 in
        Pp.list penv (
        fun penv (s, x) ->
          if s = "" then
            ty env penv stack depth x
          else begin
            if x = Tsum [] then () else ty env penv stack depth x;
            (Pp.print penv " ";
             Pp.print penv s)
          end
       ) "(" tl "," ")"

  let rec get_fields vim_mode env stack acc = function
    | Tvar n ->
        let n = Subst.get env n in
        if ISet.mem n stack then SSet.empty else
        let stack = ISet.add n stack in
        let t = TEnv.get env n in
        get_fields vim_mode env stack acc t
    | Tsum l -> List.fold_left (get_prim_fields vim_mode env stack) acc l

  and get_prim_fields vim_mode env stack acc = function
    | Tabstr _ -> acc
    | Tsstring s -> SSet.union s acc
    | Tienum s
    | Tsenum s -> SSet.union s acc
    | Tobject m ->
        SMap.fold (
        fun x t acc ->
          if x = "__obj" then acc else
          let x =
            (* pad: old = 'if not vim_mode && Fun.is_fun env ISet.empty t' *)

            if vim_mode || true then
              if Fun.is_fun env ISet.empty t
              then
                (match Fun.get_args env ISet.empty t with
                | [] -> x^"()"
                | _ ->
                    x^"("
                )
              else x
            else (* not vim_mode *)
              let buf = Buffer.create 256 in
              let o = Buffer.add_string buf in
              let penv = Pp.empty o in
              ty env penv stack 0 t;
              x^"\t"^(Buffer.contents buf)
          in
          SSet.add x acc
       ) m acc
    | Tclosed (s, m) ->
        let acc =
          try
            if SSet.cardinal s = 1
            then (match GEnv.get_class env (SSet.choose s) with
            | Tsum [Tobject m] ->
                get_fields vim_mode env stack acc (SMap.find "__obj" m)
            | _ -> acc)
            else acc
          with _ -> acc
        in
        get_prim_fields vim_mode env stack acc (Tobject m)
    | Trecord m ->
        SMap.fold (fun x _ acc -> SSet.add x acc) m acc
    | Tarray (s, t, _) ->
        let acc = SSet.union s acc in
        let acc = get_fields vim_mode env stack acc t in
        acc
    | Tfun _ -> acc


  let get_fields vim_mode env t =
    let acc = get_fields vim_mode env ISet.empty SSet.empty t in
    acc

end

module Print = struct

  let rec print o env stack = function
    | Tvar n ->
        let n = Subst.get env n in
        if IMap.mem n stack
        then if env.debug then (o "rec["; o (string_of_int n); o "]") else o "rec"
        else if TEnv.mem env n
        then begin
          if env.debug then (o "["; o (string_of_int n); o "]");
          let stack = IMap.add n true stack in
          print o env stack (TEnv.get env n)
        end
        else
            (o "`"; o (string_of_int n))
    | Tsum [] -> o "*"
    | Tsum l ->
        sum o env stack l

  and print_prim o env stack = function
    | Tabstr x -> o x
    | Tienum s -> o "ienum{"; SSet.iter (fun x -> o " | "; o x) s; o " | }"
    | Tsstring s -> o "cstring{"; SSet.iter (fun x -> o " | "; o x) s; o " | }"
    | Tsenum s -> o "senum{"; SSet.iter (fun x -> o " | "; o x) s; o " | }"
    | Trecord m ->
        o "r{";
        SMap.iter (
        fun x t ->
          o x;
          if env.debug then
            (o ":"; print o env stack t);
          o ","
       ) m;
        o "}";
    | Tarray (_, t1, t2) ->
        o "array(";
        print o env stack t1;
        o " => ";
        print o env stack t2;
        o ")"
    | Tfun (l, t) ->
        o "(";
        list o env stack l;
        o " -> ";
        print o env stack t;
        o ")"
    | Tobject m ->
        o "obj"; print_prim o env stack (Trecord m)
    | Tclosed (_, m) -> print_prim o env stack (Tobject m)

  and list o env stack l =
    match l with
    | [] -> o "()"
    | [_, x] -> print o env stack x
    | (_, x) :: rl -> print o env stack x; o ", "; list o env stack rl

  and sum o env stack l =
    match l with
    | [] -> ()
    | [x] -> print_prim o env stack x
    | x :: rl -> print_prim o env stack x; o " | "; sum o env stack rl

  let dd env x =
    print print_string env IMap.empty x;
    print_string "\n"

  let genv env =
    GEnv.iter env (
    fun x t ->
      if not (SSet.mem x !(env.builtins)) then begin
        print_string x; print_string " = ";
        print print_string env IMap.empty t;
        print_string "\n";
      end
       ) ; flush stdout

  let penv env =
    Printf.printf "*******************************\n";
    genv env;
    if env.debug then
      SMap.iter (
      fun x t ->
        if not (SSet.mem x !(env.builtins)) then begin
          print_string x; print_string " = ";
          print print_string env IMap.empty t;
          print_string "\n";
        end
     ) !(env.vars);
    flush stdout

  let show_type env o t =
    Print2.ty env (Pp.empty o) ISet.empty 0 t;
    o "\n"
end

module Type_string = struct

  
  let rec ty env stack depth x = 
    match x with 
    | Tvar n -> 
        let n = Subst.get env n in 
        let t = TEnv.get env n in 
        if ISet.mem n stack then
          (string_of_int n)^"&"
        else begin
          let stack = ISet.add n stack in
          ty env stack depth t
        end
    | Tsum[] -> "_"
    | Tsum [x] -> prim_ty env stack depth x
    | Tsum l -> tsum_list env stack depth l

  and prim_ty env stack depth = function
    | Tabstr s -> s
    | Tsstring _s -> "string"
    | Tienum _ 
    | Tsenum _ -> "enum"
    | Trecord m -> 
        let depth = depth + 1 in 
        let str = "array" in
        if depth >= 2 
        then str^"(...)" 
        else 
          let l = SMap.fold (fun x y l -> (x,y)::l) m [] in
          record_list env stack depth l
    | Tarray (_, t1, t2) -> 
        let str = "array(" in
        let str = str^(ty env stack depth t1) in
        let str = str^" => " in
        let str = str^(ty env stack depth t2) in
        str^")"
    | Tfun (tl, t) -> 
        let str = "fun " in
        let str = str^(fun_list env stack depth tl) in
        str^" -> "^(ty env stack depth t)
    | Tobject m -> 
        let depth = depth + 1 in
        let str = "object" in
        if depth >= 3 then 
          str^"(...)"
        else 
          let l = SMap.fold (fun x y l -> (x, y) :: l) m [] in
          str^(object_list env stack depth l)
    | Tclosed (s, _) -> 
        if SSet.cardinal s = 1 then SSet.choose s else
        (match FindCommonAncestor.go env s with
        | None -> 
            let l = SSet.fold (fun x acc -> x :: acc) s [] in
            closed_list l
        | Some s -> s)
            
    and tsum_list env stack depth l = 
      let tys = List.map (fun t -> prim_ty env stack depth t) l in
      let str_of_ty_list = List.fold_left (fun a b -> a^b^" |" ) "(" tys in
      (Str.string_before str_of_ty_list ((String.length str_of_ty_list) - 2))^")"

    and record_list env stack depth l =
      let tys = List.map (fun x -> match x with 
        | (s, t) -> s^" => "^(ty env stack depth t)) l in
      let str_of = List.fold_left (fun a b -> a^b^";" ) "(" tys in
      (Str.string_before str_of ((String.length str_of) - 2))^")"

    and fun_list env stack depth l = 
      let tys = List.map (fun (s, x) -> (ty env stack depth x)^" "^s ) l in
      let str_of = List.fold_left (fun a b -> a^b^", ") "(" tys in
      (Str.string_before str_of ((String.length str_of) - 2))^")"
  
    and object_list env stack depth l =
      let tys = List.map (fun x -> match x with 
        | (s, t) -> s^": "^(ty env stack depth t)) l in
      let str_of = List.fold_left (fun a b -> a^b^";" ) "(" tys in
      (Str.string_before str_of ((String.length str_of) - 2))^")"

    and closed_list l =
      let str_of = List.fold_left (fun a b -> a^b^"|" ) "(" l in
      (Str.string_before str_of ((String.length str_of) - 2))^")"
end

(*****************************************************************************)
(* Instantiate/Generalize/Normalize *)
(*****************************************************************************)

module Instantiate = struct

  let rec get_vars env stack subst = function
    | Tvar n ->
        let n = Subst.get env n in
        (match TEnv.get env n with
        | _ when ISet.mem n stack ->
            ISet.add n subst
        | Tsum [] -> ISet.add n subst
        | t -> get_vars env (ISet.add n stack) subst t
        )
    | Tsum l -> List.fold_left (get_prim_vars env stack) subst l

  and get_prim_vars env stack subst = function
    | Trecord m ->
        SMap.fold (
        fun _ t subst ->
          get_vars env stack subst t
       ) m subst
    | Tarray (_, t1, t2) ->
        let subst = get_vars env stack subst t1 in
        let subst = get_vars env stack subst t2 in
        subst
    | _ -> subst

  let rec replace_vars env stack subst is_left = function
    | Tvar n ->
        let n = Subst.get env n in
        if IMap.mem n subst then Tvar (IMap.find n subst) else
        (match TEnv.get env n with
        | _ when ISet.mem n stack -> Tsum []
        | t -> replace_vars env (ISet.add n stack) subst is_left t
        )
    | Tsum l when List.length l > 1 -> Tsum []
    | Tsum l -> Tsum (List.map (replace_prim_vars env stack subst is_left) l)

  and replace_prim_vars env stack subst is_left = function
    | Trecord m -> Trecord (SMap.map (replace_vars env stack subst is_left) m)
    | Tarray (s, t1, t2) ->
        let t1 = replace_vars env stack subst is_left t1 in
        let t2 = replace_vars env stack subst is_left t2 in
        Tarray (s, t1, t2)
    | x -> x

  let rec ty env stack t =
    match t with
    | Tvar x ->
        let x = Subst.get env x in
        let t = TEnv.get env x in
        if ISet.mem x stack then Tvar x else
        let stack = ISet.add x stack in
        TEnv.set env x (ty env stack t);
        Tvar x
    | Tsum tyl -> Tsum (List.map (prim_ty env stack) tyl)

  and prim_ty env _stack = function
    | Tfun (tl, t) ->
        let argl = List.map snd tl in
        let vars = List.fold_left (get_vars env ISet.empty) ISet.empty argl in
        let vars = ISet.fold (fun x acc -> IMap.add x (fresh()) acc) vars IMap.empty in
        Tfun (List.map (fun (s, x) -> s, replace_vars env ISet.empty vars true x) tl,
              replace_vars env ISet.empty vars false t)
    | x -> x

  let rec approx env stack t =
    match t with
    | Tvar x ->
        let x = Subst.get env x in
        let t = TEnv.get env x in
        if ISet.mem x stack then Tvar x else
        let stack = ISet.add x stack in
        approx env stack t
    | Tsum [x] -> Tsum (approx_prim_ty env stack x)
    | _ -> Tsum []

  and approx_prim_ty env stack = function
    | Tarray (s, t1, t2) -> [Tarray (s, approx env stack t1, approx env stack t2)]
    | Tobject _
    | Tfun _ -> []
    | x -> [x]

end

module Generalize = struct

  let rec ty env stack = function
    | Tvar n ->
        let n = Subst.get env n in
        if ISet.mem n stack then Tsum [] else
        (match TEnv.get env n with
        | Tsum [Tabstr "null"]
        | Tsum [] -> Tvar n
        | t ->
            ty env (ISet.add n stack) t
        )
    | Tsum l -> Tsum (List.map (prim_ty env stack) l)

  and prim_ty env stack = function
    | Tarray (s, t1, t2) -> Tarray (s, ty env stack t1, ty env stack t2)
    | Tfun (tl, t) -> Tfun (List.map (fun (s, x) -> s, ty env stack x) tl, ty env stack t)
    | x -> x

end

(* This module normalizes a type, that is gets rid of the type variables
 * The problem is, a type can have many equivalents modulo alpha-conversion
   (alpha-conversion is when one renames type variables).
   For instance, f: forall 'a, 'a -> 'a is equivalent to forall 'b, 'b -> 'b
   The normalization gets rid of all these type variables.
   Of course, it is wrong, since every type variable is renamed to (-1).
   But it doesn't matter, we use this function to check the equality of 2 types
   in the unit tests. Since all these types are instantiated, we don't really
   care about the type variables.
*)
module Normalize = struct

  let rec normalize stack env = function
    | Tvar n ->
        let n = Subst.get env n in
        if ISet.mem n stack
        then Tvar (-1)
        else if TEnv.mem env n
        then normalize (ISet.add n stack) env (TEnv.get env n)
        else Tvar (-1)
    | Tsum l -> Tsum (List.map (prim_ty stack env) l)

  and prim_ty stack env t =
    let k = normalize stack env in
    match t with
    | Tsstring _
    | Tabstr _
    | Tienum _
    | Tsenum _ as x -> x
    | Trecord m -> Trecord (SMap.map k m)
    | Tarray (s, t1, t2) -> Tarray (s, k t1, k t2)
    | Tfun (l, t) -> Tfun (List.map (fun (_, t) -> "", k t) l, k t)
    | Tobject obj -> Tobject (SMap.map k obj)
    | Tclosed (s, m) -> Tclosed (s, SMap.map k m)

  let normalize = normalize ISet.empty

end


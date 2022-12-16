(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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
open Common

module E = Entity_code
module G = Graph_code
(*module PI = Parse_info*)

open Ast_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Graph of dependencies for Javascript. See graph_code.ml
 * and main_codegraph.ml for more information.
 *
 * schema:
 *  Root -> Dir -> File -> Function
 *                      -> Class (and Obj)
 *                            -> TODO Field
 *                      -> Const
 *                      -> Global TODO need type to know if not a func|class?
 *       -> Dir -> SubDir -> ...
 *
 * todo:
 *  - look at the deadcode detected by codegraph on the graph built
 *    by this file; they usually contains FP indicating bugs in this file
 *  - too many stuff
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* old: used to be in ast_js.ml *)
(* For bar() in a/b/foo.js the qualified_name is 'a/b/foo.bar'.
 * I remove the filename extension for codegraph (which assumes
 * the dot is a package separator), which is convenient to show
 * shorter names when exploring a codebase (and maybe also when hovering
 * a function in codemap).
 * This is computed after ast_js_build in graph_code_js.ml
*)
type qualified_name = string

(* for the extract_uses visitor *)
type env = {
  g: Graph_code.t;

  phase: phase;

  current: Graph_code.node;
  file_readable: Common.filename;
  root: Common.dirname; (* to find node_modules/ *)

  (* imports of external entities; also abused to create
   * fake imports of the entities defined in the current file *)
  imports: (string, qualified_name (* orig name *)) Hashtbl.t;
  (* 'locals' covers also the parameters; I handle block scope by not using
   * a ref of mutable here! Just build a new list and passed it down.
  *)
  locals: string list;
  (* 'var' has a function scope.
   * alt: lift var up in a ast_js_build.ml transforming phase
  *)
  vars: (string, bool) Hashtbl.t;

  (* less: use it? could use to check if the import match the exports *)
  exports: (Common.filename, string list) Hashtbl.t;

  (* error reporting *)
  dupes: (Graph_code.node, bool) Hashtbl.t;

  (* this is for the abstract interpreter *)
  db: (qualified_name, Ast_js.var) Hashtbl.t;
  asts: (Common.filename (* readable *)* Ast_js.a_program (* resolved*)) list ref;

  log: string -> unit;
  pr2_and_log: string -> unit;
  lookup_fail: env -> Graph_code.node -> Parse_info.t -> unit;
}
and phase = Defs | Uses

(* useful to evaluate the amount of constructs not yet handled *)
let error_recovery = ref true

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* because we use a 2 passes process (should do like in PHP all in 1 pass?) *)

let _hmemo = Hashtbl.create 101

let parse file =
  Common.memoized _hmemo file (fun () ->
    try
      Parse_js.parse_program file
    with
    | Timeout _ as exn -> Exception.catch_and_reraise exn
    | exn ->
        let e = Exception.catch exn in
        pr2 (spf "PARSE ERROR with %s, exn = %s" file (Exception.to_string e));
        if !error_recovery
        then []
        else
          Exception.reraise e
  )

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s tok =
  let err = spf "%s: %s" (Parse_info.string_of_info tok) s in
  failwith err

let s_of_n (s, _) =
  s

let pos_of_tok tok file =
  { (Parse_info.unsafe_token_location_of_info tok) with PI.file }

let is_undefined_ok (src, _kindsrc) (dst, _kinddst) =
  src =~ "^node_modules/.*" ||
  (*dst =~ "^NOTFOUND-.*" || *)
  (* r2c: too specific? *)
  dst =~ "^src/images/"

let fake s = Parse_info.fake_info s

let unbracket (_, x, _) = x

let option = Option.iter

(*****************************************************************************)
(* Qualified Name *)
(*****************************************************************************)

let mk_qualified_name readable s =
  assert (not (readable =~ "^\\./"));
  let str =
    try Filename.chop_extension readable
    with Invalid_argument _ ->
      failwith (spf "readable filename without any extension: %s" readable)
  in
  str ^ "." ^ s

let qualified_name env name =
  (let s = s_of_n name in
   if Hashtbl.mem env.imports s
   then Hashtbl.find env.imports s
   else s
  )|> (fun s -> assert (not (s =~ "^\\./")); s)

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)
let is_local env n =
  let s = s_of_n n in
  List.mem s env.locals || Hashtbl.mem env.vars s

let add_locals env vs =
  let locals = vs |> Common.map_filter (fun (ent, def) ->
    let s = s_of_n ent.name in
    match def with
    | VarDef { v_kind = (Var, _); _} ->
        Hashtbl.replace env.vars s true;
        None
    | _ -> Some s
  ) in
  { env with locals = locals @ env.locals }

let kind_of_expr_opt v_kind eopt =
  match eopt with
  | Some (Fun _) -> E.Function
  | Some (Class _) -> E.Class
  | Some (Obj _) -> E.Class
  | _ ->
      (* without types, this might be wrong; a constant might
       * actually refer to a function, and a global to an object
      *)
      if fst v_kind = Const
      then E.Constant
      else E.Global


(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)
let add_node_and_edge_if_defs_mode env (name, kind) =
  let str = s_of_n name in
  let str' =
    match env.current with
    | (readable, E.File) -> mk_qualified_name readable str
    | (s, _) -> s ^ "." ^ str
  in
  let node = (str', kind) in

  if env.phase = Defs then begin
    match () with
    (* if parent is a dupe, then don't want to attach yourself to the
     * original parent, mark this child as a dupe too.
    *)
    | _ when Hashtbl.mem env.dupes env.current ->
        Hashtbl.replace env.dupes node true

    (* already there? a dupe? *)
    | _ when G.has_node node env.g ->
        env.pr2_and_log (spf "DUPE entity: %s" (G.string_of_node node));
        let orig_file = G.file_of_node node env.g in
        env.log (spf " orig = %s" orig_file);
        env.log (spf " dupe = %s" env.file_readable);
        Hashtbl.replace env.dupes node true;
        (* ok not a dupe, let's add it then *)
    | _ ->
        (* try but should never happen, see comment below *)
        try
          let pos = pos_of_tok (snd name) env.file_readable in
          let nodeinfo = { Graph_code. pos; typ = None; props = []; } in
          env.g |> G.add_node node;
          env.g |> G.add_edge (env.current, node) G.Has;
          env.g |> G.add_nodeinfo node nodeinfo;
          (* this should never happen, but it's better to give a good err msg *)
        with Not_found ->
          error ("Not_found:" ^ str) (snd name)
  end;
  if Hashtbl.mem env.dupes node
  then env
  else { env with current = node }

(*****************************************************************************)
(* Add edges *)
(*****************************************************************************)
let add_use_edge env (name, kind) =
  let s = qualified_name env name in
  let src = env.current in
  let dst = (s, kind) in
  let loc = snd name in
  match () with
  | _ when Hashtbl.mem env.dupes src || Hashtbl.mem env.dupes dst ->
      (* todo: stats *)
      env.pr2_and_log (spf "skipping edge (%s -> %s), one of it is a dupe"
                         (G.string_of_node src) (G.string_of_node dst));

  | _ when not (G.has_node src env.g) ->
      error (spf "SRC FAIL: %s (-> %s)"
               (G.string_of_node src) (G.string_of_node dst)) loc
  (* the normal case *)
  | _ when G.has_node dst env.g ->
      G.add_edge (src, dst) G.Use env.g;
      (* error *)
  | _ -> env.lookup_fail env dst loc

let add_use_edge_candidates env (name, kind) (*scope*) =
  let kind =
    let s = qualified_name env name in
    let dst = (s, kind) in
    if G.has_node dst env.g
    then kind
    else
      let candidates = [E.Function; E.Class; E.Constant; E.Global] in
      let valids = candidates |> List.filter (fun k ->
        G.has_node (s, k) env.g) in
      (match valids with
       | [x] -> x
       | _ -> kind (* default to first kind, but could report error *)
      )
  in
  add_use_edge env (name, kind);
  (* old: I've removed scope info in ast_js, use the generic AST for that
     let s = qualified_name env name in
     scope := Global s;
  *)
  ()


(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)

let rec extract_defs_uses env ast =
  if env.phase = Defs then begin
    let dir = Common2.dirname env.file_readable in
    G.create_intermediate_directories_if_not_present env.g dir;
    let node = (env.file_readable, E.File) in
    env.g |> G.add_node node;
    env.g |> G.add_edge ((dir, E.Dir), node) G.Has;
  end;
  let env = { env with current = (env.file_readable, E.File); } in
  toplevels_entities_adjust_imports env ast;
  toplevels env ast

(* The order of toplevel declarations does not matter in Javascript.
 * It is a dynamic language without static checking; if in the body of
 * a function you use an entity declared further, nothing will complain
 * about it.
 * So we need to first extract all those toplevel entities and
 * create an import for them to not get lookup failures otherwise
 * on those forward uses.
*)
and toplevels_entities_adjust_imports env xs =
  xs |> List.iter (function
    | DefStmt (ent, _def) ->
        let str = s_of_n ent.name in
        Hashtbl.replace env.imports str
          (mk_qualified_name env.file_readable str);
        (*    | M _ | S _ -> () *)
    | _ -> ()
  )

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)
and toplevel env x =
  match x with
  | DefStmt ({name; _}, VarDef { v_kind; v_init; v_type = _}) ->
      name_expr env name v_kind v_init
  (* TODO: other DefStmt now that have separate FuncDef and ClassDef *)
  | M x -> module_directive env x
(*
  | S (tok, st) ->
      let kind = E.TopStmts in
      let s = spf "__top__%d:%d"
          (Parse_info.line_of_info tok) (Parse_info.col_of_info tok) in
      let name = s, tok in
      let env = add_node_and_edge_if_defs_mode env (name, kind) in
      if env.phase = Uses
      then stmt env st
*)
  | _ ->
      stmt env x

and module_directive env x =
  match x with
  | Import (_, names, (file, tok)) ->
      List.iter (fun (name1, name2opt) ->
        if env.phase = Uses then begin
          let str1 = s_of_n name1 in
          let str2_opt = Option.map s_of_n name2opt in
          let path_opt = Module_path_js.resolve_path
              ~root:env.root
              ~pwd:(Filename.dirname env.file_readable)
              file in
          let readable =
            match path_opt with
            | None ->
                env.pr2_and_log (spf "could not resolve path %s at %s" file
                                   (Parse_info.string_of_info tok));
                spf "NOTFOUND-|%s|.js" file
            | Some fullpath -> Common.readable env.root fullpath
          in
          str2_opt |> Option.iter (fun str2 ->
            Hashtbl.replace env.imports str2 (mk_qualified_name readable str1)
          )
        end
      ) names
  | Export (_t, name)
  | ReExportNamespace (_t, _, Some name, _, _) ->
      if env.phase = Defs then begin
        let exports =
          try
            Hashtbl.find env.exports env.file_readable
          with Not_found -> []
        in
        let str = s_of_n name in
        Hashtbl.replace env.exports env.file_readable (str::exports)
      end
  | ReExportNamespace (_t, _, None, _, _file) -> ()
  | ModuleAlias (_, name, _fileTODO) ->
      (* for now just add name as a local; anyway we do not
       * generate dependencies for fields yet
      *)
      let s = s_of_n name in
      Hashtbl.replace env.vars s true;
  | ImportFile (_t, _file) -> ()

and toplevels env xs = List.iter (toplevel env) xs

and name_expr env name v_kind eopt (*v_resolved*) =
  let kind = kind_of_expr_opt v_kind eopt in
  let env = add_node_and_edge_if_defs_mode env (name, kind) in
  if env.phase = Uses
  then begin
    option (expr env) eopt;
    let (qualified, _kind) = env.current in
    (* v_resolved := Global qualified; *)
    Hashtbl.add env.db qualified
      (basic_entity name ,{ v_kind; v_init = eopt; v_type = None; })
  end

(* ---------------------------------------------------------------------- *)
(* Statements *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
  | DefStmt v ->
      let env = add_locals env [v] in
      (match snd v with
       | VarDef { v_init; _ } ->
           option (expr env) v_init
       | _ -> raise Todo
      )
  | M m -> module_directive env m
  | Block (_, xs, _) -> stmts env xs
  | ExprStmt (e, _) -> expr env e
  | If (_, e, st1, st2) ->
      expr env e;
      stmt env st1;
      option (stmt env) st2
  | Do (_, st, e) ->
      stmt env st;
      expr env e;
  | While (_, e, st) ->
      expr env e;
      stmt env st
  | For (_, header, st) ->
      let env = for_header env header in
      stmt env st
  | Switch (_tok, e, xs) ->
      expr env e;
      cases env xs
  | Continue (_, lopt, _) | Break (_, lopt, _) ->
      Option.iter (label env) lopt
  | Return (_, eopt, _) ->
      option (expr env) eopt
  | Label (l, st) ->
      label env l;
      stmt env st
  | Throw (_, e, _) ->
      expr env e
  | Try (_, st1, catchopt, finalopt) ->
      stmt env st1;
      catchopt |> Option.iter (catch_block env);
      finalopt |> Option.iter (fun (_t, st) -> stmt env st);
  | With (_, e, st) ->
      expr env e;
      stmt env st
  | StmtTodo _ -> failwith "StmtTodo"

and catch_block env = function
  | BoundCatch (_t, pat, st) ->
      expr env pat;
      stmt env st
  | UnboundCatch (_t, st) -> stmt env st

and for_header env = function
  | ForClassic (e1, e2, e3) ->
      let env =
        match e1 with
        | Left vars ->
            (* less: need fold_with_env? *)
            vars |> vars_to_stmts |> List.iter (stmt env);
            add_locals env (vars_to_defs vars)
        | Right e ->
            expr env e;
            env
      in
      option (expr env) e2;
      option (expr env) e3;
      env
  | ForIn (e1, _, e2) | ForOf (e1, _, e2) ->
      let env =
        match e1 with
        | Left var ->
            (* less: need fold_with_env? *)
            [var] |> vars_to_stmts |> List.iter (stmt env);
            add_locals env ([var] |> vars_to_defs)
        | Right e ->
            expr env e;
            env
      in
      expr env e2;
      env
  | ForEllipsis _t -> env

(* less: could check def/use of lbls, but less important *)
and label _env _lbl =
  ()

and cases env xs = List.iter (case env) xs

and case env = function
  | Case (_, e, st) ->
      expr env e;
      stmt env st
  | Default (_, st) ->
      stmt env st

and stmts env xs =
  let rec aux env = function
    | [] -> ()
    | x::xs ->
        stmt env x;
        let env =
          match x with
          | DefStmt v -> add_locals env [v]
          | _ -> env
        in
        aux env xs
  in
  aux env xs

(* ---------------------------------------------------------------------- *)
(* Expessions *)
(* ---------------------------------------------------------------------- *)
and expr env e =
  match e with
  | ExprTodo _ | Cast _ | TypeAssert _ -> failwith "ExprTodo|Cast|..."
  | Ellipsis _ | DeepEllipsis _ | ObjAccessEllipsis _ | TypedMetavar _ -> ()

  | L (Bool _ | Num _ | String _ | Regexp _) -> ()
  | Id (n(*, scope*)) ->
      if not (is_local env n)
      then
        (* the big one! *)
        add_use_edge_candidates env (n, E.Global) (*scope*);


  | IdSpecial _ -> ()
  | Assign (e1, _tok, e2) ->
      expr env e1;
      expr env e2

  | Obj o ->
      obj_ env o
  | Arr xs ->
      xs |> unbracket |> List.iter (expr env)
  | Class (c, nopt) ->
      let env =
        match nopt with
        | None -> env
        | Some n ->
            let v = basic_entity n, VarDef { v_kind = Let, fake (snd n) "let";
                                             v_init = None; v_type = None;
                                             (* v_resolved = ref Local *)}
            in
            add_locals env [v]
      in
      class_ env c
  | ObjAccess (e, _, prop) ->
      (match e with
       | Id (n (*, scope*)) when not (is_local env n) ->
           add_use_edge_candidates env (n, E.Class) (*scope*)
       | _ ->
           expr env e
      );
      property_name env prop
  | ArrAccess (e1, (_, e2, _)) ->
      (match e1 with
       | Id (n(*, scope*)) when not (is_local env n) ->
           add_use_edge_candidates env (n, E.Class) (*scope*)
       | _ ->
           expr env e1
      );
      expr env e2

  | Fun (f, nopt) ->
      let env =
        match nopt with
        | None -> env
        | Some n ->
            let v = basic_entity n, VarDef {v_kind = Let, fake (snd n) "let";
                                            v_init = None; v_type = None;
                                            (*v_resolved = ref Local*)}
            in
            add_locals env [v]
      in
      fun_ env f
  | Apply (e, (_, es, _)) ->
      (match e with
       | Id (n(*, scope*)) when not (is_local env n) ->
           add_use_edge_candidates env (n, E.Function) (*scope*)
       | IdSpecial (special, _tok) ->
           (match special, es with
            | _ -> ()
           )
       | _ ->
           expr env e
      );
      List.iter (expr env) es

  | New _ ->  (* TODO *) ()

  | Conditional (e1, e2, e3) ->
      List.iter (expr env) [e1;e2;e3]
  | Xml x -> xml env x

and xml env x =
  (* TODO add_use_edge env ([x.xml_tag], E.Class); *)
  x.xml_attrs |> List.iter (function
    | XmlAttr (_identTODO, _, xhp_attr) ->
        (* TODO add_use_edge_lookup ~xhp:true env ([x.xml_tag], ident) E.Field; *)
        expr env xhp_attr
    | XmlAttrExpr (_, e, _) -> expr env e
    | XmlEllipsis _ -> ()
  );
  x.xml_body |> List.iter (xhp env)

and xhp env = function
  | XmlText _s -> ()
  | XmlExpr e -> option (expr env) (unbracket e)
  | XmlXml x -> xml env x

(* ---------------------------------------------------------------------- *)
(* Entities *)
(* ---------------------------------------------------------------------- *)

(* todo: create nodes if global var? *)
and obj_ env xs =
  xs |> unbracket |> List.iter (property env)

and type_ _env _t = ()

and parent env = function
  | Left e -> expr env e
  | Right t -> type_ env t

and class_ env c =
  List.iter (parent env) c.c_extends;
  List.iter (type_ env) c.c_implements;
  List.iter (property env) (unbracket c.c_body)

and field_classic env {fld_name = pname; fld_body = e; _} =
  property_name env pname;
  option (expr env) e

and property env = function
  | Field v1 -> field_classic env v1
  | FieldColon v1 -> field_classic env v1
  | FieldSpread (_, e) ->
      expr env e
  | FieldPatDefault (pat, _, e) ->
      pattern env pat;
      expr env e
  | FieldEllipsis _ -> ()
  | FieldTodo _ -> failwith "FieldTodo"

and pattern env x = expr env x

and property_name env = function
  | PN _n2 -> ()
  | PN_Computed e ->
      expr env e

and fun_ env f =
  (* less: need fold_with_env here? can not use previous param in p_default? *)
  parameters env f.f_params;
  let params = f.f_params |> Common.map_filter (function
    | ParamClassic p -> Some (s_of_n p.p_name)
    | ParamEllipsis _ -> None
    | ParamPattern _ -> None (* TODO *)
  ) in
  let env = { env with
              locals = params @ env.locals;
              (* new scope, but still inherits enclosing vars *)
              vars = Hashtbl.copy env.vars;
            } in
  stmt env f.f_body

and parameters env xs = List.iter (parameter env) xs
and parameter env = function
  | ParamEllipsis _ -> ()
  | ParamClassic p ->
      Option.iter (expr env) p.p_default
  | ParamPattern _ -> () (* TODO *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build_gen ?(verbose=false) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  (* use Hashtbl.find_all property *)
  let hstat_lookup_failures = Hashtbl.create 101 in

  let chan = open_out_bin (Filename.concat root "pfff.log") in

  let env = {
    g;
    phase = Defs;
    current = G.pb;
    file_readable = "__filled_later__";
    root;

    imports = Hashtbl.create 0;
    locals = [];
    vars = Hashtbl.create 0;
    exports = Hashtbl.create 101;

    dupes = Hashtbl.create 101;
    db = Hashtbl.create 101;
    asts = ref [];

    log = (fun s -> output_string chan (s ^ "\n"); flush chan;);
    pr2_and_log = (fun s ->
      if verbose then pr2 s;
      output_string chan (s ^ "\n"); flush chan;
    );
    lookup_fail = (fun env dst loc ->
      let src = env.current in
      let fprinter =
        if not verbose || is_undefined_ok src dst
        then env.log
        else env.pr2_and_log
      in
      fprinter (spf "PB: lookup_fail on %s (in %s, at %s)"
                  (G.string_of_node dst) (G.string_of_node src)
                  (Parse_info.string_of_info loc));
      (* note: could also use Hashtbl.replace to count entities only once *)
      Hashtbl.add hstat_lookup_failures dst true;
    );

  } in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  (Stdlib_js.path_stdlib::files) |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      let file_readable =
        if file = Stdlib_js.path_stdlib
        then "Stdlib.js"
        else Common.readable ~root file
      in
      extract_defs_uses { env with
                          phase = Defs; file_readable; imports = Hashtbl.create 13;
                        } ast
    ));

  (* step2: creating the 'Use' edges *)

  let default_import =
    let ast = parse Stdlib_js.path_stdlib in
    let env = { env with phase = Uses; file_readable = "Stdlib.js";
                         locals = []; imports = Hashtbl.create 13; } in
    toplevels_entities_adjust_imports env ast;
    env.imports
  in

  env.pr2_and_log "\nstep2: extract uses";
  files |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      let file_readable = Common.readable ~root file in
      extract_defs_uses { env with
                          phase = Uses; file_readable;
                          locals = []; imports = Hashtbl.copy default_import;
                        } ast;
      Common.push (file_readable, ast) env.asts;

    ));

  env.pr2_and_log "\nstep3: adjusting";
  G.remove_empty_nodes g [G.not_found; G.dupe; G.pb];

  (* less: lookup failures summary *)
  let xs = Common2.hkeys hstat_lookup_failures in
  let counts =
    xs |> List.map (fun (x)->
      G.string_of_node x,
      List.length (Hashtbl.find_all hstat_lookup_failures x))
    |> Common.sort_by_val_highfirst
    |> Common.take_safe 20
  in
  pr2 "Top lookup failures per modules";
  counts |> List.iter (fun (s, n) -> pr2 (spf "%-30s = %d" s n));

  (* finally return the graph *)
  g, env.db, !(env.asts)


let build ?verbose root files =
  let (g, _, _) = build_gen ?verbose root files in
  g

(*****************************************************************************)
(* For abstract interpreter *)
(*****************************************************************************)
(* todo: actually probably better to generate the code database on demand
 * while abstract-interpreting the code and its import/require so we
 * can actually even handle some dynamic imports.
*)
let build_for_ai root files =
  let (_, db, asts) = build_gen ~verbose:false root files in
  db, asts

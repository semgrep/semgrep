(* Yoann Padioleau
 *
 * Copyright (C) 2012, 2014 Facebook
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
module Flag = Flag_parsing
module E = Entity_code
module G = Graph_code
module P = Graph_code_prolog
module PI = Parse_info
open Ast_c
module Ast = Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for C (and partially cpp). See graph_code.ml and
 * main_codegraph.ml for more information.
 *
 * See also lang_clang/analyze/graph_code_clang.ml if you can afford yourself
 * to use clang. Lots of code of graph_code_clang.ml have been ported
 * to this file now. Being cpp-aware has many advantages:
 *  - we can track dependencies of cpp constants which is useful in codemap!
 *    With bddbddb we can also track the flow of specific constants to
 *    fields! (but people could use enum in clang to solve this problem)
 *  - we can find dead macros, duplicated macros
 *  - we can find wrong code in ifdef not compiled
 *  - we can detect ugly macros that use locals insteaf of globals or
 *    parameters; again graphcode is a perfect clowncode detector!
 *  - ...
 *
 * schema:
 *  Root -> Dir -> File (.c|.h) -> Function | Prototype
 *                              -> Global | GlobalExtern
 *                              -> Type (for Typedef)
 *                              -> Type (struct|union|enum)
 *                                 -> Field TODO track use! but need type
 *                                 -> Constructor (enum)
 *                              -> Constant | Macro
 *       -> Dir -> SubDir -> ...
 *
 * Note that here as opposed to graph_code_clang.ml constants and macros
 * are present.
 * What about nested structures? they are lifted up in ast_c_build!
 *
 * todo:
 *  - fields!!!!!!! but need type information for expressions
 *  - the management of dupes has become a bit complicated, easy to miss cases,
 *    maybe should actually work at namespace level and consider dupes
 *    constant and functions as they are kind of in the same namespace
 *  - there is different "namespaces" in C?
 *    - functions/locals
 *    - tags (struct name, enum name)
 *    - cpp?
 *    - ???
 *    => maybe we don't need those add_prefix, S__, E__ hacks.
 *  - Type is a bit overloaded maybe (used for struct/union/enum/typedefs)
 *  - and of course improve lang_cpp/ parser, lang_c/ ast builder to cover
 *    more cases, better infer typedef, better handle cpp, etc.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* for the extract_uses visitor *)
type env = {
  g : Graph_code.t; (* now in Graph_code.gensym:  cnt: int ref; *)
  phase : phase;
  current : Graph_code.node;
  c_file_readable : Common.filename;
  ctx : Graph_code_prolog.context;
  (* for prolog use/4, todo: merge in_assign with context? *)
  in_assign : bool;
  (* mostly to remove some warnings on lookup failures *)
  in_define : bool;
  (* for datalog *)
  in_return : bool;
  (* covers also the parameters; the type_ is really only for datalog_c *)
  locals : (string * type_ option) list ref;
  (* for static functions, globals, 'main', and local enums/constants/macros *)
  local_rename : (string, string) Hashtbl.t;
  conf : config;
  (* to accept duplicated typedefs if they are the same, and of course to
   * expand typedefs for better dependencies
   * less: we could also have a local_typedefs field
   *)
  typedefs : (string, Ast.type_) Hashtbl.t;
  (* to accept duplicated structs if they are the same, and at some point
   * maybe also for ArrayInit which should be transformed in a RecordInit.
   *)
  structs : (string, Ast.struct_def) Hashtbl.t;
  (* error reporting *)
  dupes : (Graph_code.node, bool) Hashtbl.t;
  (* for ArrayInit when actually used for structs *)
  fields : (string, string list) Hashtbl.t;
  log : string -> unit;
  pr2_and_log : string -> unit;
}

and phase = Defs | Uses

and config = {
  types_dependencies : bool;
  fields_dependencies : bool;
  macro_dependencies : bool;
  (* We normally expand references to typedefs, to normalize and simplify
   * things. Set this variable to true if instead you want to know who is
   * using a typedef.
   *)
  typedefs_dependencies : bool;
  propagate_deps_def_to_decl : bool;
      (* todo: readable_filenames_in_graph_nodeinfo *)
}

type kind_file = Source | Header

(* for prolog and other code indexer (e.g. syncweb's indexer) *)
let hook_use_edge = ref (fun _ctx _in_assign (_src, _dst) _g _loc -> ())
let hook_def_node = ref (fun _node _g -> ())

(* for datalog *)
let facts = ref None

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* less: could maybe call Parse_c.parse to get the parsing statistics *)
let parse ~show_parse_error file =
  try
    (* less: make this parameters of parse_program? *)
    Common.save_excursion Flag.error_recovery true (fun () ->
        Common.save_excursion Flag.show_parsing_error show_parse_error
          (fun () ->
            Common.save_excursion Flag.verbose_parsing show_parse_error
              (fun () -> Parse_c.parse_program file)))
  with
  | Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
      Exception.reraise e

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s tok = failwith (spf "%s: %s" (Parse_info.string_of_info tok) s)

(* we can have different .c files using the same function name, so to avoid
 * dupes we locally rename those entities, e.g. main -> main__234.
 *)
let new_name_if_defs env (s, tok) =
  if env.phase = Defs then (
    if Hashtbl.mem env.local_rename s then
      error (spf "Duped new name: %s" s) tok;
    let s2 = Graph_code.gensym s in
    Hashtbl.add env.local_rename s s2;
    (s2, tok))
  else (Hashtbl.find env.local_rename s, tok)

(* anywhere you get a string from the AST you must use this function to
 * get the final "value" *)
let str env (s, tok) =
  if Hashtbl.mem env.local_rename s then (Hashtbl.find env.local_rename s, tok)
  else (s, tok)

let add_prefix prefix (s, tok) = (prefix ^ s, tok)

let kind_file env =
  match env.c_file_readable with
  | s when s =~ ".*\\.[h]" -> Header
  | s when s =~ ".*\\.[c]" -> Source
  | _s ->
      (* failwith ("unknown kind of file: " ^ s) *)
      Source

let rec expand_typedefs env t =
  match t with
  | TBase _
  | TStructName _
  | TEnumName _ ->
      t
  | TTypeName name ->
      let s = Ast.str_of_name name in
      if
        Hashtbl.mem env.typedefs s
        && not (Hashtbl.mem env.dupes ("T__" ^ s, E.Type))
      then
        let t' = Hashtbl.find env.typedefs s in
        (* right now 'typedef enum { ... } X' results in X being
         * typedefed to ... itself
         *)
        if t' =*= t then t else expand_typedefs env t'
      else t
  | TPointer (t, x) -> TPointer (t, expand_typedefs env x)
  (* less: eopt could contain some sizeof(typedefs) that we should expand
   * but does not matter probably
   *)
  | TArray (eopt, x) -> TArray (eopt, expand_typedefs env x)
  | TFunction (ret, params) ->
      TFunction
        ( expand_typedefs env ret,
          params
          |> List.map (function
               | ParamClassic p ->
                   ParamClassic { p with p_type = expand_typedefs env p.p_type }
               | ParamDots t -> ParamDots t) )
  | TMacroApply _ -> raise Todo

let final_type env t =
  if env.conf.typedefs_dependencies then t
  else
    (* Can we do that anytime? like in Defs phase?
     * No we need to wait for the first pass to have all the typedefs
     * before we can expand them!
     *)
    expand_typedefs env t

let find_existing_node_opt env name candidates last_resort =
  let s = Ast.str_of_name name in
  let kind_with_a_dupe =
    candidates
    |> Common.find_opt (fun kind ->
           Hashtbl.mem env.dupes (s, kind)
           && kind <> E.Prototype && kind <> E.GlobalExtern)
  in
  let existing_nodes =
    candidates |> List.filter (fun kind -> G.has_node (s, kind) env.g)
  in
  let non_proto_existing_nodes =
    existing_nodes
    |> Common.exclude (fun kind ->
           kind =*= E.Prototype || kind =*= E.GlobalExtern)
  in
  match kind_with_a_dupe with
  (* If there is a dupe, then we don't want to create an edge to an
   * unrelated entity that happens to be unique and listed first
   * in the list of candidates. This is especially useful
   * when processing large codebase globally such as plan9.
   * So in that case we return the kind of the dupe; add_edge() will
   * then do its job and skip the edge.
   *)
  | Some kind_dupe -> Some kind_dupe
  | None -> (
      match existing_nodes with
      (* could not find anything, use last_resort to get a "lookup failure" *)
      | [] -> Some last_resort
      | [ x ] -> Some x
      (* ambiguity, maybe be proto vs def, give priority to kind listed first *)
      | x :: _ :: _ -> (
          match non_proto_existing_nodes with
          | [] -> Some x
          | [ y ] -> Some y
          (* real ambiguity, real dupe across different kind of entity *)
          | x :: y :: _ ->
              env.pr2_and_log
                (spf "skipping edge, multiple def kinds for entity %s (%s<>%s)"
                   s
                   (E.string_of_entity_kind x)
                   (E.string_of_entity_kind y));
              let xfile = G.file_of_node (s, x) env.g in
              let yfile = G.file_of_node (s, y) env.g in
              env.log (spf " orig = %s" xfile);
              env.log (spf " dupe = %s" yfile);
              None))

let is_local env s =
  Common.find_opt (fun (x, _) -> x =$= s) !(env.locals) <> None

let unbracket = PI.unbracket

(*****************************************************************************)
(* For datalog *)
(*****************************************************************************)

(* less: could mv this conrete hooks in datalog_c at some point *)
let with_datalog_env env f =
  !facts
  |> Option.iter (fun aref ->
         let env2 =
           {
             Datalog_c.scope = fst env.current;
             c_file_readable = env.c_file_readable;
             long_format = !Datalog_c.long_format;
             globals = env.g;
             (* need to pass the ref because instrs_of_expr will add new
              * local variables
              *)
             locals = env.locals;
             facts = aref;
             globals_renames = (fun n -> str env n);
           }
         in
         f env2)

let hook_expr_toplevel env_orig x =
  (* actually always called from a Uses phase, but does not hurt to x2 check*)
  if env_orig.phase = Uses then
    with_datalog_env env_orig (fun env ->
        let instrs = Datalog_c.instrs_of_expr env x in
        instrs
        |> List.iter (fun instr ->
               let facts = Datalog_c.facts_of_instr env instr in
               facts
               |> List.iter (fun fact -> Common.push fact env.Datalog_c.facts));
        if env_orig.in_return then
          let fact = Datalog_c.return_fact env (Common2.list_last instrs) in
          Common.push fact env.Datalog_c.facts)

(* To be called normally after each add_node_and_edge_if_defs_mode.
 * subtle: but take care of code that use new_name_if_defs, you must
 * call hook_def after those local renames have been added!
 *)
let hook_def env def =
  if env.phase = Defs then
    with_datalog_env env (fun env ->
        let facts = Datalog_c.facts_of_def env def in
        facts |> List.iter (fun fact -> Common.push fact env.Datalog_c.facts))

(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)

let add_node_and_edge_if_defs_mode env (name, kind) typopt =
  let str = Ast.str_of_name name in
  let str' =
    match (kind, env.current) with
    | E.Field, (s, E.Type) -> s ^ "." ^ str
    | _ -> str
  in
  let node = (str', kind) in

  (if env.phase = Defs then
   match () with
   (* if parent is a dupe, then don't want to attach yourself to the
    * original parent, mark this child as a dupe too.
    *)
   | _ when Hashtbl.mem env.dupes env.current ->
       (* todo: hmm but for a struct, might not detect at first that this
        * will become a dupe later because of another struct with the same
        * name, which means the first set of fields will be added to this
        * soon-to-be dupe struct. Is this why I get some SRC fail later?
        *)
       Hashtbl.replace env.dupes node true
   (* already there? a dupe? *)
   | _ when G.has_node node env.g -> (
       match kind with
       | E.Function
       | E.Global
       | E.Constructor
       | E.Type
       | E.Field
       | E.Constant
       | E.Macro -> (
           match (kind, str) with
           (* dupe typedefs/structs are ok as long as they are equivalent,
            * and this check is done below in toplevel().
            *)
           | E.Type, s when s =~ "[ST]__" -> ()
           | _ when env.c_file_readable =~ ".*EXTERNAL" -> ()
           | _ ->
               env.pr2_and_log (spf "DUPE entity: %s" (G.string_of_node node));
               let orig_file = G.file_of_node node env.g in
               env.log (spf " orig = %s" orig_file);
               env.log (spf " dupe = %s" env.c_file_readable);
               Hashtbl.replace env.dupes node true)
       (* todo: have no Use for now for those so skip errors *)
       | E.Prototype
       | E.GlobalExtern ->
           (* It's common to have multiple times the same prototype declared.
            * It can also happen that the same prototype have
            * different types (e.g. in plan9 newword() had an argument with type
            * 'Word' and another 'word'). We don't want to add to the same
            * entity dependencies to this different types so we need to mark
            * the prototype as a dupe too!
            * Anyway normally we should add the dependency to the Function or
            * Global first so we should hit this code only for really external
            * entities (and when we don't find the Function or Global we will
            * get some "skipping edge because of dupe" errors).
            *)
           Hashtbl.replace env.dupes node true
       | _ -> failwith (spf "Unhandled category: %s" (G.string_of_node node)))
   (* ok not a dupe, let's add it then *)
   | _ -> (
       let typ =
         match typopt with
         | None -> None
         | Some t ->
             (* hmmm can't call final_type here, no typedef PASS yet
                let t = final_type env t in
             *)
             let _s = Ast_c.show_any (Type t) in
             (* hmmm this is fed to prolog so need to be a simple string
              * without special quote in it, so for now let's skip
              *)
             Some "_TODO_type"
       in
       (* try but should never happen, see comment below *)
       try
         let pos = Parse_info.unsafe_token_location_of_info (snd name) in
         (*        let pos = { pos with Parse_info.file = env.c_file_readable } in *)
         let nodeinfo = { Graph_code.pos; typ; props = [] } in
         (* less: hmmm actually it could be a dupe it there is another node
          * with a different def kind and same name. But we should at
          * least warn about in find_existing_node_opt().
          *)
         env.g |> G.add_node node;
         env.g |> G.add_edge (env.current, node) G.Has;
         env.g |> G.add_nodeinfo node nodeinfo;
         !hook_def_node node env.g
         (* this should never happen, but it's better to give a good err msg *)
       with
       | Not_found -> error ("Not_found:" ^ str) (snd name)));
  if Hashtbl.mem env.dupes node then env else { env with current = node }

(*****************************************************************************)
(* Add edge *)
(*****************************************************************************)

let add_use_edge env (name, kind) =
  let s = Ast.str_of_name name in
  let src = env.current in
  let dst = (s, kind) in
  match () with
  | _ when Hashtbl.mem env.dupes src || Hashtbl.mem env.dupes dst ->
      (* todo: stats *)
      env.pr2_and_log
        (spf "skipping edge (%s -> %s), one of it is a dupe"
           (G.string_of_node src) (G.string_of_node dst))
  (* plan9, those are special functions in kencc? *)
  | _ when s =$= "USED" || s =$= "SET" -> ()
  | _ when not (G.has_node src env.g) ->
      error
        (spf "SRC FAIL: %s (-> %s)" (G.string_of_node src)
           (G.string_of_node dst))
        (snd name)
  (* the normal case *)
  | _ when G.has_node dst env.g ->
      G.add_edge (src, dst) G.Use env.g;

      let pos = Parse_info.unsafe_token_location_of_info (snd name) in
      (*     let pos = { pos with Parse_info.file = env.c_file_readable } in *)
      !hook_use_edge env.ctx env.in_assign (src, dst) env.g pos
  (* try to 'rekind'? we use find_existing_node now so no need to rekind *)
  | _ ->
      let prfn = if env.in_define then env.log else env.pr2_and_log in
      prfn
        (spf "Lookup failure on %s (%s)" (G.string_of_node dst)
           (Parse_info.string_of_info (snd name)))
(* todo? still need code below?*)
(*
    | E.Type when s =~ "S__\\(.*\\)" ->
        add_use_edge env ("T__" ^ Common.matched1 s, E.Type)
    | E.Type when s =~ "U__\\(.*\\)" ->
        add_use_edge env ("T__" ^ Common.matched1 s, E.Type)
    | E.Type when s =~ "E__\\(.*\\)" ->
        add_use_edge env ("T__" ^ Common.matched1 s, E.Type)
*)

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)

let rec extract_defs_uses env ast =
  if env.phase = Defs then (
    let dir = Common2.dirname env.c_file_readable in
    G.create_intermediate_directories_if_not_present env.g dir;
    let node = (env.c_file_readable, E.File) in
    env.g |> G.add_node node;
    env.g |> G.add_edge ((dir, E.Dir), node) G.Has);
  let env = { env with current = (env.c_file_readable, E.File) } in
  toplevels env ast

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevel env x = stmt env x

and directive env x =
  match x with
  | Define (_t, name, body) ->
      let name =
        if kind_file env =*= Source then new_name_if_defs env name else name
      in
      let env = add_node_and_edge_if_defs_mode env (name, E.Constant) None in
      hook_def env (DirStmt x);
      if env.phase = Uses && env.conf.macro_dependencies then
        Option.iter (define_body env) body
  | Macro (_t, name, params, body) ->
      let name =
        if kind_file env =*= Source then new_name_if_defs env name else name
      in
      let env = add_node_and_edge_if_defs_mode env (name, E.Macro) None in
      hook_def env (DirStmt x);
      let env =
        {
          env with
          locals =
            ref
              (params |> List.map (fun p -> (Ast.str_of_name p, None (*TAny*))));
        }
      in
      if env.phase = Uses && env.conf.macro_dependencies then
        Option.iter (define_body env) body
  (* less: should analyze if s has the form "..." and not <> and
   * build appropriate link? but need to find the real File
   * corresponding to the string, so may need some -I
   *)
  | Include _ -> ()
  | OtherDirective _ -> ()

and definition env x =
  match x with
  | FuncDef def
  | Prototype def -> (
      let name = def.f_name in
      let typ = Some (TFunction def.f_type) in
      let kind =
        match x with
        | Prototype _ -> E.Prototype
        | FuncDef _ -> E.Function
        | _ -> raise Impossible
      in
      let static =
        (* if we are in an header file, then we don't want to rename
         * the inline static function because would have a different
         * local_rename hash. Renaming in the header file would lead to
         * some unresolved lookup in the c files.
         *)
        (def.f_static && kind_file env =*= Source)
        || Ast.str_of_name name = "main"
      in
      match kind with
      (* todo: when static and prototype, we should create a new_str_if_defs
       * that will match the one created later for the Function, but
       * right now we just don't create the node, it's simpler.
       *)
      | E.Prototype when static -> ()
      | E.Prototype ->
          (* todo: when prototype and in .c, then it's probably a forward
           * decl that we could just skip?
           *)
          add_node_and_edge_if_defs_mode env (name, kind) typ |> ignore
      | E.Function ->
          let name = if static then new_name_if_defs env name else name in
          let env = add_node_and_edge_if_defs_mode env (name, kind) typ in
          type_ env (TFunction def.f_type);
          hook_def env (DefStmt x);
          let xs =
            snd def.f_type
            |> Common.map_filter (function
                 | ParamDots _ -> None
                 | ParamClassic x -> (
                     match x.p_name with
                     | None -> None
                     | Some n -> Some (Ast.str_of_name n, Some x.p_type)))
          in
          let env = { env with locals = ref xs } in
          if env.phase = Uses then stmts env (unbracket def.f_body)
      | _ -> raise Impossible)
  | VarDef v -> (
      let { v_name = name; v_type = t; v_storage = sto; v_init = eopt } = v in
      (* can have code like 'Readfn chardraw;' that looks like a global but
       * is actually a Prototype.
       * todo: we rely on the typedef decl being in the same file and before
       *  because normally typedefs are computed in Defs phase but we
       *  need also in this phase to know if this global is actually a proto
       *)
      let finalt = expand_typedefs env t in
      let typ = Some t (* or finalt? *) in
      let kind =
        match (sto, finalt, eopt) with
        | _, TFunction _, _ -> E.Prototype
        | Extern, _, _ -> E.GlobalExtern
        (* when have 'int x = 1;' in a header, it's actually the def.
         * less: print a warning asking to mv in a .c
         *)
        | _, _, Some _ when kind_file env = Header -> E.Global
        (* less: print a warning; they should put extern decl *)
        | _, _, _ when kind_file env = Header -> E.GlobalExtern
        | DefaultStorage, _, _
        | Static, _, _ ->
            E.Global
      in
      let static =
        sto =*= Static (* && kind_file env =*= Source? need that? *)
      in
      match kind with
      (* see comment above in the FuncDef case *)
      | E.Prototype when static -> ()
      (* note that no need | E.GlobalExtern when static, it can't happen *)
      | E.Prototype
      | E.GlobalExtern ->
          add_node_and_edge_if_defs_mode env (name, kind) typ |> ignore
      | E.Global ->
          let name = if static then new_name_if_defs env name else name in
          let env = add_node_and_edge_if_defs_mode env (name, kind) typ in
          hook_def env (DefStmt x);
          type_ env t;
          if env.phase = Uses then
            eopt
            |> Option.iter (fun e ->
                   let n = name in
                   expr_toplevel env
                     (Assign (Ast_cpp.SimpleAssign (snd n), Id n, e)))
      | _ -> raise Impossible)
  | StructDef def ->
      let { s_name = name; s_kind = kind; s_flds = flds } = def in
      let prefix =
        match kind with
        | Struct -> "S__"
        | Union -> "U__"
      in
      let name = add_prefix prefix name in
      let s = Ast.str_of_name name in

      if env.phase = Defs then
        if Hashtbl.mem env.structs s then
          let old = Hashtbl.find env.structs s in

          (* TODO: need call abstract_line_info here *)
          if
            (* (Meta_ast_c.vof_any (Toplevel (StructDef old))) =*=
               (Meta_ast_c.vof_any (Toplevel (StructDef def))) *)
            failwith "TODO: abstract_line_info and =*= for C, or deriving eq"
            (* Why they don't factorize? because they don't like recursive
             * #include in plan I think
             *)
          then env.log (spf "you should factorize struct %s definitions" s)
          else (
            env.pr2_and_log
              (spf "conflicting structs for %s, %s <> %s" s (Dumper.dump old)
                 (Dumper.dump def));
            Hashtbl.replace env.dupes (fst name, E.Type) true)
        else (
          Hashtbl.add env.structs s def;
          let env = add_node_and_edge_if_defs_mode env (name, E.Type) None in
          hook_def env (DefStmt x);
          (* this is used for InitListExpr *)
          let fields =
            flds |> unbracket
            |> Common.map_filter (function
                 | { fld_name = Some name; _ } -> Some (Ast.str_of_name name)
                 | _ -> None)
          in
          Hashtbl.replace env.fields (prefix ^ s) fields;

          flds |> unbracket
          |> List.iter (fun { fld_name = nameopt; fld_type = t } ->
                 nameopt
                 |> Option.iter (fun name ->
                        add_node_and_edge_if_defs_mode env (name, E.Field)
                          (Some t)
                        |> ignore)))
      else
        let env = add_node_and_edge_if_defs_mode env (name, E.Type) None in
        flds |> unbracket
        |> List.iter (fun { fld_name = nameopt; fld_type = t } ->
               match nameopt with
               | Some name ->
                   let typ = Some t in
                   let env =
                     add_node_and_edge_if_defs_mode env (name, E.Field) typ
                   in
                   type_ env t
               | None ->
                   (* TODO: kencc: anon substruct, invent anon? *)
                   (* (spf "F__anon__%s" (str_of_angle_loc env loc), E.Field) None *)
                   type_ env t)
  | EnumDef { e_name = name; e_consts = xs } ->
      let name = add_prefix "E__" name in
      let env = add_node_and_edge_if_defs_mode env (name, E.Type) None in
      xs
      |> List.iter (fun (name, eopt) ->
             let name =
               if kind_file env =*= Source then new_name_if_defs env name
               else name
             in
             let env =
               add_node_and_edge_if_defs_mode env (name, E.Constructor) None
             in
             if env.phase = Uses then Common2.opt (expr_toplevel env) eopt);
      (* subtle: called here after all the local renames have been created! *)
      hook_def env (DefStmt x)
      (* I am not sure about the namespaces, so I prepend strings *)
  | TypeDef { t_name = name; t_type = t } ->
      let s = Ast.str_of_name name in
      let name = add_prefix "T__" name in
      if env.phase = Defs then
        if Hashtbl.mem env.typedefs s then
          let old = Hashtbl.find env.typedefs s in
          if
            (* (Meta_ast_c.vof_any (Type old) =*= (Meta_ast_c.vof_any (Type t))) *)
            failwith "TODO: =*= and abstract_line info or us deriving eq"
          then ()
          else (
            env.pr2_and_log
              (spf "conflicting typedefs for %s, %s <> %s" s (Dumper.dump old)
                 (Dumper.dump t));
            Hashtbl.replace env.dupes (fst name, E.Type) true)
          (* todo: if are in Source, then maybe can add in local_typedefs *)
        else Hashtbl.add env.typedefs s t;
      let typ = Some t in
      let _env = add_node_and_edge_if_defs_mode env (name, E.Type) typ in
      (* no hook_def here *)
      (* type_ env typ; *)
      ()

and toplevels env xs = List.iter (toplevel env) xs

and define_body env v =
  let env = { env with in_define = true } in
  match v with
  | CppExpr e -> expr_toplevel env e
  | CppStmt st -> stmt env st

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

(* Mostly go through without doing anything; stmts do not use
 * any entities (expressions do).
 *)
and stmt env = function
  | CaseStmt _ -> raise Todo
  | DefStmt x -> definition env x
  | DirStmt x -> directive env x
  | ExprSt (e, _) -> expr_toplevel env e
  | Block (_, xs, _) -> stmts env xs
  | Asm xs -> List.iter (expr_toplevel env) xs
  | If (_, e, st1, st2) ->
      expr_toplevel env e;
      stmts env (st1 :: Option.to_list st2)
  | Switch (_, e, xs) ->
      expr_toplevel env e;
      cases env xs
  | While (_, e, st)
  | DoWhile (_, st, e) ->
      expr_toplevel env e;
      stmt env st
  | For (_, ForClassic (e1, e2, e3), st) ->
      expr_or_vars env e1;
      Common2.opt (expr_toplevel env) e2;
      Common2.opt (expr_toplevel env) e3;
      stmt env st
  | For (_, ForEllipsis _, st) -> stmt env st
  | Return (_, eopt) ->
      Common2.opt (expr_toplevel { env with in_return = true }) eopt
  | Continue _
  | Break _ ->
      ()
  | Label (_name, st) -> stmt env st
  | Goto (_, _name) -> ()
  | Vars xs ->
      xs
      |> List.iter (fun x ->
             let { v_name = n; v_type = t; v_storage = sto; v_init = eopt } =
               x
             in
             if sto <> Extern then (
               env.locals := (Ast.str_of_name n, Some t) :: !(env.locals);
               type_ env t);
             match eopt with
             | None -> ()
             | Some e ->
                 expr_toplevel env
                   (Assign (Ast_cpp.SimpleAssign (snd n), Id n, e)))

and expr_or_vars env = function
  | Right e -> expr_toplevel env e
  | Left _vars -> raise Todo

and case env = function
  | Case (_, e, xs) ->
      expr_toplevel env e;
      stmts env xs
  | Default (_, xs) -> stmts env xs

and stmts env xs = List.iter (stmt env) xs
and cases env xs = List.iter (case env) xs

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
(* can assume we are in Uses phase *)
and expr_toplevel env x =
  expr env x;
  hook_expr_toplevel env x

(* can assume we are in Uses phase *)
and expr env = function
  | Int _
  | Float _
  | Char _
  | Bool _
  | Null _ ->
      ()
  | String _
  | ConcatString _ ->
      ()
  (* Note that you should go here only when it's a constant. You should
   * catch the use of Id in other contexts before. For instance you
   * should match on Id in Call, so that this code
   * is executed really as a last resort, which usually means when
   * there is the use of a constant or global.
   *)
  | Id name ->
      let s = Ast.str_of_name name in
      if is_local env s then ()
      else
        let name = str env name in
        let kind_opt =
          find_existing_node_opt env name
            [
              E.Constant;
              E.Constructor;
              E.Global;
              E.Function;
              (* can pass address of func *)
              E.Prototype;
              (* can be asm function *)
              E.GlobalExtern;
            ]
            (if looks_like_macro name then E.Constant else E.Global)
        in
        kind_opt |> Option.iter (fun kind -> add_use_edge env (name, kind))
  | Call (e, (_, es, _)) -> (
      match e with
      | Id name ->
          let s = Ast.str_of_name name in
          if is_local env s then ()
          else
            let name = str env name in
            let kind_opt =
              find_existing_node_opt env name
                [
                  E.Macro;
                  E.Constant;
                  (* for DBG-like macro *)
                  E.Function;
                  E.Global;
                  (* can do foo() even with a function pointer *)
                  E.Prototype;
                  E.GlobalExtern;
                ]
                (if looks_like_macro name then E.Macro else E.Function)
            in
            (* we don't call call like foo(bar(x)) to be counted
             * as special calls in prolog, hence the NoCtx here.
             *)
            kind_opt
            |> Option.iter (fun kind ->
                   add_use_edge { env with ctx = P.NoCtx } (name, kind);
                   args { env with ctx = P.CallCtx (fst name, kind) } es)
      (* todo: unexpected form of call? function pointer call? add to stats *)
      | _ ->
          expr env e;
          args env es)
  | Assign (_, e1, e2) ->
      (* mostly for generating use/read or use/write in prolog *)
      expr { env with in_assign = true } e1;
      expr env e2
  | ArrayAccess (e1, (_, e2, _)) -> exprs env [ e1; e2 ]
  (* todo: determine type of e and make appropriate use link *)
  | RecordPtAccess (e, _, _name) -> expr env e
  | Cast (t, e) ->
      type_ env t;
      expr env e
  (* potentially here we would like to treat as both a write and read
   * of the variable, so maybe a trivalue would be better than a boolean
   *)
  | Postfix (e, _op)
  | Infix (e, _op) ->
      expr { env with in_assign = true } e
  | Unary (e, op) -> (
      match Ast.unwrap op with
      (* if get the address probably one wants to modify it *)
      | Ast_cpp.GetRef -> expr { env with in_assign = true } e
      | _ -> expr env e)
  | Binary (e1, _op, e2) -> exprs env [ e1; e2 ]
  | CondExpr (e1, e2, e3) -> exprs env [ e1; e2; e3 ]
  | Sequence (e1, e2) -> exprs env [ e1; e2 ]
  | ArrayInit xs ->
      xs |> unbracket
      |> List.iter (fun (eopt, init) ->
             Common2.opt (expr env) eopt;
             expr env init)
  (* todo: add deps on field *)
  | RecordInit xs -> xs |> unbracket |> List.map snd |> exprs env
  | SizeOf (_, x) -> (
      match x with
      (* ugly: because of bad typedef inference what we think is an Id
       * could actually be a TTypename. So add a hack here.
       *)
      | Left (Id origname) ->
          let s = Ast.str_of_name origname in
          if is_local env s then ()
          else
            let name = str env origname in
            if
              (not (G.has_node (Ast.str_of_name name, E.Global) env.g))
              && not (G.has_node (Ast.str_of_name name, E.GlobalExtern) env.g)
            then type_ env (TTypeName origname)
            else expr env (Id origname)
      | Left e -> expr env e
      | Right t -> type_ env t)
  | GccConstructor (t, e) ->
      type_ env t;
      expr env e
  | Defined (_t, _id) -> ()
  | Ellipses _
  | DeepEllipsis _
  | TypedMetavar _ ->
      ()

and exprs env xs = xs |> List.iter (expr env)
and args env xs = xs |> List.iter (function Arg e -> expr env e)
(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)

and type_ env typ =
  if env.phase = Uses && env.conf.types_dependencies then
    let t = final_type env typ in
    let rec aux t =
      match t with
      | TBase _ -> ()
      (* The use of prefix below is consistent with what is done for the defs.
       * We do that right now because we are not clear on the namespaces ...
       * whether we can have both struct x and enum x in a file
       * (I think no, but right now easier to just prefix)
       *)
      | TStructName (Struct, name) ->
          add_use_edge env (add_prefix "S__" name, E.Type)
      | TStructName (Union, name) ->
          add_use_edge env (add_prefix "U__" name, E.Type)
      | TEnumName name -> add_use_edge env (add_prefix "E__" name, E.Type)
      | TTypeName name ->
          let s = Ast.str_of_name name in
          (* could be a type parameter *)
          if is_local env s && env.in_define then ()
          else if env.conf.typedefs_dependencies then
            add_use_edge env (add_prefix "T__" name, E.Type)
          else if Hashtbl.mem env.typedefs s then
            let t' = Hashtbl.find env.typedefs s in
            (* right now 'typedef enum { ... } X' results in X being
             * typedefed to ... itself
             *)
            if t' = t then add_use_edge env (add_prefix "T__" name, E.Type)
              (* should be done in expand_typedefs? unless we had a dupe *)
            else
              env.pr2_and_log
                (spf "skipping edge, probably dupe typedef %s (%s)" s
                   (Parse_info.string_of_info (snd name)))
          else
            env.pr2_and_log
              (spf "typedef not found: %s (%s)" s
                 (Parse_info.string_of_info (snd name)))
      | TPointer (_, x) -> aux x
      | TArray (eopt, x) ->
          Common2.opt (expr env) eopt;
          aux x
      | TFunction (t, xs) ->
          aux t;
          xs
          |> List.iter (function
               | ParamDots _ -> ()
               | ParamClassic p -> aux p.p_type)
      | TMacroApply _ -> raise Todo
    in
    aux t

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose = true) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  let chan = open_out_bin (Filename.concat root "pfff.log") in

  (* less: we could also have a local_typedefs_of_files to avoid conflicts *)
  let conf =
    {
      types_dependencies = true;
      (* TODO currently unused, we do not process fields :( *)
      fields_dependencies = true;
      macro_dependencies = false;
      propagate_deps_def_to_decl = false;
      (* let's expand typedefs, it's simpler, hence false *)
      typedefs_dependencies = false;
    }
  in

  let env =
    {
      g;
      phase = Defs;
      current = G.pb;
      ctx = P.NoCtx;
      c_file_readable = "__filled_later__";
      conf;
      in_assign = false;
      in_define = false;
      in_return = false;
      (* will be overriden by file specific hashtbl *)
      local_rename = Hashtbl.create 0;
      dupes = Hashtbl.create 101;
      typedefs = Hashtbl.create 101;
      structs = Hashtbl.create 101;
      fields = Hashtbl.create 101;
      locals = ref [];
      log =
        (fun s ->
          output_string chan (s ^ "\n");
          flush chan);
      pr2_and_log =
        (fun s ->
          (*if verbose then *)
          pr2 s;
          output_string chan (s ^ "\n");
          flush chan);
    }
  in

  (* step0: parsing *)
  env.pr2_and_log "\nstep0: parsing";

  (* we could run the parser in the different steps
   * but we need to make sure to reset some counters because
   * the __anon_struct_xxx build in ast_c_simple_build
   * must be stable when called another time with the same file!
   * alt: fix ast_c_simple_build to not use gensym and generate stable
   * names.
   *)
  let elems =
    files
    |> Console.progress ~show:verbose (fun k ->
           List.map (fun file ->
               k ();
               let ast = parse ~show_parse_error:true file in
               let readable = Common.readable ~root file in
               let local_rename = Hashtbl.create 101 in
               (ast, readable, local_rename)))
  in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  elems
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (ast, c_file_readable, local_rename) ->
             k ();
             extract_defs_uses
               { env with phase = Defs; c_file_readable; local_rename }
               ast));

  (* step2: creating the 'Use' edges *)
  env.pr2_and_log "\nstep2: extract Uses";
  elems
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (ast, c_file_readable, local_rename) ->
             k ();
             extract_defs_uses
               { env with phase = Uses; c_file_readable; local_rename }
               ast));

  env.pr2_and_log "\nstep3: adjusting";
  if conf.propagate_deps_def_to_decl then
    Graph_code_helpers
    .propagate_users_of_functions_globals_types_to_prototype_extern_typedefs g;
  G.remove_empty_nodes g [ G.not_found; G.dupe; G.pb ];

  g

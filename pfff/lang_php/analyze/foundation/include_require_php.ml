(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Cst_php

module Ast = Cst_php
module V = Visitor_php

module Env = Env_php
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Most of the arguments to require/include are static strings or concatenation
 * of know variables (e.g. $_SERVER) to static strings. It is useful to 
 * statically analyze those arguments, e.g. to detect bugs such as missing
 * filenames, and so to resolve statically the filenames, hence this file.
 * We just provide a better "view" over the Include | Require | ... 
 * statements present in Ast_php.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type increq = 
  increq_kind * Cst_php.tok * increq_expr

 and increq_expr = 
   (* e.g. require 'master_include.php'; *)
   | Direct of Common.filename 
   (* e.g. require $BASEPATH .'/lib/init/ajax.php'; *)
   | ConcatVar of Cst_php.dname * Common.filename
   (* e.g. require BASEPATH .'/lib/init/ajax.php'; *)
   | ConcatConstant of Cst_php.ident * Common.filename
   (* e.g. require $_SERVER['PHP_ROOT'].'/lib/init/ajax.php'; *)
   | ConcatArrrayVar of Cst_php.dname * string * Common.filename
   (* e.g. require dirname(__FILE__).'/master_include.php'; *)
   | ConcatDirname of Common.filename
   (* e.g. require realpath(dirname(__FILE__)).'/master_include.php'; 
    * todo: diff with just dirname ??
    *)
   | ConcatRealpathDirname of Common.filename
   (* e.g. require $file; *)
   | SimpleVar of Cst_php.dname

   | Other of Cst_php.expr

 and increq_kind = 
   | Include
   | IncludeOnce
   | Require
   | RequireOnce

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo? should perhaps try to port that to use sgrep, and to access
 * sgrep result from OCaml itself.
 *)
let rec increq_expr_of_expr e = 
  match e with

  | (Sc(C(String((sfilemame, _))))) ->
      Direct sfilemame

  (* generated from ./ffi -dump_php_ml ../tests/require_classic.php *)
  | (Binary(
      ((
        (ArrayGet(IdVar(darray, _scope_ref),
                     (_, Some((Sc(C(String((sfld, _)))))), _))
        ))), (BinaryConcat, _0),
      (Sc(C(String((sfilename, _1)))))))
    -> 
      ConcatArrrayVar (darray, sfld, sfilename)

  (* generated from ./ffi -dump_php_ml ../tests/require_classic_bis.php *)
  | (Binary(
      (IdVar(dvar, _scope_ref)),
      (BinaryConcat, _),
      (Sc(C(String((sfilename, _))))
      )))
    ->
      ConcatVar (dvar, sfilename)

  (* generated from ./ffi -dump_php_ml ../tests/require_classic2.php *)
  | (Binary(
      (Binary(
        (ArrayGet(
            (IdVar(darray, _scope)),
            (_,
            Some((Sc(C(String((sfld, _)))))),
            _))
        ), (BinaryConcat, _0),
        (Sc(C(String((sfilename1, _1))))))), 
         (BinaryConcat, _4),
      (Sc(C(String((sfilename2, _5))))
      )))
    -> 
      ConcatArrrayVar (darray, sfld, sfilename1 ^ sfilename2)

  (* ./ffi -dump_php_ml ../tests/require_dirname.php *)
  | (Binary(
      (Call(Id (XName[QI (Name(("dirname", _)))]),
                      (_,
                      [Left (Arg(
                        (Sc(C(PreProcess(((File, _))))))))],
                      _))), 
        (BinaryConcat, _),
      (Sc(C(String((sfilename, _0)))))))
    ->
      ConcatDirname(sfilename)
  (* __DIR__ . 'file' *)
  | Binary(Sc(C(PreProcess((Dir, _3)))), (BinaryConcat, _4),
          Sc(C(String((sfilename, _5)))))
    -> ConcatDirname(sfilename)

  (* ./ffi -dump_php_ml ../tests/require_realpath.php *)
  | (Binary(
      (Call(Id (XName[QI (Name(("realpath", _)))]),
                      (_,
                      [Left (Arg(
                        (Call(Id (XName[QI (Name(("dirname", _)))]),
                                        (_,
                                        [Left (Arg(
                                          (Sc(
                                            C(PreProcess(((File, _)))))
                                          )))],
                                        _))
                        )))],
                      _1))
      ), (BinaryConcat, _4),
      (Sc(
        C(String((sfilename, _5))))
      ))) 
    ->
      ConcatRealpathDirname(sfilename)

  | (Binary(
      (Call(Id (XName[QI (Name(("realpath", _)))]),
                      (_,
                      [Left (Arg(
                        (Binary(
                          (Call(Id (XName[QI (Name(("dirname", _)))]),
                                          (_,
                                          [Left Arg((
                                            (Sc(
                                              C(PreProcess(((File, _)))))
                                            )))],
                                          _))
                          ), (BinaryConcat, _1),
                          (Sc(C(String((sfilename1, _2))))))
                        )))],
                      _5))
        ), 
      (BinaryConcat, _8),
      (Sc(C(String((sfilename2, _9))))
      )))
    ->
      ConcatRealpathDirname(sfilename1 ^ sfilename2)

  (* ./ffi -dump_php_ml ../tests/require_realpath3.php *)
  | (Call(Id (XName[QI (Name(("realpath", _)))]),
                    (_,
                    [Left (Arg(
                      (Binary(
                        (Call(Id (XName[QI (Name(("dirname", _)))]),
                                        (_,
                                        [Left (Arg(
                                          (Sc(
                                            C(PreProcess(((File, _)))))
                                          )))],
                                        _))
                        ), (BinaryConcat, _1),
                        (Sc(
                          C(
                            String((sfilename, _2))))
                       ))
                    )))],
                _5))
          )
      ->
      ConcatRealpathDirname(sfilename)


  (* ./ffi -dump_php_ml ../tests/require_constant_concat.php *)
  | (Binary((Id(XName[QI (name)])),
           (BinaryConcat, _),
           (Sc(C(String((sfilename, _)))))))
    ->
      ConcatConstant (name, sfilename)

  (* ./ffi -dump_php_ml ../tests/require_classic_bis2.php *)
  | (Sc(
      Guil(_,
          [EncapsVar(IdVar(dname, _scope));
           EncapsString((sfilename, _))], _)))
    ->
      ConcatVar (dname, sfilename)

  (* some simple isomorphisms *)
  | ParenExpr (eparen) ->
      increq_expr_of_expr (Ast.unparen eparen)
        

  | IdVar(dvar, _scope) ->
      SimpleVar dvar

  | _ -> Other e



(* todo: check that the directives are at the toplevel ? *)
let increq_of_include_stmt e = 
  match e with
  | Ast.Include     (t, e) -> Some (Include,      t, increq_expr_of_expr e)
  | Ast.IncludeOnce (t, e) -> Some (IncludeOnce,  t, increq_expr_of_expr e)
  | Ast.Require     (t, e) -> Some (Require,      t, increq_expr_of_expr e)
  | Ast.RequireOnce (t, e) -> Some (RequireOnce,  t, increq_expr_of_expr e)

  | _ -> None


let filename_concat dir file = 
  if file =~ "^/\\(.*\\)"
  then Filename.concat dir (matched1 file)
  else Filename.concat dir file

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let all_increq_of_any = 
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kexpr = (fun (k, _) x ->
      match increq_of_include_stmt x with
      | Some require -> Common.push require aref;
      | None -> 
          (* do we need to recurse ? *)
          k x
    );
  }
  )

let top_increq_of_program asts = 
  let stmts = Lib_parsing_php.top_statements_of_program asts in
  stmts |> Common.map_filter (fun st ->
    match st with
    | ExprStmt (e, _tok) -> 
        increq_of_include_stmt e
    | _ -> None
  )

(* note that the strings in increq_expr can contain some '../' and 
 * so need to resolve also that 
 *)
let resolve_path (env, pwd) incexpr = 

  match incexpr with
   | Direct filename ->
       if Filename.is_relative filename
       then
         Some (Filename.concat pwd filename)
       else begin
         pr2_once (spf "should not use absolute path in include/require: %s"
                      filename);
       
         Some filename
       end
   | ConcatVar (dname, filename) ->
       let s = Ast.str_of_dname dname in
       (try 
         let path = Hashtbl.find env.Env.globals s in
         Some (filename_concat path filename)
       with Not_found ->
         (* maybe a dynamic var like $BASE_PATH *)
         (match env.Env.globals_specials s pwd with
         | Some path ->
             Some (filename_concat path filename)
         | _ -> 
             None
         ))
       
   | ConcatConstant (name, filename) ->
       let s = Ast.str_of_ident name in
       (try
           let path = Hashtbl.find env.Env.constants s in
           Some (filename_concat path filename)
        with Not_found -> None
       )

   | ConcatArrrayVar (dname, fld, filename) -> 
       let s = Ast.str_of_dname dname in
       (try 
           let h = Hashtbl.find env.Env.global_arrays s in
           let path = Hashtbl.find h fld in
           Some (filename_concat path filename)
       with Not_found -> None
       )

   | ConcatDirname (filename) ->
       Some (filename_concat pwd filename)

   | ConcatRealpathDirname (filename) ->
       Some (filename_concat pwd filename)
       

   | SimpleVar _dname -> 
       None
   | Other _e ->
       None


(* note: copy pasted in flib.ml *)
let includes_of_file env file = 
  let ast = Parse_php.parse_program file in
  let dir = Common2.dirname file in
  
  let incs = all_increq_of_any (Program ast) in
  incs |> Common.map_filter (fun (_kind, tok, incexpr) ->
    
    let fopt = resolve_path (env, dir) incexpr in
    match fopt with
    | Some f -> Some f
    | None ->
        pr2_once (spf "includes_of_file: could not resolve path at\t %s"
                (Parse_info.string_of_info tok));
        None
  )

type algo = Dfs | Bfs
let default_algo = Dfs

let _ignore = Bfs

let recursive_included_files_of_file 
 ?(verbose=false)
 ?(depth_limit = None)
 ?(includes_of_file=includes_of_file) 
 env file = 

  let hdone = Hashtbl.create 101 in

  (* only for the dfs for now *)
  let reached_limit depth = 
    match depth_limit with
    | None -> false
    | Some x -> depth >= x
  in

  let rec aux_dfs depth file stack = 
    if Hashtbl.mem hdone file || reached_limit depth
    then ()
    else begin
      Hashtbl.add hdone file true;
    
      (* could be good to have a readable path here.
       * info is in env ? :)
       *)
      if verbose then begin
        Common2._tab_level_print := depth;
        pr2 (spf "->%s" file);
      end;

      let incs = 
        try 
          includes_of_file env file 
        with exn ->
          pr2 (spf "PB processing %s, exn = %s. Trace = "
                  file (Common.exn_to_s exn));
          stack |> List.iter (fun (file, included_files) ->
            pr2 (spf " %s:" file);
            included_files |> List.iter (fun file -> pr2 (spf "  %s" file));
          );
          raise exn;
      in
          
      incs |> List.iter (fun file2 ->
        aux_dfs (depth+1) file2 ((file, incs)::stack)
      );
    end
  in
  
  let aux_bfs () =
    let current_wave = ref [file] in
    
    while !current_wave <> [] do
      let current = !current_wave in
      current_wave := [];
      
      current |> List.iter (fun file ->
        if Hashtbl.mem hdone file then ()
        else begin
          Hashtbl.add hdone file true;
          
          let incs = includes_of_file env file in
          
          if verbose then 
            pr2 (spf "Found %d includes (implicit or explicit) in\n\t%s"
                  (List.length incs) file);
          current_wave := incs @ !current_wave;
        end
      );
    done;
  in
  (match default_algo with
  | Dfs -> 
      Common.save_excursion Common2._tab_level_print 1 (fun () ->
        aux_dfs 0 file [];
      )
      
  | Bfs -> aux_bfs ()
  );

  Common.hashset_to_list hdone


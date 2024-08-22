(* Yoann Padioleau
 *
 * Copyright (C) 2009-2011 Facebook
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
open Fpath_.Operators

(*module Ast = Cst_php*)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag_parsing.verbose_parsing

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

let is_php_script file =
  UFile.with_open_in file (fun chan ->
      try
        let l = input_line chan in
        l =~ "#!/usr/.*/php" || l =~ "#!/bin/env php"
        || l =~ "#!/usr/bin/env php"
      with
      | End_of_file -> false)

(* todo: can not include those files for now because
 * they conflict with data/php_stdlib and generate lots
 * of DUPE in codegraph
 *
 * (filename =~ ".*\\.hhi") (* hack uses this extension *)
 *)
let is_php_filename filename =
  !!filename =~ ".*\\.php$" || !!filename =~ ".*\\.phpt$"
  (* hotcrp uses this extension *)
  || !!filename =~ ".*\\.inc"

let is_hhi_filename filename = !!filename =~ ".*\\.hhi$" || false
let is_php_filename_phar filename = !!filename =~ ".*\\.phar$" || false

let is_php_file filename =
  (not (is_php_filename_phar filename))
  && (is_php_filename filename || is_php_script filename)

(*
 * In command line tools like git or mercurial, many operations works
 * when a file, a set of files, or even dirs are passed as parameters.
 * We want the same with pfff, hence this small helper function that
 * transform such files_or_dirs into a flag set of filenames.
 *)
let find_source_files_of_dir_or_files ?(verbose = false) ?(include_hack = false)
    xs =
  xs |> UFile.files_of_dirs_or_files_no_vcs_nofilter
  |> List.filter (fun filename ->
         (* note: there was a possible race here because between the time we
          * do the 'find' and the time we call is_php_file(), the file may have
          * disappeared (this happens for instance because of watchman).
          * Hence the Sys.file_exists guard.
          *)
         let valid =
           (* note that there is still a race between the call to file_exists
            * and is_php_file, but this one is far shorter :)
            *)
           Sys.file_exists !!filename
           && (is_php_file filename
              || (include_hack && is_hhi_filename filename))
         in
         if (not valid) && verbose then pr2 ("not analyzing: " ^ !!filename);
         valid)
  |> List_.sort

(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)
(*s: extract infos *)
(*
let extract_info_visitor recursor =
  let globals = ref [] in
  let hooks = { V.default_visitor with
    V.kinfo = (fun (_k, _) i ->
      (* most of the time when you use ii_of_any, you want to use
       * functions like max_min_pos which works only on origin tokens
       * hence the filtering done here.
       *
       * ugly: For PHP we use a fakeInfo only for generating a fake left
       * brace for abstract methods.
       *)
      match i.Parse_info.token with
      | Parse_info.OriginTok _ ->
        Common.push i globals
      | _ ->
        ()
    )
  } in
  begin
    let vout = V.mk_visitor hooks in
    recursor vout;
    List.rev !globals
  end
*)
(*x: extract infos *)
(*
let ii_of_any any =
  extract_info_visitor (fun visitor -> visitor any)
*)
(*e: extract infos *)

(*****************************************************************************)
(* Max min, range *)
(*****************************************************************************)
(*s: max min range *)
(*x: max min range *)

let (range_of_origin_ii : Cst_php.tok list -> (int * int) option) =
 fun ii ->
  let ii = List.filter Tok.is_origintok ii in
  try
    let min, max = Tok_range.min_max_toks_by_pos ii in
    assert (Tok.is_origintok max);
    assert (Tok.is_origintok min);
    let strmax = Tok.content_of_tok max in
    Some (Tok.bytepos_of_tok min, Tok.bytepos_of_tok max + String.length strmax)
  with
  | _ -> None
(*e: max min range *)

(*****************************************************************************)
(* Ast getters *)
(*****************************************************************************)
(*s: ast getters *)
(*
let get_funcalls_any any =
  let h = Hashtbl.create 101 in

  let hooks = { V.default_visitor with
    (* TODO if nested function ??? still wants to report ? *)
    V.kexpr = (fun (k,_vx) x ->
      match x with
      | Call (Id callname, _args) ->
          let str = Cst_php.str_of_name callname in
          Hashtbl.replace h str true;
          k x
      | _ -> k x
    );
  }
  in
  let visitor = V.mk_visitor hooks in
  visitor any;
  Common.hashset_to_list h
*)
(*x: ast getters *)
(*x: ast getters *)
(*
let get_constant_strings_any any =
  let h = Hashtbl.create 101 in

  let hooks = { V.default_visitor with
    V.kconstant = (fun (k,_vx) x ->
      match x with
      | String (str,_ii) ->
          Hashtbl.replace h str true;
      | _ -> k x
    );
    V.kencaps = (fun (k,_vx) x ->
      match x with
      | EncapsString (str, _ii) ->
          Hashtbl.replace h str true;
      | _ -> k x
    );
  }
  in
  (V.mk_visitor hooks) any;
  Common.hashset_to_list h
*)
(*e: ast getters *)
(*
let get_static_vars_any any =
  any |> V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kstmt = (fun (k,_vx) x ->
      match x with
      | StaticVars (_tok, xs, _tok2) ->
          xs |> Ast.uncomma |> List.iter (fun (dname, _affect_opt) ->
            Common.push dname aref
          );
      | _ ->
          k x
    );
  })

(* todo? do last_stmt_is_a_return isomorphism ? *)
let get_returns_any any =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kstmt = (fun (k,_vx) x ->
      match x with
      | Return (_tok1, Some e, _tok2) ->
          Common.push e aref
      | _ -> k x
    )}) any

let get_vars_any any =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kexpr = (fun (k, _vx) x ->
      match x with
      | IdVar (dname, _scope) ->
          Common.push dname aref

      (* todo? sure ?? *)
      | Lambda (l_use, _def) ->
          l_use |> Common.do_option (fun (_tok, xs) ->
            xs |> Ast.unparen |> Ast.uncomma |> List.iter (function
            | LexicalVar (_is_ref, dname) ->
                Common.push dname aref
            )
          );
          k x
      | _ -> k x
    );
  }) any
*)

(*****************************************************************************)
(* Ast adapters *)
(*****************************************************************************)

(*
let top_statements_of_program ast =
  ast |> List.map (function
  | StmtList xs -> xs
  | FinalDef _|NotParsedCorrectly _
  | ClassDef _| FuncDef _ | ConstantDef _ | TypeDef _
  | NamespaceDef _ | NamespaceBracketDef _ | NamespaceUse _
      -> []
  ) |> List_.flatten

(* We often do some analysis on "unit" of code like a function,
 * a method, or toplevel statements. One can not use the
 * 'toplevel' type for that because it contains Class and Interface which
 * are too coarse grained; the method granularity is better.
 *
 * For instance it makes sense to have a CFG for a function, a method,
 * or toplevel statements but a CFG for a class does not make sense.
 *)
let functions_methods_or_topstms_of_program prog =
  let funcs = ref [] in
  let methods = ref [] in
  let toplevels = ref [] in

  let visitor = V.mk_visitor { V.default_visitor with
    V.kfunc_def = (fun (_k, _) def ->
      match def.f_type with
      | FunctionRegular -> Common.push def funcs
      | MethodRegular | MethodAbstract -> Common.push def methods
      | FunctionLambda -> ()
    );
    V.ktop = (fun (k, _) top ->
      match top with
      | StmtList xs ->
          Common.push xs toplevels
      | _ ->
          k top
    );
  }
  in
  visitor (Program prog);
  !funcs, !methods, !toplevels


(* do some isomorphisms for declaration vs assignement *)
let get_vars_assignements_any recursor =
  (* We want to group later assignement by variables, and
   * so we want to use function like Common.group_by_xxx
   * which requires to have identical key. Each dname occurence
   * below has a different location and so we can use dname as
   * key, but the name of the variable can be used, hence the use
   * of Ast.dname
   *)
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
      V.kstmt = (fun (k,_) x ->
        match x with
        | StaticVars (_tok, xs, _tok2) ->
            xs |> Ast.uncomma |> List.iter (fun (dname, affect_opt) ->
              let s = Ast.str_of_dname dname in
              affect_opt |> Common.do_option (fun (_tok, scalar) ->
                Common.push (s, scalar) aref;
              );
            );
        | _ ->
            k x
      );

      V.kexpr = (fun (k,_vx) x ->
        match x with
        | Assign (lval, _, e)
        | AssignOp (lval, _, e) ->
            (* the expression itself can contain assignements *)
            k x;

            (* for now we handle only simple direct assignement to simple
             * variables *)
            (match lval with
            | IdVar (dname, _scope) ->
                let s = Ast.str_of_dname dname in
                Common.push (s, e) aref;
            | _ ->
                ()
            )
        (* todo? AssignRef AssignNew ? *)
        | _ ->
            k x
      );
    }
  ) recursor |> Common.group_assoc_bykey_eff
 *)
(*e: lib_parsing_php.ml *)

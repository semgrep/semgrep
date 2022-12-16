open Common

open Cst_php
module Ast = Cst_php
module V = Visitor_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This file contains the implementation of a simple code search where we
 * want to find all methods returning $this when the method name matches
 * a certain regexp (if it starts with 'get').
 *
 * To compile this file do:
 *
 *    $ ocamlc -g -o simple_code_search \
 *      -I ../commons -I ../lang_php/parsing \
 *      str.cma unix.cma bigarray.cma \
 *      ../commons/lib.cma ../h_program-lang/lib.cma \
 *      ../lang_php/parsing/lib.cma \
 *      simple_code_search.ml
 *
 * To run do for instance:
 *
 *    $ ./simple_code_search /path/to/php/file/or/dir
 *
 * which may output for instance:
 *
 *  FOUND A MATCH in getFoo at return_this.php:5:4
 *
 *
 * We can rely on the 'pfff -dump_php' tool to help finding the
 * relevant OCaml constructors in ast_php.ml.
 * Here is the result of 'pfff -dump_php pfff/tests/php/sgrep/return_this.php'
 *
 * [ClassDef(
 *    {c_type=ClassRegular(i_1); c_name=Name("Test", i_2); c_tparams=None;
 *     c_extends=None; c_implements=None; c_attrs=None;
 *     c_body=(i_3,
 *             [Method(
 *                {f_tok=i_4; f_type=MethodRegular; f_attrs=None;
 *                 f_modifiers=[(Public, i_5)]; f_ref=None;
 *                 f_name=Name("getFoo", i_6); f_tparams=None;
 *                 f_params=(i_7, [], i_8); f_return_type=None;
 *                 f_body=(i_9, [Return(i_10, Some(This(i_11)),i_12)],i_13);})],
 *             i_14);
 *     }); FinalDef(i_15)]
 *)

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

let main files_or_dirs =
  let files = Lib_parsing_php.find_source_files_of_dir_or_files files_or_dirs in

  files |> List.iter (fun file ->

    (* step1: parse the file *)
    let ast = Parse_php.parse_program file in

    let current_method = ref "" in

    (* step2: visit the AST  *)
    let visitor = V.mk_visitor { V.default_visitor with

                                 (* here is the relevant section of 'pfff -dump_php return_this.php'
                                  *             [Method(
                                  *                {f_tok=i_4; f_type=MethodRegular; f_attrs=None;
                                  *                 f_modifiers=[(Public, i_5)]; f_ref=None;
                                  *                 f_name=Name("getFoo", i_6); f_tparams=None;
                                  *                 f_params=(i_7, [], i_8); f_return_type=None;
                                  *                 f_body
                                 *)
                                 V.kfunc_def = (fun (k, _) def ->
                                   (match def with
                                    | {f_type = MethodRegular; f_name = Name(str, _); _ } ->
                                        current_method := str;
                                    | _ -> ();
                                   );
                                   (* call the continuation *)
                                   k def;
                                 );

                                 V.kstmt = (fun (k, _) st ->
                                   match st with
                                   (* here is the relevant section of 'pfff -dump_php return_this.php'
                                    * f_body=(i_9, [Return(i_10, Some(This(i_11)), i_12)], i_13); })],
                                   *)
                                   | Return(i_10, Some(This(i_11)), i_12) ->
                                       if !current_method =~ "get.*"
                                       then
                                         pr2 (spf "FOUND A MATCH in %s at %s" !current_method
                                                (PI.string_of_info i_10))
                                   | _ -> k st
                                 );
                               }
    in
    visitor (Program ast);
    ()
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let _ =
  main (Array.to_list Sys.argv)

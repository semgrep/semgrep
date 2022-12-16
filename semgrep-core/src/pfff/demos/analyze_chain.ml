open Common

open Cst_php
module Ast = Cst_php
module V = Visitor_php
module PI = Parse_info
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Toy static analysis to analyze method call chains.
 * usage:
 *  $ ./analyze_chain.byte chain.php
 *  ["foo"; "bar"; "foobar"]
 *)

(*****************************************************************************)
(* Chain analyzer *)
(*****************************************************************************)
(* A chain like "$o->foo()->bar->foobar()" is internally parsed
 * as ((($o->foo())->bar())->foobar())
 * and so the first Call we will see as we go down in the AST is
 * actually the last call in the chain.
 * This function will return the list of methods in the AST top-to-bottom
 * order (which is right to left), so use List.rev if you want the
 * left to right order.
*)
let rec methods_in_chain e =
  match e with
  | Call (ObjGet(objexpr, _tok, Id name), _args) ->
      name::methods_in_chain objexpr
  | _ -> []

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

let main files_or_dirs =
  let files = Lib_parsing_php.find_source_files_of_dir_or_files files_or_dirs in

  files |> List.iter (fun file ->

    (* step1: parse the file *)
    let ast = Parse_php.parse_program file in

    let visitor = V.mk_visitor { V.default_visitor with
                                 V.kexpr = (fun (k, vx) e ->
                                   (match e with
                                    | Call (ObjGet(objexpr, _tok, name), (_lp, args, _rp)) ->
                                        let chain = methods_in_chain e in
                                        let strs = chain |> List.map Ast.str_of_name |> List.rev in
                                        pr2_gen strs;

                                        (* visit just the arguments *)
                                        vx (Arguments args);
                                    | _ -> k e
                                   );
                                 );
                               }
    in
    visitor (Program ast);
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let _ =
  main (Array.to_list Sys.argv)

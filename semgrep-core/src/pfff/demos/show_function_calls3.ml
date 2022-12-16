(*s: show_function_calls3.ml *)
(*s: basic pfff modules open *)
open Common
open Cst_php
(*e: basic pfff modules open *)
module V = Visitor_php

(*s: show_function_calls v3 *)
let show_function_calls file =
  let ast = Parse_php.parse_program file in

  (*s: initialize hfuncs *)
  let hfuncs = Common2.hash_with_default (fun () ->
    Common2.hash_with_default (fun () -> 0)
  )
  in
  (*e: initialize hfuncs *)

  (*s: iter on asts using visitor, updating hfuncs *)
  let visitor = V.mk_visitor  { V.default_visitor with
                                V.kexpr = (fun (k, _) e ->
                                  match e with
                                  | Call (Id funcname, args) ->

                                      (*s: print funcname and nbargs *)
                                      let f = Cst_php.str_of_name funcname in
                                      let nbargs = List.length (Cst_php.unparen args) in
                                      pr2 (spf "Call to %s with %d arguments" f nbargs);
                                      (*e: print funcname and nbargs *)

                                      (*s: update hfuncs for name with nbargs *)
                                      (* hfuncs[f][nbargs]++ *)
                                      hfuncs#update f (fun hcount ->
                                        hcount#update nbargs (fun x -> x + 1);
                                        hcount
                                      )
                                  (*e: update hfuncs for name with nbargs *)

                                  | _ ->
                                      k e
                                );
                              }
  in
  visitor (Program ast);
  (*e: iter on asts using visitor, updating hfuncs *)

  (*s: display hfuncs to user *)
  (* printing statistics *)
  hfuncs#to_list |> List.iter (fun (f, hcount) ->
    pr2 (spf "statistics for %s" f);
    hcount#to_list |> Common.sort_by_key_highfirst
    |> List.iter (fun (nbargs, nbcalls_at_nbargs) ->
      pr2 (spf " when # of args is %d: found %d call sites"
             nbargs nbcalls_at_nbargs)
    )
  )
(*e: display hfuncs to user *)
(*e: show_function_calls v3 *)

let main =
  show_function_calls Sys.argv.(1)

(*e: show_function_calls3.ml *)

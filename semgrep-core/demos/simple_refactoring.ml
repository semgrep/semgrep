open Common

module Ast = Cst_php

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This file contains the implementation of a simple refactoring where we
 * remove the second argument in all calls to ArgAssert::isString() and
 * ArgAssert::isArray(). The goal is to provide a tutorial on how
 * to write PHP refactorings using the low level Pfff infrastructure.
 * For most refactoring, spatch should be fine,
 * see https://github.com/facebook/pfff/wiki/Spatch
 *
 * To compile this file do:
 *
 *    $ ocamlc -g -o simple_refactoring \
 *      -I ../commons -I ../lang_php/parsing \
 *      str.cma unix.cma nums.cma bigarray.cma \
 *      ../commons/lib.cma ../h_program-lang/lib.cma \
 *      ../lang_php/parsing/lib.cma \
 *      simple_refactoring.ml
 *
 * To run do for instance:
 *
 *    $ ./simple_refactoring ../tests/php/spatch/arg_assert.php
 *
 * which should output:
 *
 *   processing: ../tests/php/spatch/arg_assert.php (0/1)
 *   --- ../tests/php/spatch/arg_assert.php	2010-10-22 13:09:09.0 -0700
 *   +++ /tmp/trans-17581-464603.php	2010-10-22 13:21:52.0 -0700
 *   @@ -1,7 +1,6 @@
 *    <?php
 *
 *    function foo($s) {
 *   -  ArgAssert::isString($s, "s");
 *   -  ArgAssert::isArray($s,
 *   -                     array(1, 2));
 *   +  ArgAssert::isString($s);
 *   +  ArgAssert::isArray($s);
 *    }
 *
 * If you modify the 'apply_patch' variable below it will actually
 * do the modification in place on the file. It is set to false by
 * default for ease of prototyping.
 *
 *
 * (the equivalent semantic patch is simply:
 * - ArgAssert::isString(X, Y)
 * + ArgAssert::isString(X)
 *
 * or
 *
 *   ArgAssert::isString(X
 * -                     ,Y
 *                       )
 * )
 *
 * Timing: 25 minutes
 *
 * Alternatives:
 *  - lex-pass I believe has a special command just for that.
 *
 * See also main_spatch.ml
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let apply_patch = ref false

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

let main files_or_dirs =
  let files = Lib_parsing_php.find_source_files_of_dir_or_files files_or_dirs in

  Flag_parsing.show_parsing_error := false;
  Flag_parsing.verbose_lexing := false;

  files |> Common2.index_list_and_total |> List.iter (fun (file, i, total) ->
    pr2 (spf "processing: %s (%d/%d)" file i total);

    (* step1: parse the file *)
    let (_ast, _tokens), _ = Parse_php.parse file in

(*
    (* step2: visit the AST and annotate the relevant tokens in AST leaves *)
    let visitor = V.mk_visitor { V.default_visitor with

      V.kexpr = (fun (k, _) expr ->
        match expr with

        (*
         * At this point 'expr' is the AST of a PHP expression that we
         * can match over a specific pattern. The pattern we want is
         * a static method call to the 'isString' function of the
         * 'ArgAssert' class. One could look at ast_php.ml and
         * find the appropriate OCaml constructors for those
         * AST elements. An alternative is to use pfff -dump_php_ml
         * to get a first draft for the pattern. For instance
         *
         *   $ ./pfff -dump_php_ml tests/php/spatch/arg_assert2.php
         *
         * will output the internal representation of the AST of
         * arg_assert2.php, which is:
         *
         * [StmtList(
         *  [ExprStmt(
         *    (Lv(
         *     (StaticMethodCallSimple(Qualifier(Name(("ArgAssert", i_1)), i_2),
         *          Name(("isString", i_3)),
         *           (i_4,
         *            [Left(
         *             Arg(
         *             (Lv((Var(DName(("s", i_5)), Ref(NoScope)), tlval_6)), t_7)));
         *             Right(i_8); Left(Arg((Sc(C(String(("s", i_9)))), t_10)))],
         *            i_11)),
         *         tlval_12)),
         *     t_13), i_14)]); FinalDef(i_15)]
         *
         * One can then copy paste part of this AST directly into this
         * file as an OCaml pattern.
         *
         * The i_<num> are matching the corresponding tokens in the AST.
         * For instance i_1 above is the token containing the information
         * on the "ArgAssert" in the file. This token contain information
         * such as the line position of the token and also the
         * 'transfo' annotation that we can modify to remove the token
         * for instance.
         *)
        |
             (Call(
               ClassGet (Id (XName[QI(Name(("ArgAssert", i_9)))]), i_10,
                         Id (XName[QI(Name(("isString", i_11)))])),
               (i_left_paren,
               [Left(
                 Arg(
                   (((IdVar(DName((var_name, i_13)), _scope_info))))));
                Right(i_token_comma);
                Left(Arg((Sc(C(String((a_string, i_token_string)))))))
               ],
               i_right_paren))
             )
           ->
            i_token_comma.PI.transfo <- PI.Remove;
            i_token_string.PI.transfo <- PI.Remove;

        (* similar pattern except we accept any kind of expression as
         * the second argument, not just constant strings.
         *)
        |
             (Call(
               ClassGet (Id (XName[QI(Name(("ArgAssert", i_9)))]), i_10,
                         Id (XName[QI(Name(("isString", i_11)))])),
               (i_left_paren,
               [Left(
                 Arg(
                   (((IdVar(DName((var_name, i_13)), _scope_info))))));
                Right(i_token_comma);
                Left(Arg(an_expr))
               ],
               i_right_paren))
             )
           ->
            (* let's get all the tokens composing the expression *)
            let tokens_in_expression =
              Lib_parsing_php.ii_of_any (Expr an_expr) in

            tokens_in_expression |> List.iter (fun tok ->
              tok.PI.transfo <- PI.Remove;
            );
            i_token_comma.PI.transfo <- PI.Remove;
        | _ -> k expr
      );
    }
    in
    visitor (Program ast);

*)

    (* step3: unparse the annotated AST and show the diff *)
(*
    let s =
      Unparse_php.string_of_program_with_comments_using_transfo (ast, tokens) in

*)
    let s = "TODO" in
    let tmpfile = Common.new_temp_file "trans" ".php" in
    Common.write_file ~file:tmpfile s;

    let diff = Common.cmd_to_list (spf "diff -u %s %s" file tmpfile) in
    diff |> List.iter pr;

    if !apply_patch
    then Common.write_file ~file:file s;
  )


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let _ =
  main (Array.to_list Sys.argv)

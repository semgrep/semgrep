(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

open Parse_info
module PI = Parse_info
module R = Refactoring_code

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * A syntactical patch. https://github.com/facebook/pfff/wiki/Spatch
 * Right now there is support for PHP, C/C++/ObjectiveC, OCaml, Java, and
 * Javascript.
 *
 * opti: git grep xxx | xargs spatch -e 's/foo()/bar()/'
 *
 *
 * Alternatives:
 *  - you could also just write a sgrep that put a special mark to the
 *    place where it matched and then do the transformation using an
 *    emacs macro leveraging those marks.
 *
 * history:
 *  - coccinelle for C
 *  - expression restricted spatch for PHP
 *  - generalized spatch for PHP, C++, and other using ast_fuzzy parsing
 *    technique, see h_program-lang/ast_fuzzy.ml top comment
 *
 * related:
 *  - sed, perl, codemod
 *  - http://www.jetbrains.com/idea/documentation/ssr.html
 *  - go has a replace option in its formating tool http://golang.org/cmd/gofmt/
 *    it has even an automatic API rewriter: http://golang.org/cmd/fix
 *    can do: gofmt -w -r 'x[i:len(x)] -> x[i:]' *.go
 *    see http://blog.golang.org/go-fmt-your-code
 *  - rust format https://github.com/pcwalton/rustfmt
 *  - clang-format http://llvm.org/devmtg/2013-04/jasper-slides.pdf
 *  - todo: http://www.refactory.com/tools/refactoring-browser/rewrite-tool
 *  - todo: https://catalog.comby.dev/ multi-language transformation!
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

let spatch_file = ref ""
let sed_string = ref ""

(* todo: infer from basename argv(0) ? *)
let lang = ref "php"

let apply_patch = ref false
(* too experimental for now *)
let pretty_printer = ref false

let case_sensitive = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2 s =
  if !verbose then Common.pr2 s

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

let parse_pattern file =
  match Lang_fuzzy.lang_of_string_opt !lang with
  | Some lang ->
      Right (Spatch_fuzzy.parse
                ~pattern_of_string:(Parse_fuzzy.parse_pattern lang)
                ~ii_of_pattern:Lib_ast_fuzzy.toks_of_trees
                file)
  | None ->
    (match !lang with
    | "php" ->
        raise Todo
(*        Left (Spatch_php.parse file) *)
    | _ -> failwith ("unsupported language for the pattern: " ^ !lang)
    )

let spatch pattern file =
 try (
  match Lang_fuzzy.lang_of_string_opt !lang, pattern with
  | Some lang, Right pattern ->
     let trees, toks = Parse_fuzzy.parse_and_tokens_with_lang lang file in
     let was_modified = Spatch_fuzzy.spatch pattern trees in
     if was_modified
     then Some (Lib_unparser.string_of_toks_using_transfo toks)
     else None

  | None, Left _pattern ->
    (match !lang with
    | "php" ->
(*
      (try
        Spatch_php.spatch ~case_sensitive:!case_sensitive pattern file
      with Parse_php.Parse_error tok ->
        failwith ("PARSING PB: " ^ Parse_info.error_message_info tok);
      )
*)
      raise Todo
    | _ -> failwith ("unsupported language: " ^ !lang)
    )
  | _ -> raise Impossible
  ) with exn ->
     pr2 (spf "PB with %s, exn = %s"  file (Common.exn_to_s exn));
     None


(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  let spatch_file =
    match !spatch_file, !sed_string with
    | "", "" ->
      failwith "I need a semantic patch file; use -f"
    | "", s ->
        (* does not handle nested /, does not handle s# alternate
         * syntax as in perl, does not handle much ...
         *)
        if s =~ "s/\\(.*\\)/\\(.*\\)/"
        then begin
          let (before, after) = Common.matched2 s in
          let tmpfile = Common.new_temp_file "spatch" ".spatch" in
          Common.with_open_outfile tmpfile (fun (pr, _chan) ->
            pr (spf "- %s\n" before);
            if after <> ""
            then pr (spf "+ %s\n" after);
          );
          tmpfile
        end
        else failwith ("wrong format, use s/.../.../ not: " ^ s)
    | _s, "" ->
        !spatch_file
    | _s1, _s2 ->
        failwith "Can't use -f and -e at the same time"
  in

  let pattern = parse_pattern spatch_file in
  let files =
    Find_source.files_of_dir_or_files ~lang:!lang xs in

  files |> Console.progress ~show:!verbose (fun k ->
   List.iter (fun file->
    k();
    let resopt = spatch pattern file in
    resopt |> Common.do_option (fun (s) ->
      pr2 (spf "transforming: %s" file);

      let tmpfile = Common.new_temp_file "trans" ".spatch" in
      Common.write_file ~file:tmpfile s;

      if !pretty_printer && !lang =$= "php"
      then raise Todo;
(*
Unparse_pretty_print_mix.pretty_print_when_need_it
             ~oldfile:file ~newfile:tmpfile;
*)

      let diff = Common2.unix_diff file tmpfile in
      diff |> List.iter pr;
      if !apply_patch
      then Common.write_file ~file:file (Common.read_file tmpfile);
    )
  ))

(*****************************************************************************)
(* Helpers for PHP *)
(*****************************************************************************)

(* Some helper functions when using the low-level transformation API of pfff.
 * You should use the spatch DSL if you can.
 *)
type transformation = {
  (* Works by side effect on the tokens in the AST, see the transfo field.
   * It returns a boolean indicating whether any transformation was done.
   *)
  trans_func: Cst_php.program -> bool;
  (* optimisation; if a file does not contain certain keywords we don't
   * even have to parse it
   *)
  grep_keywords: string list option;
}

let apply_transfo transfo xs =

  let files = Lib_parsing_php.find_source_files_of_dir_or_files xs in
  let pbs = ref [] in
  (* xhp and transformation was not mixing well, but now it's better
   * thanks to builtin xhp support
   *)
  Flag_parsing.show_parsing_error := false;
  Flag_parsing.verbose_lexing := false;

  files |> Console.progress (fun k -> List.iter (fun file ->
    let file = Common2.relative_to_absolute file in
    pr2 (spf "processing: %s" file);
    k();

    let worth_trying =
      match transfo.grep_keywords with
      | None -> true
      | Some xs -> Common2.contain_any_token_with_egrep xs file
    in
    if not worth_trying then ()
    else
    try (
    let (ast, toks) =
      try
        Parse_php.parse file |> fst
      with Parse_info.Parsing_error _err ->
        Common.pr2 (spf "warning: parsing problem in %s" file);
        [], []
    in
    let was_modified = transfo.trans_func ast in

    (* old:
     * let patch = Patch.generate_patch !edition_cmds
     * ~filename_in_project:file file in
     * patch +> List.iter pr
     *)

    if was_modified then begin
      let s =
        Unparse_php.string_of_program_with_comments_using_transfo (ast, toks)
      in

      let tmpfile = Common.new_temp_file "trans" ".php" in
      Common.write_file ~file:tmpfile s;

      let diff = Common2.unix_diff file tmpfile in
      diff |> List.iter pr;

      if !apply_patch
      then Common.write_file ~file:file s;
    end
    ) with exn ->
      Common.push (spf "PB with %s, exn = %s" file (Common.exn_to_s exn)) pbs;
  ));
  !pbs |> List.iter Common.pr2

let apply_refactoring _refactoring file =
  let _ast_and_toks, _ = Parse_php.parse file in
  let s =
  (*  Refactoring_code_php.refactor [refactoring] ast_and_toks  *) raise Todo
  in
  let tmpfile = Common.new_temp_file "trans" ".spatch" in
  Common.write_file ~file:tmpfile s;
  let diff = Common2.unix_diff file tmpfile in
  diff |> List.iter pr;
  if !apply_patch
  then Common.write_file ~file:file (Common.read_file tmpfile);
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

open Cst_php
module V = Visitor_php

(* -------------------------------------------------------------------------*)
(* to test *)
(* -------------------------------------------------------------------------*)

(* see also demos/simple_refactoring.ml *)
let simple_transfo xs =

  let files = Lib_parsing_php.find_source_files_of_dir_or_files xs in

  Flag_parsing.show_parsing_error := false;
  Flag_parsing.verbose_lexing := false;
  files |> List.iter (fun file ->
    pr2 (spf "processing: %s" file);

    let (ast, toks), _ = Parse_php.parse file in

    let hook = { Visitor_php.default_visitor with
      Visitor_php.kexpr = (fun (k, _) x ->
        match x with
        | Call(Id(XName[QI (Name ("foo", info_foo))]), (_, _args, _)) ->
            pr2 "found match";

            let ii = Lib_parsing_php.ii_of_any (Expr x) in
            ii |> List.iter (fun info ->
              info.transfo <- Remove
            );
            info_foo.transfo <- Replace (AddStr "1");
            ()
        | _ -> k x
      );
    }
    in
    (Visitor_php.mk_visitor hook) (Program ast);

    let s =
      Unparse_php.string_of_program_with_comments_using_transfo (ast, toks) in

    let tmpfile = Common.new_temp_file "trans" ".php" in
    Common.write_file ~file:tmpfile s;

    let diff = Common2.unix_diff file tmpfile in
    diff |> List.iter pr;
  );
  ()
(* -------------------------------------------------------------------------*)
(* Trailing comma transformation *)
(* -------------------------------------------------------------------------*)
let all_different xs =
  List.length xs = (xs |> Common.sort |> Common2.uniq |> List.length)

(* todo:
 * - julien thinks we should transform even if some commas
 *   are on the same line, as long as the last ')' is on a different line.
 * - we should also transform function decl and use() lists.
 *)
let add_trailing_comma_multiline_funcalls ast =
  let was_modified = ref false in
  let visitor = V.mk_visitor { V.default_visitor with
    V.karguments = (fun (k, _) (lp, args, rp) ->
      if List.length args >= 1 then begin
        let (args, commas) = Common.partition_either (fun x -> x) args in
        let lines_commas =
          commas |> List.map Parse_info.line_of_info
        in
        let line_rp = Parse_info.line_of_info rp in
        let last_expr = Common2.list_last args in
        let ii = Lib_parsing_php.ii_of_any (Argument last_expr) in
        let (_min, max) =
          Parse_info.min_max_ii_by_pos ii in
        let line_last_expr = Parse_info.line_of_info max in

        if List.length args > 2 &&
          all_different (line_last_expr::line_rp::lines_commas) then
          begin
            max.transfo <- AddAfter (AddStr ",");
            was_modified := true;
          end;
      end;
      k (lp, args, rp)
    );
  }
  in
  visitor (Program ast);
  !was_modified

let trailing_comma_transfo = {
  trans_func = add_trailing_comma_multiline_funcalls;
  grep_keywords = None;
}

(* -------------------------------------------------------------------------*)
(* An example of an XHP transformation *)
(* -------------------------------------------------------------------------*)

(*
 * The transformation below can be encoded via this syntactical patch:
 *
 * <ui:section-header
 * -   border=X
 * ></ui:section-header>
 *
 * There are nevertheless currenty a few limitations in spatch which
 * will cause this syntactical patch to fail to transform all the
 * relevant code (e.g. because of some XHP isomorphisms not yet
 * handled by spatch). In the mean time the code below uses the low-level
 * transformation API of pfff to express the same transformation;
 * it can be used as a reference for spatch to compare with.
 *)

let remove_border_attribute ast =
  let was_modified = ref false in

  (* $ ./pfff -dump_php_ml tests/php/spatch/border_var.php
   *
   * [StmtList(
   *  [ExprStmt(
   *    (Assign((Var(DName(("a", i_1)), Ref(NoScope)), tlval_2), i_3,
   *       (XhpHtml(
   *          Xhp((["ui"; "section-header"], i_4),
   *            [(("href", i_5), i_6,
   *              XhpAttrString(i_7, [EncapsString(("foo", i_8))], i_9));
   *             (("border", i_10), i_11,
   *              XhpAttrString(i_12, [EncapsString(("1", i_13))], i_14))],
   *            i_15, [XhpText(("
   *   This is nuts
   *   ", i_16))],
   *            (Some(["ui"; "section-header"]), i_17))),
   *        t_18)),
   *     t_19), i_20)]); FinalDef(i_21)]
   *)
  let visitor = V.mk_visitor { V.default_visitor with
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp ( (["ui"; "section-header"], _), attributes, _, _, _)
      | XhpSingleton ( (["ui"; "section-header"], _), attributes, _) ->

          attributes |> List.iter (fun attr ->
            match attr with
            | (("border", _tok_border), _tok_equal, _xhp_attr_value) ->
                was_modified := true;
                let tokens = Lib_parsing_php.ii_of_any (XhpAttribute attr) in
                tokens |> List.iter (fun tok ->
                  tok.transfo <- Remove;
                );
            | _ -> ()
          );
          (* recurse *)
          k x
      (* recurse *)
      | _ -> k x
    );
  }
  in
  visitor (Program ast);
  !was_modified

let remove_border_attribute_transfo = {
  trans_func = remove_border_attribute;
  grep_keywords = Some ["border"];
}

(* -------------------------------------------------------------------------*)
(* Add action attribute to <ui:form> *)
(* -------------------------------------------------------------------------*)

(*
 * Here is the spatch (disjunction are currently not supported in spatch
 * so have to do things manually):
 *
 *   <ui:form
 * (
 *  action=...
 * |
 *  + action="#"
 * )
 *  >
 *
 * todo: maybe we could generalize this transformation and add to
 * spatch some command lines like:
 *  *  -add_xhp_attribute_if_not_there "ui:form" "action='#'"
 *  *  -remove_xhp_attribute "ui:form" "border"
 * a la lex-pass. Or maybe it's better to add support in the DSL directly
 * so one can write the syntactical patch above.
 *)
let add_action_ui_form_transfo_func ast =
  let was_modified = ref false in
  let visitor = V.mk_visitor { V.default_visitor with
    V.kxhp_html = (fun (k, _) xhp ->
      (* mostly copy paste of: pfff -dump_php tests/.../ui_form.php *)
      match xhp with
      | XhpSingleton((["ui"; "form"], info_tag), attributes, _)
      | Xhp ((["ui"; "form"], info_tag), attributes, _, _, _)
        ->
          if not (attributes |>
                  List.exists (fun ((attr_name,_), _tok, _attr_val) ->
                    attr_name = "action"
          ))
          then begin
            was_modified := true;
            info_tag.PI.transfo <- PI.AddAfter (PI.AddStr " action=\"#\"");
          end;
          k xhp
      | _ -> k xhp (* call the continuation *)
    );
  }
  in
  visitor (Program ast);
  !was_modified

let add_action_ui_form_transfo = {
  trans_func = add_action_ui_form_transfo_func;
  grep_keywords = Some ["ui:form"];
}


(*---------------------------------------------------------------------------*)
(* pretty printer testing *)
(*---------------------------------------------------------------------------*)
let test_pp file =
  let _tokens = Parse_php.tokens file in
  let _ast = Parse_php.parse_program file in

  let _ast =
    (*Ast_pp_build.program_with_comments tokens ast  *) raise Todo
  in

  let buf = Buffer.create 256 in
(*
  let env = Pretty_print_code.empty (Buffer.add_string buf) in
  Pretty_print.program_env env ast;
*)
  let s = Buffer.contents buf in

  let tmp_file = Common.new_temp_file "pp" ".php" in
  Common.write_file ~file:tmp_file s;
  let xs = Common2.unix_diff file tmp_file in
  xs |> List.iter pr2

(*---------------------------------------------------------------------------*)
(* juju refactorings *)
(*---------------------------------------------------------------------------*)

let juju_refactoring spec_file =
  let xs = Refactoring_code.load spec_file in

  (* Group by file because we need to do all the transfo on a file
   * before unparsing as the refactoring spec use line/col position
   * that refers to the original file
   *)
  let xxs =
    xs |> List.map (fun x ->
      match x with
      | (_a, Some pos) ->
        pos.Refactoring_code.file, x
      | _ -> failwith "no file position"
    )
    |> Common.group_assoc_bykey_eff
  in
  xxs |> List.iter (fun (file, _refactorings) ->
    let (_ast2, _stat) = Parse_php.parse file in
    let s =
    (*  Refactoring_code_php.refactor refactorings ast2  *) raise Todo
    in

    let tmpfile = Common.new_temp_file "trans" ".php" in
    Common.write_file ~file:tmpfile s;

    if !pretty_printer
    then (* Unparse_pretty_print_mix.pretty_print_when_need_it
      ~oldfile:file ~newfile:tmpfile; *) raise Todo;

    let diff = Common2.unix_diff file tmpfile in
    diff |> List.iter pr;
    if !apply_patch
    then Common.write_file ~file:file (Common.read_file tmpfile);
  );
  ()

(*---------------------------------------------------------------------------*)
(* case refactorings *)
(*---------------------------------------------------------------------------*)
(* todo: handke Xxx.$yyy
 * Also avoid false positive change like s/TIME/Time/ where
 * in some files you must do it only selectively, when on
 * TIME::sec_day, but not on TimelineProfile::TIME
 *)
let case_refactoring pfff_log =
  let xs = Common.cat pfff_log in
  let simple_rename =
    xs |> Common.map_filter (fun s ->
   (*ex: CASE SENSITIVITY: SmsConst instead of SMSConst at /path/file:176:18 *)
      if s =~
        ("CASE SENSITIVITY: \\([A-Za-z_0-9\\.]+\\) " ^
         "instead of \\([A-Za-z_0-9\\.]+\\) " ^
         "at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)")
      then
        let (actual, expected, file, line, col) = Common.matched5 s in
        if file =~
          (match 0 with
          | 0 -> ".*/www-git/"
          | 1 -> ".*/www-git/\\(lib\\|scripts\\|tests\\)/*"
          | 2 -> ".*/www-git/flib/[a-f].*"
          | 3 -> ".*/www-git/flib/[g-m].*"
          | 4 -> ".*/www-git/flib/[n-r].*"
          | 5 -> ".*/www-git/flib/[s-z].*"
          | _ -> raise Impossible
          )
        then
          Some (actual, expected, file, s_to_i line, s_to_i col)
        else None
      else None
    )
  in
  simple_rename |> List.iter (fun (actual, expected, file, _, _) ->
    let xs = Common.split "\\." actual in
    let ys = Common.split "\\." expected in
    (match xs, ys with
    | [a1], [b1]  ->
      pr2_gen actual;
      Common.command2 (spf
        "%s/spatch -lang phpfuzzy --apply-patch -e 's/%s/%s/' %s"
        Config_pfff.path_pfff_home
        a1 b1 file);
    | [a1;_a2], [b1;_b2] when a1 <> b1 ->
      (* old:
       *  Common.command2 (spf "perl -p -i -e 's/\\b%s\\b/%s/g' %s" a1 b1 file);
       * but got some FPs when s/crud/Crud/ on certain files because it also
       * uppercased a URL (see D867035). So use spatch, safer.
       *)
      pr2_gen actual;
      Common.command2 (spf
        "%s/spatch -lang phpfuzzy --apply-patch -e 's/%s/%s/' %s"
        Config_pfff.path_pfff_home
        a1 b1 file);
      ()
    | [a1;a2], [b1;b2] when a1 = b1 && a2 <> b2 ->
      Common.command2 (spf
        "%s/spatch -lang phpfuzzy --apply-patch -e 's/%s/%s/' %s"
        Config_pfff.path_pfff_home
        a2 b2 file);

    | _ ->
      ()

  )
  )


(*---------------------------------------------------------------------------*)
(* remove undefined xhp field *)
(*---------------------------------------------------------------------------*)
let remove_undefined_xhp_field (xhp_class_str, field) ast=
  let was_modified = ref false in
  let string_of_xhp_tag xs = "<" ^ Common.join ":" xs ^ ">" in
  let visitor = Visitor_php.mk_visitor {Visitor_php.default_visitor with
    Visitor_php.kxhp_html = (fun (k, _) x ->
      match x with
      | XhpSingleton ( (xhp_class, _), attributes, _)
      | Xhp ( (xhp_class, _), attributes, _, _, _)
          when (xhp_class_str = string_of_xhp_tag xhp_class) ->
        attributes |> List.iter (fun attr ->
          match attr with
          | ((s, _), _, _) when (s = field) ->
            let ii = Lib_parsing_php.ii_of_any (XhpAttribute attr) in
            ii |> List.iter (fun info ->
              info.transfo <- Remove
            );
            was_modified := true;
          | _ -> ()
        ); k x
      | _ -> k x
    );
  }
  in
  visitor (Program ast);
  !was_modified

let read_log_undefined_xhp_field pfff_log =
  let xs = Common.cat pfff_log in
  let undefined_xhp_field =
    xs |> Common.map_filter (fun s ->
      (* PB: lookup fail on Field:<x:misc>.xnosuchstr= (at xhp_use.php:9) *)
      if s =~
        ("PB: lookup fail on Field:\\(<[A-Za-z_0-9:-]+>\\)\\." ^
            "\\([A-Za-z_0-9-]+\\)= " ^
            "(at \\([^:]+\\):\\([0-9]+\\))")
      then
        let (xhp_class_str, field, filename, line) = Common.matched4 s in
        Some (xhp_class_str, field, filename, s_to_i line)
      else None
    )
  in
  undefined_xhp_field |> List.iter
    (fun  (xhp_class_str, field, filename, _) ->
      let transfo ={
        trans_func = remove_undefined_xhp_field (xhp_class_str, field);
        grep_keywords = None;
      } in
      apply_transfo transfo [filename]
    )

(*---------------------------------------------------------------------------*)
(* array refactoring *)
(*---------------------------------------------------------------------------*)

let array_to_int_array_transfo line_closing_paren_array replacment = {
  grep_keywords = None;
  trans_func = (fun ast ->
    let visitor = V.mk_visitor { V.default_visitor with
      V.kexpr = (fun (k, _) e ->
        match e with
        | ArrayLong (tok, (_lp, _body, rp)) ->
          let line_rp = PI.line_of_info rp in
          if line_rp = line_closing_paren_array
          then begin
            tok.transfo <- Replace (AddStr replacment);
          end
        | _ -> k e
      );
    }
    in
    visitor (Program ast);
    true (* was_modified *)
  )
}

let array_to_int_array_ptc logfile =
  logfile |> Common.cat |> List.iter (fun s ->
    if s =~ "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$"
    then
      let (file, linestr, replacment) = Common.matched3 s in
      let line = int_of_string linestr in
      apply_transfo (array_to_int_array_transfo line replacment) [file]
    else failwith (spf "wrong format, expect a string * int: %s" s)
  )

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)

open OUnit
let test () =

  let suite = "spatch" >::: [
  (* ugly: todo: use a toy fuzzy parser instead of the one in lang_cpp/ *)
    Unit_fuzzy.spatch_fuzzy_unittest
      ~ast_fuzzy_of_string:(fun str ->
        Common2.with_tmp_file ~str ~ext:"cpp" (fun tmpfile ->
          Parse_cpp.parse_fuzzy tmpfile |> fst
        ))
      ~parse_file:(fun file ->
        Common.save_excursion Flag_parsing.verbose_lexing false (fun () ->
          Parse_cpp.parse_fuzzy file
        ))
        ;
(*
    Unit_matcher_php.spatch_unittest;
    Unit_matcher_php.refactoring_unittest;
*)
  ]
  in
  OUnit.run_test_tt suite |> ignore;
  ()

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let spatch_extra_actions () = [
  (* see also demos/simple_refactoring.ml *)
  "-simple_transfo", " <files_or_dirs>",
  Common.mk_action_n_arg (simple_transfo);

  "-remove_border_attribute", " <files_or_dirs>",
  Common.mk_action_n_arg (apply_transfo remove_border_attribute_transfo);
  "-add_action_ui_form", " <files_or_dirs>",
  Common.mk_action_n_arg (apply_transfo add_action_ui_form_transfo);
  "-add_trailing_comma", " <files_or_dirs>",
  Common.mk_action_n_arg (apply_transfo trailing_comma_transfo);

  "-juju_refactoring", " <file>",
  Common.mk_action_1_arg juju_refactoring;
  "-case_refactoring", " <file>",
  Common.mk_action_1_arg case_refactoring;
  "-remove_undefined_xhp_field", " <file>",
  Common.mk_action_1_arg read_log_undefined_xhp_field;
  "-add_interface", " <class>:<interface> <file>",
  Common.mk_action_2_arg (fun str file ->
    if str =~ "\\(.*\\):\\(.*\\)"
    then
      let (classname, interface) = Common.matched2 str in
      let refactoring = R.AddInterface (Some classname, interface), None in
      apply_refactoring refactoring file
    else failwith "use the CLASS:INTERFACE format for -add_interface"
  );
  "-remove_interface", " <class>:<interface> <file>",
  Common.mk_action_2_arg (fun str file ->
    if str =~ "\\(.*\\):\\(.*\\)"
    then
      let (classname, interface) = Common.matched2 str in
      let refactoring = R.RemoveInterface (Some classname, interface), None in
      apply_refactoring refactoring file;
    else failwith "use the CLASS:INTERFACE format for -remove_interface"
  );
  "-array_to_int_array", " <specfile>",
  Common.mk_action_1_arg (fun specfile ->
    array_to_int_array_ptc specfile
  );

  "-test", " run regression tests",
  Common.mk_action_0_arg test;
  "-pp", "",
  Common.mk_action_1_arg test_pp;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
 spatch_extra_actions()@
 []

let options () =
  [
    "-lang", Arg.Set_string lang,
    (spf " <str> choose language (default = %s)" !lang);

    "-f", Arg.Set_string spatch_file,
    " <spatch_file>";
    "-e", Arg.Set_string sed_string,
    " <s/before/after/>, sed mode";

    "--apply-patch", Arg.Set apply_patch,
    " ";
    "--pretty-printer", Arg.Set pretty_printer,
    " reindent the modified code (fragile)";

    "--case-sensitive", Arg.Set case_sensitive,
    " match code in a case sensitive manner\n";

  ] @
  (* Flag_parsing_php.cmdline_flags_pp () ++ *)
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [
    "--verbose", Arg.Set verbose,
    " ";
    "-v", Arg.Set verbose,
    " shortcut for --verbose";
    "-version",   Arg.Unit (fun () ->
      Common.pr2 (spf "spatch version: %s" Config_pfff.version);
      exit 0;
    ),
    "  guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  let usage_msg =
    spf "Usage: %s [options] <file or dir> \nDoc: %s\nOptions:"
      (Common2.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Spatch"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) ->
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) ->
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs ->
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] ->
        Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
      main ();
  )

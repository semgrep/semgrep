(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
module Flag = Flag_semgrep
module PI = Parse_info
module E = Semgrep_error_code
module Out = Output_from_core_t
module OutH = Output_from_core_util

let logger = Logging.get_logger [ __MODULE__ ]

(* To get a better backtrace, to better debug parse errors *)
let debug_exn = ref false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly a wrapper around pfff Parse_generic, but which can also use
 * tree-sitter parsers when possible.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type parsing_result = {
  ast : AST_generic.program;
  (* partial errors tree-sitter was able to recover from *)
  skipped_tokens : PI.token_location list;
  stat : Parse_info.parsing_stat;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type 'ast parser =
  | Pfff of (Common.filename -> 'ast * Parse_info.parsing_stat)
  | TreeSitter of (Common.filename -> 'ast Tree_sitter_run.Parsing_result.t)

type 'ast internal_result =
  | Ok of ('ast * Parse_info.parsing_stat)
  | Partial of 'ast * PI.token_location list * Parse_info.parsing_stat
  | Error of exn * Printexc.raw_backtrace

let loc_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  {
    PI.str = err.substring;
    charpos = 0;
    (* fake *)
    line = start.row + 1;
    column = start.column;
    file = err.file.name;
  }

let exn_of_loc loc =
  let info = { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo } in
  PI.Parsing_error info

let error_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let loc = loc_of_tree_sitter_error err in
  exn_of_loc loc

let errors_from_skipped_tokens xs =
  match xs with
  | [] -> []
  | x :: _ ->
      let exn = exn_of_loc x in
      let err = E.exn_to_error x.PI.file exn in
      let locs = xs |> Common.map OutH.location_of_token_location in
      [ { err with typ = Out.PartialParsing locs } ]

let stat_of_tree_sitter_stat file (stat : Tree_sitter_run.Parsing_result.stat) =
  {
    Parse_info.filename = file;
    total_line_count = stat.total_line_count;
    error_line_count = stat.error_line_count;
    have_timeout = false;
    commentized = 0;
    problematic_lines = [];
  }

let (run_parser : 'ast parser -> Common.filename -> 'ast internal_result) =
 fun parser file ->
  match parser with
  | Pfff f ->
      Common.save_excursion Flag_parsing.show_parsing_error false (fun () ->
          logger#trace "trying to parse with Pfff parser %s" file;
          try
            let res = f file in
            Ok res
          with
          | Timeout _ as e -> raise e
          | exn ->
              (* TODO: print where the exception was raised or reraise *)
              logger#error "exn (%s) with Pfff parser" (Common.exn_to_s exn);
              Error (exn, Printexc.get_raw_backtrace ()))
  | TreeSitter f -> (
      logger#trace "trying to parse with TreeSitter parser %s" file;
      try
        let res = f file in
        let stat = stat_of_tree_sitter_stat file res.stat in
        match (res.program, res.errors) with
        | None, [] ->
            let msg =
              "internal error: failed to recover typed tree from tree-sitter's \
               untyped tree"
            in
            Error (Failure msg, Printexc.get_callstack 100)
        | Some ast, [] -> Ok (ast, stat)
        | None, ts_error :: _xs ->
            let exn = error_of_tree_sitter_error ts_error in
            logger#error "non-recoverable error (%s) with TreeSitter parser"
              (Common.exn_to_s exn);
            Error (exn, Printexc.get_callstack 100)
        | Some ast, x :: xs ->
            (* Note that the first error is probably the most important;
             * the following one may be due to cascading effects *)
            logger#error "partial errors (%d) with TreeSitter parser"
              (List.length (x :: xs));
            let locs = x :: xs |> Common.map loc_of_tree_sitter_error in
            Partial (ast, locs, stat)
      with
      | Timeout _ as e -> raise e
      (* to get correct stack trace on parse error *)
      | exn when !debug_exn -> raise exn
      | exn ->
          let trace = Printexc.get_raw_backtrace () in
          logger#error "exn (%s) with TreeSitter parser" (Common.exn_to_s exn);
          Error (exn, trace))

let rec (run_either :
          Common.filename -> 'ast parser list -> 'ast internal_result) =
 fun file xs ->
  match xs with
  | [] ->
      Error
        (Failure (spf "no parser found for %s" file), Printexc.get_callstack 100)
  | p :: xs -> (
      let res = run_parser p file in
      match res with
      | Ok ast -> Ok ast
      | Partial (ast, errs, stat) -> (
          let res = run_either file xs in
          match res with
          | Ok res -> Ok res
          | Error (exn2, trace2) ->
              logger#debug "exn again (%s) but return Partial:\n%s"
                (Common.exn_to_s exn2)
                (Printexc.raw_backtrace_to_string trace2);
              (* prefer a Partial to an Error *)
              Partial (ast, errs, stat)
          | Partial _ ->
              logger#debug "Partial again but return first Partial";
              Partial (ast, errs, stat))
      | Error (exn1, trace1) -> (
          let res = run_either file xs in
          match res with
          | Ok res -> Ok res
          | Partial (ast, errs, stat) ->
              logger#debug "Got now a Partial, better than exn (%s)"
                (Common.exn_to_s exn1);
              Partial (ast, errs, stat)
          | Error (exn2, _trace2) ->
              logger#debug "exn again (%s) but return original exn (%s)"
                (Common.exn_to_s exn2) (Common.exn_to_s exn1);
              (* prefer the first error *)
              Error (exn1, trace1)))

let (run :
      Common.filename ->
      'ast parser list ->
      ('ast -> AST_generic.program) ->
      parsing_result) =
 fun file xs fconvert ->
  let xs =
    match () with
    | _ when !Flag.tree_sitter_only ->
        xs
        |> Common.exclude (function
             | Pfff _ -> true
             | _ -> false)
    | _ when !Flag.pfff_only ->
        xs
        |> Common.exclude (function
             | TreeSitter _ -> true
             | _ -> false)
    | _ -> xs
  in
  match run_either file xs with
  | Ok (ast, stat) -> { ast = fconvert ast; skipped_tokens = []; stat }
  | Partial (ast, skipped_tokens, stat) ->
      { ast = fconvert ast; skipped_tokens; stat }
  | Error (exn, trace) -> Printexc.raise_with_backtrace exn trace

let throw_tokens f file =
  let res = f file in
  (res.PI.ast, res.PI.stat)

let lang_to_python_parsing_mode = function
  | Lang.Python -> Parse_python.Python
  | Lang.Python2 -> Parse_python.Python2
  | Lang.Python3 -> Parse_python.Python3
  | s -> failwith (spf "not a python language:%s" (Lang.to_string s))

let rec just_parse_with_lang lang file =
  match lang with
  | Lang.Ruby ->
      (* for Ruby we start with the tree-sitter parser because the pfff parser
       * is not great and some of the token positions may be wrong.
       *)
      run file
        [
          TreeSitter Parse_ruby_tree_sitter.parse;
          (* right now the parser is verbose and the token positions
           * may be wrong, but better than nothing. *)
          Pfff (throw_tokens Parse_ruby.parse);
        ]
        Ruby_to_generic.program
  | Lang.Java ->
      run file
        [
          (* we used to start with the pfff one; it was quite good and faster
           * than tree-sitter (because we need to wrap tree-sitter inside
           * an invoke because of a segfault/memory-leak), but when both parsers
           * fail, it's better to give the tree-sitter parsing error now.
           *)
          TreeSitter Parse_java_tree_sitter.parse;
          Pfff (throw_tokens Parse_java.parse);
        ]
        Java_to_generic.program
  | Lang.Go ->
      run file
        [
          TreeSitter Parse_go_tree_sitter.parse;
          Pfff (throw_tokens Parse_go.parse);
        ]
        Go_to_generic.program
  | Lang.Js ->
      (* we start directly with tree-sitter here, because
       * the pfff parser is slow on minified files due to its (slow) error
       * recovery strategy.
       *)
      run file
        [
          TreeSitter (Parse_typescript_tree_sitter.parse ~dialect:`TSX);
          Pfff (throw_tokens Parse_js.parse);
        ]
        Js_to_generic.program
  | Lang.Ts ->
      run file
        [ TreeSitter (Parse_typescript_tree_sitter.parse ?dialect:None) ]
        Js_to_generic.program
  (* there is no pfff parsers for C#/Kotlin/... so let's go directly to
   *  tree-sitter, and there's no ast_xxx.ml either so we directly generate
   * a generic AST (no xxx_to_generic here)
   *)
  | Lang.Csharp ->
      run file [ TreeSitter Parse_csharp_tree_sitter.parse ] (fun x -> x)
  | Lang.Kotlin ->
      run file [ TreeSitter Parse_kotlin_tree_sitter.parse ] (fun x -> x)
  | Lang.Solidity ->
      run file [ TreeSitter Parse_solidity_tree_sitter.parse ] (fun x -> x)
  | Lang.Swift ->
      run file [ TreeSitter Parse_swift_tree_sitter.parse ] (fun x -> x)
  | Lang.Elixir ->
      run file [ TreeSitter Parse_elixir_tree_sitter.parse ] (fun x -> x)
  | Lang.Lua -> run file [ TreeSitter Parse_lua_tree_sitter.parse ] (fun x -> x)
  | Lang.Bash ->
      run file [ TreeSitter Parse_bash_tree_sitter.parse ] (fun x -> x)
  | Lang.Dockerfile ->
      run file [ TreeSitter Parse_dockerfile_tree_sitter.parse ] (fun x -> x)
  | Lang.Rust ->
      run file [ TreeSitter Parse_rust_tree_sitter.parse ] (fun x -> x)
  | Lang.C ->
      run file
        [
          (* this internally uses the CST for c++ *)
          Pfff (throw_tokens Parse_c.parse);
          TreeSitter Parse_c_tree_sitter.parse;
        ]
        C_to_generic.program
  (* use pfff *)
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      let parsing_mode = lang_to_python_parsing_mode lang in
      run file
        [ Pfff (throw_tokens (Parse_python.parse ~parsing_mode)) ]
        (* old: Resolve_python.resolve ast;
         * switched to call Naming_AST.ml to correct def and use tagger
         *)
        Python_to_generic.program
  | Lang.Json ->
      run file
        [
          Pfff
            (fun file ->
              (Parse_json.parse_program file, Parse_info.correct_stat file));
        ]
        Json_to_generic.program
  | Lang.Cpp ->
      run file
        [
          TreeSitter Parse_cpp_tree_sitter.parse;
          Pfff (throw_tokens Parse_cpp.parse);
        ]
        Cpp_to_generic.program
  | Lang.Ocaml ->
      run file
        [
          Pfff (throw_tokens Parse_ml.parse);
          TreeSitter Parse_ocaml_tree_sitter.parse;
        ]
        Ml_to_generic.program
  | Lang.Scala ->
      run file
        [ Pfff (throw_tokens Parse_scala.parse) ]
        Scala_to_generic.program
  | Lang.Php ->
      run file
        [
          Pfff
            (fun file ->
              (* TODO: at some point parser_php.mly should go directly
               * to ast_php.ml and we should get rid of cst_php.ml
               *)
              let cst, stat = throw_tokens Parse_php.parse file in
              (Ast_php_build.program cst, stat));
          (* TODO: can't put TreeSitter first, because we still use Pfff
           * to parse the pattern, and there must be mismatch between the
           * AST generated by Ast_php_build and Parse_php_tree_sitter.parse.
           *)
          TreeSitter Parse_php_tree_sitter.parse;
        ]
        Php_to_generic.program
  | Lang.Hack ->
      run file [ TreeSitter Parse_hack_tree_sitter.parse ] (fun x -> x)
  | Lang.R -> run file [ TreeSitter Parse_r_tree_sitter.parse ] (fun x -> x)
  | Lang.Yaml ->
      {
        ast = Yaml_to_generic.program file;
        skipped_tokens = [];
        stat = Parse_info.default_stat file;
      }
  | Lang.Html ->
      (* less: there is an html parser in pfff too we could use as backup *)
      run file [ TreeSitter Parse_html_tree_sitter.parse ] (fun x -> x)
  | Lang.Vue ->
      let parse_embedded_js file =
        let { ast; skipped_tokens; stat = _ } =
          just_parse_with_lang Lang.Js file
        in
        (* TODO: pass the errors down to Parse_vue_tree_sitter.parse
         * and accumulate with other vue parse errors
         *)
        if skipped_tokens <> [] then failwith "parse error in embedded JS";
        ast
      in
      run file
        [ TreeSitter (Parse_vue_tree_sitter.parse parse_embedded_js) ]
        (fun x -> x)
  | Lang.Hcl -> run file [ TreeSitter Parse_hcl_tree_sitter.parse ] (fun x -> x)
  [@@profiling]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_and_resolve_name lang file =
  if lang = Lang.C && Sys.file_exists !Flag_parsing_cpp.macros_h then
    Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;

  let res = just_parse_with_lang lang file in
  let ast = res.ast in

  (* to be deterministic, reset the gensym; anyway right now semgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
   *)
  AST_generic_helpers.gensym_counter := 0;
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow lang ast;
  if !Flag.use_bloom_filter then Bloom_annotation.annotate_program ast;

  logger#info "Parse_target.parse_and_resolve_name_use_pfff_or_treesitter done";
  res
  [@@profiling]

(* used in test files *)
let parse_and_resolve_name_warn_if_partial lang file =
  let { ast; skipped_tokens; _ } = parse_and_resolve_name lang file in
  if skipped_tokens <> [] (* nosemgrep *) then
    pr2 (spf "WARNING: fail to fully parse %s" file);
  ast

let parse_and_resolve_name_fail_if_partial lang file =
  let { ast; skipped_tokens; _ } = parse_and_resolve_name lang file in
  if skipped_tokens <> [] then failwith (spf "fail to fully parse %s" file);
  ast

(*****************************************************************************)
(* For testing purpose *)
(*****************************************************************************)
(* was in pfff/.../Parse_generic.ml before *)
let parse_program file =
  let lang = List.hd (Lang.langs_of_filename file) in
  let res = just_parse_with_lang lang file in
  res.ast

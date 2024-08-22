(* Yoann Padioleau, Emma Jin
 *
 * Copyright (C) 2020 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
open IL
module G = AST_generic
module H = AST_generic_helpers
module D = Datalog_fact

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Experiment to use Datalog/DOOP/Souffle for taint analysis on generic AST.
 *
 * We perform a simple intraprocedural dataflow-based taint analysis in
 * ../tainting/Tainting_generic.ml using OCaml, but extending it to
 * be interprocedural, context-sensitive, field-sensitive, etc. could
 * be a lot of work. An alternative is to try to leverage all the work
 * done in the DOOP datalog-based state-of-the-art static analysis framework.
 *
 * Datalog is a very declarative language to express complex program analysis
 * in a very concise way[1]. In the last few years, it has also become a
 * practical solution thanks to the DOOP[2] framework and recent scalable free
 * datalog engines such as Souffle[3] (DOOP used to depend on LogixBlox
 * which had a commercial license, and previous Datalog-based framework
 * such as BDDBDDB could be hard to tune for performance).
 * Moreover, DOOP can not only perform complex point-analysis, it can now also
 * perform complex taint analysis[4].
 *
 * For more information see:
 * https://returntocorp.quip.com/O1S8AoOaw1Y9/Datalog-experiments-SouffleDOOP
 *
 * See also early experiments with Datalog using BDDBDDB instead
 * of DOOP/Souffle in:
 *  - h_program-lang/datalog_code.ml
 *  - h_program-lang/datalog_code.dl (and datalog_code.dtl)
 *  - lang_c/analyze/datalog_c.ml
 *  - pfff-mini/datalog_minic.ml
 *
 * References:
   -  [1] https://yanniss.github.io/points-to-tutorial15.pdf
   -  [2] https://bitbucket.org/yanniss/doop/src/master/docs/doop-101.md
   -  [3] https://souffle-lang.github.io/docs.html
   -  [4] https://yanniss.github.io/ptaint-oopsla17-prelim.pdf
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type env = { facts : Datalog_fact.t list ref }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let add env x = Stack_.push x env.facts

let todo any =
  let s = IL.show_any any in
  failwith (spf "Datalog_experiment: IL element not handled: %s" s)

let var_of_name _env name = spf "%s__%s" (fst name.ident) (G.SId.show name.sid)
let heap_of_int _env (_, tok) = spf "int %s" (Tok.content_of_tok tok)

(*****************************************************************************)
(* Fact extractor *)
(*****************************************************************************)
(* See c/analyze/datalog_c.ml for inspiration *)

let instr env x =
  match x.i with
  | Assign (lval, e) -> (
      match (lval, e.e) with
      | { base = Var n; rev_offset = [] }, Literal (G.Int pi) ->
          let v = var_of_name env n in
          let h = heap_of_int env pi in
          add env (D.PointTo (v, h))
      | _ -> todo (I x))
  | _ -> todo (I x)

let stmt env x =
  match x.IL.s with
  | Instr x -> instr env x
  | _ -> todo (S x)

let facts_of_function lang def =
  let xs = AST_to_IL.stmt lang (H.funcbody_to_stmt def.G.fbody) in
  let env = { facts = ref [] } in

  xs |> List.iter (stmt env);
  List.rev !(env.facts)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let gen_facts file outdir =
  let ast = Parse_target.parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;

  (* less: use treesitter also later
   *  Parse_code.parse_and_resolve_name_use_pfff_or_treesitter lang file in
   *)
  let facts = ref [] in

  let v =
    object (_self : 'self)
      inherit [_] AST_generic.iter_no_id_info

      method! visit_function_definition _env def =
        Stack_.push (facts_of_function lang def) facts
    end
  in
  v#visit_program () ast;

  let facts = !facts |> List.rev |> List_.flatten in
  (* nosemgrep: no-logs-in-library *)
  Logs.info (fun m ->
      m "generating %d facts in %s" (List.length facts) !!outdir);
  Datalog_io.write_facts_for_doop facts outdir

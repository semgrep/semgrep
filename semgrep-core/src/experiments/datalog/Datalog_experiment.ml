(* Yoann Padioleau, Emma Jin
 *
 * Copyright (C) 2020 r2c
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
open IL
module G = AST_generic
module H = AST_generic_helpers
module V = Visitor_AST
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
 *  - pfff/h_program-lang/datalog_code.ml
 *  - pfff/h_program-lang/datalog_code.dl (and datalog_code.dtl)
 *  - pfff/lang_c/analyze/datalog_c.ml
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
(* Dumper *)
(*****************************************************************************)

(* mostly a copy paste of pfff/lang_GENERIC/analyze/Test_analyze_generic.ml *)
let dump_il file =
  let lang = List.hd (Lang.langs_of_filename file) in
  let ast = Parse_target.parse_program file in
  Naming_AST.resolve lang ast;

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let s =
              AST_generic.show_any (G.S (H.funcbody_to_stmt def.G.fbody))
            in
            pr2 s;
            pr2 "==>";

            let xs = AST_to_IL.stmt lang (H.funcbody_to_stmt def.G.fbody) in
            let s = IL.show_any (IL.Ss xs) in
            pr2 s);
      }
  in
  v (G.Pr ast)
  [@@action]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let add env x = Common.push x env.facts

let todo any =
  let s = IL.show_any any in
  pr2 s;
  failwith "Datalog_experiment: TODO: IL element not handled (see above)"

let var_of_name _env name = spf "%s__%d" (fst name.ident) name.sid

let heap_of_int _env (_, tok) = spf "int %s" (Parse_info.str_of_info tok)

(*****************************************************************************)
(* Fact extractor *)
(*****************************************************************************)
(* See pfff/lang_c/analyze/datalog_c.ml for inspiration *)

let instr env x =
  match x.i with
  | Assign (lval, e) -> (
      match (lval, e.e) with
      | { base = Var n; offset = NoOffset }, Literal (G.Int s) ->
          let v = var_of_name env n in
          let h = heap_of_int env s in
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
  let lang = List.hd (Lang.langs_of_filename file) in
  let ast = Parse_target.parse_program file in
  Naming_AST.resolve lang ast;

  (* less: use treesitter also later
   *  Parse_code.parse_and_resolve_name_use_pfff_or_treesitter lang file in
   *)
  let facts = ref [] in

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def -> Common.push (facts_of_function lang def) facts);
      }
  in
  v (G.Pr ast);

  let facts = !facts |> List.rev |> List.flatten in
  pr2 (spf "generating %d facts in %s" (List.length facts) outdir);
  Datalog_io.write_facts_for_doop facts outdir

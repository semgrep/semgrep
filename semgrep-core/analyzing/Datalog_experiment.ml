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
(*open AST_generic*)

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
(* Entry point *)
(*****************************************************************************)
let gen_facts _file _outdir =
  raise Todo

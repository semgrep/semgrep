open Common

(*module V = Visitor_c*)

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_dump_cil file =
  let _ast = Parse_c.parse_program file in

  let env =
    {
      Datalog_c.locals (* only thing that actually matters *) = ref [];
      globals = Graph_code.create ();
      globals_renames = (fun x -> x);
      (* not actually used by instrs_of_expr *)
      scope = "";
      c_file_readable = "";
      long_format = false;
      facts = ref [];
    }
  in
  ignore env;
  raise Todo
(*
  (* todo: actually need to build a correct set of locals!
   * also when do int v = ...; we just see the ... in kexpr,
   * and so there will be a _v_xx = ... but not the  = _v_xx;
   *)
  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun _k e ->
      let instrs = Datalog_c.instrs_of_expr env e in
      instrs |> List.iter (fun instr ->
        let s = Ast_cil.show_instr instr in
        pr s
      )
    );
  }
  in
  visitor (Ast_c.Program ast);
  ()
*)

let test_dataflow_c file =
  let file = Common.fullpath file in
  let root = Sys.getcwd () |> Common.fullpath in
  Graph_code_c.facts := Some (ref []);
  Datalog_c.long_format := false;
  let _g = Graph_code_c.build ~verbose:false root [ file ] in
  let facts = List.rev !(Common2.some !Graph_code_c.facts) in
  Common2.pr2_xxxxxxxxxxxxxxxxx ();
  (* debug *)
  facts |> List.iter (fun fact -> pr2 (Datalog_code.string_of_fact fact));
  Common2.pr2_xxxxxxxxxxxxxxxxx ();

  let facts_file = "/tmp/facts.dl" in
  Common.with_open_outfile facts_file (fun (pr_no_nl, _chan) ->
      let pr s = pr_no_nl (s ^ ".\n") in
      facts |> List.iter (fun fact -> pr (Datalog_code.string_of_fact fact)));

  let logic_file =
    Filename.concat Config_pfff.path_pfff_home "h_program-lang/datalog_code.dl"
  in

  let final_file = "/tmp/datalog.dl" in
  let cmd = spf "cat %s %s > %s" facts_file logic_file final_file in
  Common.command2 cmd;
  (* using http://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html *)
  let cmd = spf "datalog %s | sort" final_file in
  Common.command2 cmd;
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-dump_cil", " <file>", Common.mk_action_1_arg test_dump_cil);
    ("-dataflow_c", "   <file>", Common.mk_action_1_arg test_dataflow_c);
  ]

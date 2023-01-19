(*
   Estimate the fraction of untranslated nodes in the generic AST.
*)

module G = AST_generic

type t = Parsing_stat.ast_stat

let stat (ast : G.program) : t =
  let total_node_count = ref 0 in
  let untranslated_node_count = ref 0 in
  let count_ordinary_node (k, _) node =
    incr total_node_count;
    k node
  in
  let visit_nodes =
    Visitor_AST.mk_visitor
      {
        kexpr = count_ordinary_node;
        kstmt = count_ordinary_node;
        ktype_ = count_ordinary_node;
        kpattern = count_ordinary_node;
        kfield = count_ordinary_node;
        kfields = count_ordinary_node;
        kpartial = count_ordinary_node;
        kdef = count_ordinary_node;
        kdir = count_ordinary_node;
        kattr = count_ordinary_node;
        kparam = count_ordinary_node;
        ktparam = count_ordinary_node;
        kcatch = count_ordinary_node;
        kident = count_ordinary_node;
        kname = count_ordinary_node;
        kentity = count_ordinary_node;
        kstmts = count_ordinary_node;
        kfunction_definition = count_ordinary_node;
        kclass_definition = count_ordinary_node;
        kinfo = count_ordinary_node;
        (* By default, do not visit the refs in id_info *)
        kid_info = Visitor_AST.default_visitor.kid_info;
        ksvalue = count_ordinary_node;
        kargument = count_ordinary_node;
        klit = count_ordinary_node;
        ktodo =
          (fun (k, _) x ->
            incr total_node_count;
            incr untranslated_node_count;
            k x);
        kraw =
          (fun (k, _) raw_node ->
            incr total_node_count;
            incr untranslated_node_count;
            k raw_node);
      }
  in
  visit_nodes (G.Pr ast);
  {
    total_node_count = !total_node_count;
    untranslated_node_count = !untranslated_node_count;
  }

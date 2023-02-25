(*
   Estimate the fraction of untranslated nodes in the generic AST.
*)

module G = AST_generic

type t = Parsing_stat.ast_stat

let stat (ast : G.program) : t =
  let total_node_count = ref 0 in
  let untranslated_node_count = ref 0 in
  let count_ordinary_node recurse env node =
    incr total_node_count;
    recurse env node
  in
  let visitor =
    object
      inherit [_] G.iter_no_id_info as super
      method! visit_expr = count_ordinary_node super#visit_expr
      method! visit_stmt = count_ordinary_node super#visit_stmt
      method! visit_type_ = count_ordinary_node super#visit_type_
      method! visit_pattern = count_ordinary_node super#visit_pattern
      method! visit_field = count_ordinary_node super#visit_field
      method! visit_definition = count_ordinary_node super#visit_definition
      method! visit_directive = count_ordinary_node super#visit_directive
      method! visit_attribute = count_ordinary_node super#visit_attribute
      method! visit_parameter = count_ordinary_node super#visit_parameter

      method! visit_type_parameter =
        count_ordinary_node super#visit_type_parameter

      method! visit_catch = count_ordinary_node super#visit_catch
      method! visit_ident = count_ordinary_node super#visit_ident
      method! visit_name = count_ordinary_node super#visit_name
      method! visit_entity = count_ordinary_node super#visit_entity

      method! visit_function_definition =
        count_ordinary_node super#visit_function_definition

      method! visit_class_definition =
        count_ordinary_node super#visit_class_definition

      method! visit_tok = count_ordinary_node super#visit_tok
      method! visit_svalue = count_ordinary_node super#visit_svalue
      method! visit_argument = count_ordinary_node super#visit_argument
      method! visit_literal = count_ordinary_node super#visit_literal

      method! visit_todo_kind env x =
        incr total_node_count;
        incr untranslated_node_count;
        super#visit_todo_kind env x

      method! visit_raw_tree env raw_node =
        incr total_node_count;
        incr untranslated_node_count;
        super#visit_raw_tree env raw_node
    end
  in
  visitor#visit_program () ast;
  {
    total_node_count = !total_node_count;
    untranslated_node_count = !untranslated_node_count;
  }

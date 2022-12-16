
type visitor_in = {
  khtml_tree: Ast_html.html_tree vin;
  kinfo: Ast_html.info vin;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

and visitor_out = Ast_html.any -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out

val do_visit_with_ref:
  ('a list ref -> visitor_in) -> Ast_html.any -> 'a list

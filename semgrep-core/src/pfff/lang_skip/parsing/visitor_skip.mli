
type visitor_in = {
  kinfo: Ast_skip.tok vin;
}
and visitor_out = Ast_skip.any -> unit
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out

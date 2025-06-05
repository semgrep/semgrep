let foo a b = a + b [@@trace "foo 1"]

let foo a b =
  Trace_core.with_span ~__FILE__ ~__LINE__ "get_rules" @@ fun _sp -> a + b

let foo = function
  | (lazy trace) -> 1
  | _ -> 2


let test2 () =

  let (lazy { Pattern_match.source; tokens; sink }) = taint_trace in
  2

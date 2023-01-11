let x =
  match String.unsafe_get format j with
  '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)

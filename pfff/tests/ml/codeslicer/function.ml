let size_component = function
    Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float

module MainModule =
struct
  (* ERROR: match *)
  let foo = try 1 with _ -> 2
end

module MainFunctor (Arg: S) =
struct
  (* ERROR: match *)
  let foo = try 1 with _ -> 2
end


module A = struct
  let bar () = ()
end

let f () =
  let open A in
  bar()

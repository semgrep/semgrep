(*****************************************************************************)
(* API *)
(*****************************************************************************)

let partition f l =
  let rec aux left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match f x with
        | Ok x -> aux (x :: left) right l
        | Error x -> aux left (x :: right) l)
  in
  aux [] [] l

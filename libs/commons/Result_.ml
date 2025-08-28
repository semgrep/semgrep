(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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

let collect l =
  let rec f acc l =
    match l with
    | [] -> acc
    | r :: rs -> (
        match r with
        | Ok x ->
            let acc = Result.map (fun xs' -> x :: xs') acc in
            f acc rs
        | Error e -> Result.error e)
  in
  f (Ok []) l |> Result.map List.rev

module Operators = struct
  let ( >>= ) x f = Result.bind x f
end

let transpose_result_option (x : ('a, 'err) result option) :
    ('a option, 'err) result =
  match x with
  | None -> Ok None
  | Some (Ok a) -> Ok (Some a)
  | Some (Error e) -> Error e

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helper functions for pretty printing (ASCII-art) *)

let line width =
  String.init (3 * width) (fun i ->
      char_of_int
        (match i mod 3 with
        | 0 -> 0xE2
        | 1 -> 0x94
        | 2 -> 0x80
        | _not_possible -> assert false))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_table (h1, heading) ppf entries =
  let int_size i =
    let rec dec acc = function
      | 0 -> acc
      | n -> dec (succ acc) (n / 10)
    in
    if i = 0 then 1 else dec 0 i
  in
  let len1, lengths =
    let acc = Common.map String.length heading in
    List.fold_left
      (fun (n1, needed) (c1, curr) ->
        ( max (String.length c1) n1,
          Common.map2 max needed (Common.map int_size curr) ))
      (String.length h1, acc)
      entries
  in
  let lengths = Common.map (fun i -> i + 3) lengths in
  let line = List.fold_left (fun acc w -> acc + w) (len1 + 2) lengths |> line in
  let pad str_size len =
    let to_pad = len - str_size in
    String.make to_pad ' '
  in
  Fmt.pf ppf "  %s%s" h1 (pad (String.length h1) len1);
  List.iter2
    (fun h l -> Fmt.pf ppf "%s%s" (pad (String.length h) l) h)
    heading lengths;
  Fmt.pf ppf "@. %s@." line;
  List.iter
    (fun (e1, entries) ->
      Fmt.pf ppf "  %s%s" e1 (pad (String.length e1) len1);
      List.iter2
        (fun e l -> Fmt.pf ppf "%s%u" (pad (int_size e) l) e)
        entries lengths;
      Fmt.pf ppf "@.")
    entries;
  Fmt.pf ppf "@."

let pp_heading ppf txt =
  let line = line (String.length txt + 2) in
  Fmt.pf ppf "@.@.┌%s┐@." line;
  Fmt.pf ppf "│ %s │@." txt;
  Fmt.pf ppf "└%s┘@." line

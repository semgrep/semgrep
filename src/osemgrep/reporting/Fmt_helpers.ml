(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helper functions for pretty printing (ASCII-art) *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_table (h1, heading) ppf entries =
  let int_size i =
    let rec dec acc = function
      | 0 -> acc
      | n -> dec (succ acc) (n / 10)
    in
    dec 1 i
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
  let llen = List.fold_left (fun acc w -> acc + w + 1) len1 lengths in
  let line = String.make llen '-' in
  let pad str_size len =
    let to_pad = succ (len - str_size) in
    String.make to_pad ' '
  in
  Fmt.pf ppf "%s%s" h1 (pad (String.length h1) len1);
  List.iter2
    (fun h l -> Fmt.pf ppf "%s%s" (pad (String.length h) l) h)
    heading lengths;
  Fmt.pf ppf "@.%s@." line;
  List.iter
    (fun (e1, entries) ->
      Fmt.pf ppf "%s%s"
        (String.capitalize_ascii e1)
        (pad (String.length e1) len1);
      List.iter2
        (fun e l -> Fmt.pf ppf "%s%u" (pad (int_size e) l) e)
        entries lengths;
      Fmt.pf ppf "@.")
    entries;
  Fmt.pf ppf "@."

let pp_heading ppf txt =
  let chars = String.length txt + 4 in
  let line = String.make chars '-' in
  Fmt.pf ppf "%s@." line;
  Fmt.pf ppf "| %s |@." txt;
  Fmt.pf ppf "%s@." line

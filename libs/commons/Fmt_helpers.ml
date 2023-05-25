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

let layout_table (h1, heading) entries =
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
  String.concat ""
    (List.flatten
       ([ h1; pad (String.length h1) len1 ]
       :: Common.map2
            (fun h l -> [ pad (String.length h) l; h ])
            heading lengths))
  :: line
  :: Common.map
       (fun (e1, entries) ->
         String.concat ""
           (List.flatten
              ([ e1; pad (String.length e1) len1 ]
              :: Common.map2
                   (fun e l -> [ pad (int_size e) l; string_of_int e ])
                   entries lengths)))
       entries

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_table (h1, heading) ppf entries =
  let lines = layout_table (h1, heading) entries in
  List.iteri
    (fun idx line -> Fmt.pf ppf "%s%s@." (if idx = 1 then " " else "  ") line)
    lines;
  Fmt.pf ppf "@."

let pp_tables ppf (h1, heading1, entries1) (h2, heading2, entries2) =
  let lines1 = layout_table (h1, heading1) entries1
  and lines2 = layout_table (h2, heading2) entries2 in
  let l1_space = String.make (String.length (List.hd lines1)) ' ' in
  let space = String.make 10 ' ' in
  let rec one idx a b =
    match (a, b) with
    | [], [] -> ()
    | a :: ra, b :: rb ->
        Fmt.pf ppf "%s%s%s%s@."
          (if idx = 1 then " " else "  ")
          a
          (if idx = 1 then String.make 8 ' ' else space)
          b;
        one (idx + 1) ra rb
    | a :: ra, [] ->
        Fmt.pf ppf "  %s@." a;
        one (idx + 1) ra []
    | [], b :: rb ->
        Fmt.pf ppf "  %s%s%s@." l1_space space b;
        one (idx + 1) [] rb
  in
  one 0 lines1 lines2

let pp_heading ppf txt =
  let line = line (String.length txt + 2) in
  Fmt.pf ppf "@.@.┌%s┐@." line;
  Fmt.pf ppf "│ %s │@." txt;
  Fmt.pf ppf "└%s┘@." line

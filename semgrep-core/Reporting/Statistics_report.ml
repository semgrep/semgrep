open Common

type run = {
  lang: string;
  rule: string;
  files: (filename * float) list;
  timeout: filename list;
}
[@@deriving show]

let map_with_previous f base xs =
  let rec aux prev xs =
    match xs with
    | [] -> []
    | x::xs -> let res = f prev x in res::aux x xs
  in
  aux base xs

let parse_run (rule, xs) =
  let lang =
    match xs |> Common.find_some_opt (fun s ->
      if s =~ ".*Executed as.*-lang \\([^ ]+\\) .*"
      then Some (Common.matched1 s)
      else None
    ) with
    | Some s -> s
    | None -> "no_language_found"
  in
  let files =
    xs |> Common.map_filter (fun s ->
      if s =~ "\\[\\([0-9]+\\.[0-9]+\\) .* done with \\(.*\\)"
      then Some (Common.matched2 s)
      else None
    )
    |> List.map (fun (a, b) -> b, float_of_string a)
    |> map_with_previous (fun (_, prevtime) (f, time) ->
      f, time -. prevtime
    ) ("nofile", 0.)
  in
  let timeout =
    xs |> Common.map_filter (fun s ->
      if s =~ ".*raised Timeout in .* for \\(.*\\)"
      then Some (Common.matched1 s)
      else None
    )
  in
  { lang; rule; files; timeout}


let stat file =
  let xs = Common.cat file in
  let ys =
    xs |> Common2.split_list_regexp "^Running rule" in
  let runs = ys |> List.map parse_run in
  runs |> List.iter (fun r -> pr2 (show_run r));

  let problematic_files =
    runs |> List.map (fun x -> x.files) |> List.flatten
    |> Common.sort_by_val_highfirst
    |> Common.take_safe 30
  in
  pr2 ("problematic files");
  problematic_files |> List.iter pr2_gen;

  let problematic_rules =
    runs |> List.map (fun x ->
      (x.rule, List.length x.files, x.lang),
      x.files |> List.map snd |> Common2.sum_float
    ) |> Common.sort_by_val_highfirst
    |> Common.take_safe 30
  in
  pr2 ("problematic rules");
  problematic_rules |> List.iter pr2_gen;
  ()

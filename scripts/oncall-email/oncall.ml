open Common

module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type card = {
  title: string;
  url: string;
  assignees: string list;
  labels: string list;
}
[@@deriving show]

type column = {
  colname: string;
  cards: card list;
}
[@@deriving show]

type board = {
  columns: column list;
}
[@@deriving show]

(*****************************************************************************)
(* Parsing JSON Combinators *)
(*****************************************************************************)

let error j s =
  pr2 (J.show j);
  failwith s

(* combinators *)
let (>>) o fld =
  match o with
  | J.Object xs ->
      let o = List.assoc fld xs in
      o
  | _ -> failwith (spf "could not find %s" fld)

let jstring j =
  match j with
  | J.String s -> s
  | _ -> error j "not a string"

let array f j =
  match j with
  | J.Array xs -> xs |> List.map f
  | _ -> error j "not an array"

let filter_some xs =
  xs |> Common.map_filter (fun x -> x)

(*****************************************************************************)
(* Parsing columns and card *)
(*****************************************************************************)

(* parsing *)
let parse_card j =
  match j with
  | J.Object [
    "content", j;
    "isArchived", J.Bool isArchived;
    "note", _note;
    "creator", _creator;
  ] ->
      if isArchived then None
      else
        (match j with
         | J.Object [
           "__typename", J.String "Issue";
           "title", J.String title;
           "url", J.String url;
           "assignees", J.Object ["nodes", assignees];
           "closed", J.Bool _closed;
           "createdAt", J.String _createdAt;
           "updatedAt", J.String _updatedAt;
           "labels", J.Object ["nodes", labels];
         ] ->
             let assignees = assignees |> array (fun j -> j >> "login" |> jstring) in
             let labels = labels |> array (fun j -> j >> "name" |> jstring) in
             Some { title; url; assignees; labels }
         | J.Null -> None
         | J.Object ["__typename", J.String "PullRequest";] -> None
         | _ -> error j "not a card"
        )
  | _ -> error j "not a card"

let parse_column j =
  match j with
  | J.Object [
    "name", J.String colname;
    "cards", cards;
  ] ->
      { colname; cards =
                   let cards = cards >> "nodes" in
                   array parse_card cards |> filter_some
      }
  | _ -> error j "not a column"

let parse_board_state file =
  let j = J.load_json file in
  let cols =
    j >> "data" >> "organization" >> "project" >> "columns" >> "nodes" in
  { columns = array parse_column cols }

(*****************************************************************************)
(* Report *)
(*****************************************************************************)

let get_cards_of_column colname board =
  board.columns |> List.find (fun col -> col.colname = colname)
  |> (fun x -> x.cards)

let report_card card =
  let extra =
    if List.mem "priority:high" card.labels
    then " !PRIORITY:HIGH!"
    else ""
  in
  pr (spf "\t- %s%s\n\t<%s> (%s)" card.title extra card.url
        (card.assignees |> String.concat " "))



let report ~base ~today =
  pr "\nTo Discuss (UNASSIGNED)\n";
  today |> get_cards_of_column "To discuss" |> List.iter (fun card ->
    if card.assignees = []
    then report_card card
  );

  pr "\nUnassigned To do\n";
  today |> get_cards_of_column "Unassigned To do" |> List.iter (fun card ->
    report_card card
  );

  (* TODO: display if no update since last time *)
  pr "\nAssigned To do\n";
  today |> get_cards_of_column "Assigned To Do" |> List.iter (fun card ->
    report_card card
  );

  (* TODO: In Progress column, double check actual progress ... *)

  (* TODO: look at closed by in the issue when no assignee *)
  pr "\nClosed since last email (good job guys!)\n";
  let base_done = base |> get_cards_of_column "Done" in
  today |> get_cards_of_column "Done" |> List.iter (fun card ->
    let id = card.url in

    match base_done |> List.find_opt (fun card -> card.url = id) with
    | None -> report_card card
    | Some _ -> ()
  );
  ()

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
(* flags *)
let base = ref "yesterday.json"
let today = ref "today.json"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let oncall () =
  let base =  parse_board_state !base in
  let today = parse_board_state !today in
  report ~base ~today


let options = [
  "-base", Arg.Set_string base,
  " ";
]


let main () =
  Arg.parse options (fun file -> today := file) "usage: ";
  oncall()

let _ =
  main ()

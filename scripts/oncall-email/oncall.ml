open Common
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Compare the state of today's customer board with yesterday to help
 * generating the OnCall standup email.
 *
 * Usage:
 *  $ ./_build/default/oncall.exe -base yesterday.json today.json > email
 *
 * See README.md for more information.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type date = string [@@deriving show]

type issue = {
  title : string;
  (* plays the role of an id too *)
  url : string;
  assignees : string list;
  labels : string list;
  closed : bool;
  createdAt : date;
  updatedAt : date;
}
[@@deriving show]

(* in theory can have note, and can also be a PR *)
type card = issue [@@deriving show]

type column = { colname : string; cards : card list } [@@deriving show]

(* the customer board! *)
type board = { columns : column list } [@@deriving show]

(* the semgrep repository *)
type repo = { repo_name : string; issues : issue list }

(*****************************************************************************)
(* Parsing JSON Combinators *)
(*****************************************************************************)

let error j s =
  pr2 (J.show j);
  failwith s

(* combinators *)
let ( >> ) o fld =
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

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let filter_some xs = xs |> Common.map_filter (fun x -> x)

let find_issue_opt x xs = xs |> List.find_opt (fun y -> y.url = x.url)

let has_issue x xs =
  match find_issue_opt x xs with
  | None -> false
  | Some _ -> true

(*****************************************************************************)
(* Parsing columns, cards, issues *)
(*****************************************************************************)

let parse_issue j =
  match j with
  | J.Object
      [
        ("title", J.String title);
        ("url", J.String url);
        ("assignees", J.Object [ ("nodes", assignees) ]);
        ("closed", J.Bool closed);
        ("createdAt", J.String createdAt);
        ("updatedAt", J.String updatedAt);
        ("labels", J.Object [ ("nodes", labels) ]);
      ] ->
      let assignees = assignees |> array (fun j -> j >> "login" |> jstring) in
      let labels = labels |> array (fun j -> j >> "name" |> jstring) in
      { title; url; assignees; labels; closed; createdAt; updatedAt }
  | _ -> error j "not an issue"

let parse_card j =
  match j with
  | J.Object
      [
        ("content", j);
        ("isArchived", J.Bool isArchived);
        ("note", _note);
        ("creator", _creator);
      ] -> (
      if isArchived then None
      else
        match j with
        | J.Object (("__typename", J.String "Issue") :: rest) ->
            Some (parse_issue (J.Object rest))
        | J.Null -> None
        | J.Object [ ("__typename", J.String "PullRequest") ] -> None
        | _ -> error j "not a card" )
  | _ -> error j "not a card"

let parse_column j =
  match j with
  | J.Object [ ("name", J.String colname); ("cards", cards) ] ->
      {
        colname;
        cards =
          (let cards = cards >> "nodes" in
           array parse_card cards |> filter_some);
      }
  | _ -> error j "not a column"

let parse_repository j =
  match j with
  | J.Object
      [
        ("name", J.String repo_name); ("issues", J.Object [ ("nodes", issues) ]);
      ] ->
      let issues = array parse_issue issues in
      { repo_name; issues }
  | _ -> error j "not a repository"

let parse_board_state_and_issues file =
  let j = J.load_json file in
  let cols =
    j >> "data" >> "organization" >> "project" >> "columns" >> "nodes"
  in
  let repo = j >> "data" >> "repository" in
  ({ columns = array parse_column cols }, parse_repository repo)

(*****************************************************************************)
(* Report *)
(*****************************************************************************)

let get_cards_of_column colname board =
  match board.columns |> List.find_opt (fun col -> col.colname = colname) with
  | Some x -> x.cards
  | None -> failwith (spf "could not find column %s in the board" colname)

let report_issue ?(extra = "") x =
  let extra2 =
    if List.mem "priority:high" x.labels then "!PRIORITY:HIGH!" else ""
  in
  pr
    (spf "\t- %s %s%s\n\t<%s> (%s)" x.title extra extra2 x.url
       (x.assignees |> String.concat " "))

let report_card ?extra x = report_issue ?extra x

let report ~base ~today =
  let base_board, base_repo = base in
  let today_board, today_repo = today in

  pr "\nTo Discuss (UNASSIGNED)\n";
  today_board
  |> get_cards_of_column "To discuss"
  |> List.iter (fun card -> if card.assignees = [] then report_card card);

  pr "\nUnassigned To do\n";
  today_board
  |> get_cards_of_column "Unassigned To do"
  |> List.iter (fun card -> report_card card);

  pr "\nAssigned To do\n";
  let base_cards = base_board |> get_cards_of_column "Assigned To Do" in
  today_board
  |> get_cards_of_column "Assigned To Do"
  |> List.iter (fun card ->
         match find_issue_opt card base_cards with
         | None -> report_card card
         | Some old ->
             let extra =
               if card.updatedAt = old.updatedAt then "!NO_UPDATE!" else ""
             in
             report_card ~extra card);

  (* less: In Progress column, double check if actual progress ... *)

  (* TODO: look at closed by in the issue when no assignee or if last
   * PR related to the issue is someone else.
   *)
  pr "\nClosed since last email (good job guys!)\n";
  let base_done = base_board |> get_cards_of_column "Done" in
  today_board |> get_cards_of_column "Done"
  |> List.iter (fun card ->
         if not (base_done |> has_issue card) then report_card card);

  pr "\nNew semgrep issues NOT in the customer board\n";
  let all_issues_today_boards =
    today_board.columns |> List.map (fun x -> x.cards) |> List.flatten
  in
  let base_issues = base_repo.issues in
  today_repo.issues
  |> List.iter (fun issue ->
         if
           (not (all_issues_today_boards |> has_issue issue))
           && not (base_issues |> has_issue issue)
         then report_issue issue);
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
  let base = parse_board_state_and_issues !base in
  let today = parse_board_state_and_issues !today in
  report ~base ~today

let options = [ ("-base", Arg.Set_string base, " ") ]

let main () =
  Arg.parse options (fun file -> today := file) "usage: ";
  oncall ()

let _ = main ()

(* Austin Theriault, Cooper Pierce
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
module Request = Cohttp.Request
module Response = Cohttp.Response
module Body = Cohttp_lwt.Body
module Header = Cohttp.Header

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type test_response = { response : Response.t; body : Body.t }
type make_response_fn = Request.t -> Body.t -> test_response Lwt.t

module type S = sig
  val make_response : make_response_fn
end

module Make (M : S) : Cohttp_lwt.S.Client = struct
  open M

  type ctx = unit
  type 'a with_context = ?ctx:ctx -> 'a
  type 'a io = 'a Lwt.t
  type body = Cohttp_lwt.Body.t

  let map_context (x : 'a with_context) (f : 'a -> 'b) ?ctx = x ?ctx |> f

  let mock_response_of_request (req : Cohttp.Request.t) (body : Body.t) =
    Logs.debug (fun m ->
        m "[Testing client] Request: %s"
          (Request.sexp_of_t req |> Sexplib.Sexp.to_string_hum));
    let%lwt _body = Body.to_string body in
    Logs.debug (fun m -> m "[Testing client] Body: %s" _body);
    let%lwt response = make_response req body in
    Lwt.return (response.response, response.body)

  let call ?ctx ?headers ?(body = `Empty) ?chunked meth uri =
    ignore ctx;
    Logs.debug (fun m ->
        m "[Testing client] Request URI: %s" (Uri.to_string uri));
    let headers =
      match headers with
      | None -> Header.init ()
      | Some h -> h
    in
    let chunked =
      match chunked with
      | Some c -> c
      | None -> false
    in
    let req = Request.make_for_client ~headers ~chunked meth uri in
    mock_response_of_request req body

  let head ?ctx ?headers _ =
    ignore ctx;
    ignore headers;
    failwith
      "head is not implemented in the HTTP mock client. If you need this \
       functionality, please implement it."

  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri

  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `DELETE uri

  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `POST uri

  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PUT uri

  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PATCH uri

  let set_cache _ =
    failwith
      "http set_cache is not implemented in the HTTP mock client. If you need \
       this functionality, please implement it."

  let post_form ?ctx ?headers ~params _ =
    ignore ctx;
    ignore headers;
    ignore params;
    failwith
      "post_form is not implemented in the HTTP mock client. If you need this \
       functionality, please implement it."

  let callv ?ctx uri (reqs : (Cohttp.Request.t * Body.t) Lwt_stream.t) =
    ignore ctx;
    Logs.debug (fun m ->
        m "[Testing client] Request URI: %s" (Uri.to_string uri));
    let response_stream =
      Lwt_stream.map_s
        (fun (req, body) -> mock_response_of_request req body)
        reqs
    in
    Lwt.return response_stream
end

(*****************************************************************************)
(* Helper Functions *)
(*****************************************************************************)

let basic_response ?(status = 200) ?(headers = Header.init ()) body =
  let status = Cohttp.Code.status_of_code status in
  let response = Response.make ~status ~headers () in
  { response; body }

let body_of_file ?(trim = false) (path : Fpath.t) =
  let content = UFile.read_file path in
  let content = if trim then String.trim content else content in
  Cohttp_lwt.Body.of_string content

let check_body expected_body actual_body =
  let%lwt actual_body_content = Cohttp_lwt.Body.to_string actual_body in
  let%lwt expected_body_content = Cohttp_lwt.Body.to_string expected_body in
  (* Passing "" for the name of the check prevents an otherwise
     unconditional print to stderr of the string "Assert <name>". *)
  Alcotest.(check string) "" expected_body_content actual_body_content;
  Lwt.return_unit

let check_method expected_meth actual_meth =
  (* Passing "" for the name of the check prevents an otherwise
     unconditional print to stderr of the string "Assert <name>". *)
  Alcotest.(check string)
    ""
    (Cohttp.Code.string_of_method expected_meth)
    (Cohttp.Code.string_of_method actual_meth)

let check_header req header header_val =
  let actual_header = Cohttp.Header.get (Cohttp.Request.headers req) header in
  match actual_header with
  | None ->
      Alcotest.fail
        (Printf.sprintf "header %s not found. Headers: %s" header
           (Cohttp.Header.to_string (Cohttp.Request.headers req)))
  (* Passing "" for the name of the check prevents an otherwise
     unconditional print to stderr of the string "Assert <name>". *)
  | Some actual_header -> Alcotest.(check string) "" header_val actual_header

let check_headers actual_headers expected_headers =
  let lowercase_and_sort xs =
    xs
    |> List_.map (fun (x, y) -> (String.lowercase_ascii x, y))
    |> List.sort (fun (x, _) (y, _) -> String.compare x y)
  in
  let actual_headers = actual_headers |> Header.to_list |> lowercase_and_sort in
  let expected_headers =
    expected_headers |> Header.to_list |> lowercase_and_sort
  in
  let pp = Alcotest.(pp (list (pair string string))) in

  let rec check = function
    | [], [] -> ()
    | (_ :: _ as expected_headers), [] ->
        Alcotest.failf "Missing some expected headers:\n%a" (Fmt.styled `Red pp)
          expected_headers
    | [], (_ :: _ as actual_headers) ->
        Alcotest.failf "Found some unexpected headers:\n%a" (Fmt.styled `Red pp)
          actual_headers
    | ( (expected_header, expected_value) :: expected_headers,
        (actual_header, actual_value) :: actual_headers ) ->
        if String.equal expected_header actual_header then
          let value_regex =
            if
              String.starts_with ~prefix:"regex{" expected_value
              && String.ends_with ~suffix:"}" expected_value
            then
              Pcre2_.regexp
                (String.sub expected_value 6 (String.length expected_value - 7))
            else Pcre2_.regexp (Pcre2_.quote expected_value)
          in
          match
            Pcre2_.pmatch
              ~flags:[ `ANCHORED; `ENDANCHORED ]
              ~rex:value_regex actual_value
          with
          | Ok true -> check (expected_headers, actual_headers)
          | Ok false ->
              Alcotest.failf
                "Header values for '%a' do not match\n\
                 Expected:\n\
                 %a\n\
                 Received:\n\
                 \"%a\""
                (Fmt.styled `Bold Fmt.string)
                expected_header
                (Fmt.styled `Green Pcre2_.pp)
                value_regex
                (Fmt.styled `Red Fmt.string)
                actual_value
          | Error err -> Alcotest.failf "Invalid regex: %a" Pcre2_.pp_error err
        else
          Alcotest.failf
            "Expected header '%a', but it was either missing, or header counts \
             did not match up"
            (Fmt.styled `Bold Fmt.string)
            expected_header
  in
  check (expected_headers, actual_headers)
(* (* Passing "" for the name of the check prevents an otherwise
      unconditional print to stderr of the string "Assert <name>". *)
   Alcotest.(check (list (pair string string)))
     "" expected_headers actual_headers *)

let get_header req header =
  Cohttp.Header.get (Cohttp.Request.headers req) header

(*****************************************************************************)
(* Entrypoint *)
(*****************************************************************************)

let with_testing_client make_fn test_fn () =
  let new_client : (module Cohttp_lwt.S.Client) =
    (module Make (struct
      let make_response = make_fn
    end))
  in
  Http_helpers.with_client_ref new_client
    (fun () -> Common.save_excursion Http_helpers.in_mock_context true test_fn)
    ()

(*****************************************************************************)
(* Saved Request/Reponse Mocking *)
(*****************************************************************************)

let strip_and_parse prefix p s =
  let rec go s acc =
    if String.starts_with ~prefix s then
      let s = Str.string_after s (String.length prefix) in
      let i = String.index s '\n' in
      let before, after = (Str.first_chars s i, Str.string_after s (i + 1)) in
      go after (before :: acc)
    else (acc, s)
  in
  let acc, rem = go s [] in
  (acc |> List.rev |> p, rem)

let parse_first_line_req s =
  let tokens = String.split_on_char ' ' s in
  match tokens with
  | [ meth; resource; ver ] ->
      ( Cohttp.Code.method_of_string meth,
        resource,
        Cohttp.Code.version_of_string ver )
  | _ -> failwith "Invalid format"

let parse_first_line_resp s =
  let tokens = String.split_on_char ' ' s in
  match tokens with
  | version :: status_code :: status_text ->
      ( Cohttp.Code.version_of_string version,
        Cohttp.Code.status_of_code (int_of_string status_code),
        String.concat " " status_text )
  | _ -> failwith "Invalid format"

let trim_front s =
  let len = String.length s in
  let rec go s i =
    if len < i then len
    else if List.mem (String.get s i) [ '\t'; '\n'; '\r'; ' '; '\x0C' ] then
      go s (i + 1)
    else i
  in
  Str.string_after s (go s 0)

let parse_headers headers =
  headers
  |> List_.map (fun header ->
         let i = String.index header ':' in
         let before, after =
           ( Str.first_chars header i,
             Str.string_after header (i + 1) |> trim_front )
         in
         (before, after))
  |> Cohttp.Header.of_list

let take_while p xs =
  let rec go xs acc =
    match xs with
    | [] -> (List.rev acc, None, [])
    | x :: xs -> if p x then go xs (x :: acc) else (List.rev acc, Some x, xs)
  in
  go xs []

let parse_req =
  strip_and_parse "> " (fun lines ->
      match lines with
      | [] -> failwith "Invalid format"
      | first_line :: lines ->
          let meth, resource, version = parse_first_line_req first_line in
          let headers, _, body =
            take_while (fun s -> String.length s <> 0) lines
          in
          let headers = parse_headers headers in
          let body = String.concat "\n" body |> Body.of_string in
          let uri = Uri.of_string resource in
          (Cohttp.Request.make ~meth ~version ~headers uri, body))

let parse_resp =
  strip_and_parse "< " (fun lines ->
      match lines with
      | [] -> failwith "Invalid format"
      | first_line :: lines ->
          let version, status, _ = parse_first_line_resp first_line in
          let headers, _, body =
            take_while (fun s -> String.length s <> 0) lines
          in
          let headers = parse_headers headers in
          let body = String.concat "\n" body |> Body.of_string in
          (Cohttp.Response.make ~version ~status ~headers (), body))

let client_from_file (req_resp_file : Fpath.t) =
  let contents = UFile.read_file req_resp_file in
  let rec go s acc =
    if String.length s = 0 then acc
    else if not (List.mem (String.get s 0) [ '>'; '<' ]) then
      go (Str.string_after s (String.index s '\n' + 1)) acc
    else
      let req, s = parse_req s in
      let resp, s = parse_resp s in
      go s ((req, resp, ref false) :: acc)
  in
  let pairs = go contents [] in
  let new_client : (module Cohttp_lwt.S.Client) =
    (module Make (struct
      let make_response (req : Request.t) body =
        match
          List.find_opt
            (fun (((req' : Request.t), _), _, used) ->
              (not !used) && req.resource = req'.resource)
            pairs
        with
        | Some ((expected_req, request_body), (response, response_body), used)
          ->
            check_method expected_req.meth req.meth;
            check_headers req.headers expected_req.headers;
            Lwt.async (fun () -> check_body body request_body);
            used := true;
            Lwt.return { response; body = response_body }
        | None -> Alcotest.fail "Given request does not match any resources"
    end))
  in
  ( new_client,
    fun f ->
      f
      @@ List_.filter_map
           (fun (req, resp, used) -> if !used then None else Some (req, resp))
           pairs )

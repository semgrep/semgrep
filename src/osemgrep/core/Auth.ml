(* A Semgrep.dev token representing your identity.
 * This is stored in ~/.semgrep/settings.yml and initially fetched
 * from https://semgrep.dev during 'semgrep loging'
 *)
type token = Token of string

let string_of_token (Token str) = str

(* TODO: remove at some point and force to get first a semgrep capability *)

let unsafe_token_of_string str = Token str
let auth_header_of_token (Token str) = ("Authorization", "Bearer " ^ str)

type cap_token = < token : token >

(* ugly: can't factorize *)
let cap_token_and_network token caps =
  object
    method token = token
    method network = caps#network
  end

let cap_token_and_network_and_tmp token caps =
  object
    method token = token
    method network = caps#network
    method tmp = caps#tmp
  end

let cap_token_and_network_and_tmp_and_exec token caps =
  object
    method token = token
    method network = caps#network
    method tmp = caps#tmp
    method exec = caps#exec
  end

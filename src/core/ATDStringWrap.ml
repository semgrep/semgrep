(* This module is here to help using the 'string wrap <ocaml module=Xxx>'
 * feature of atdgen.
 * See https://atd.readthedocs.io/en/stable/atdgen-reference.html#using-a-custom-wrapper
 * for more information.
 *)
open Common

module Fpath = struct
  type t = Fpath.t [@@deriving show]

  let unwrap = Fpath.to_string
  let wrap = Fpath.v
end

module Uri = struct
  type t = Uri.t [@@deriving show]

  let unwrap = Uri.to_string
  let wrap = Uri.of_string
end

module Uuidm = struct
  type t = Uuidm.t [@@deriving show]

  let unwrap = Uuidm.to_string

  let wrap x =
    match Uuidm.of_string x with
    | Some x -> x
    | None -> failwith (spf "Uuidm parse error on %s" x)
end

module Sha1 = struct
  type t = Digestif.SHA1.t

  let unwrap = Digestif.SHA1.to_hex
  let wrap = Digestif.SHA1.of_hex
end

module Sha256 = struct
  type t = Digestif.SHA256.t

  let unwrap = Digestif.SHA256.to_hex
  let wrap = Digestif.SHA256.of_hex
end

module Datetime = struct
  type t = Unix.tm

  let unwrap (tm : Unix.tm) : string =
    Common.spf "%04d-%02d-%02dT%02d:%02d:%02d+00:00" (1900 + tm.tm_year)
      (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let wrap s : Unix.tm =
    (* TODO? should the end always be +00:00 given we generate GMT/UTC time?
     * Should we enforce it in the regexp?
     *)
    if
      s
      =~ "^\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)T\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)[-+][0-9]+:[0-9]+$"
    then (
      let year, mon, day, hour, min, sec = Common.matched6 s in
      (* mostly the inverse of unwrap above *)
      let tm =
        {
          Unix.tm_year = int_of_string year - 1900;
          tm_mon = int_of_string mon - 1;
          tm_mday = int_of_string day;
          tm_hour = int_of_string hour;
          tm_min = int_of_string min;
          tm_sec = int_of_string sec;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = false;
        }
      in
      (* normalize *)
      (* ugly, but this is suggested in Unix.mli to get the inverse
       * of the gmtime function.
       *)
      let before = Sys.getenv_opt "TZ" in
      Unix.putenv "TZ" "UTC";
      let _s, tm = Unix.mktime tm in
      (match before with
      | None ->
          (* argh, no unsetenv,
           * see https://discuss.ocaml.org/t/unset-environment-variable/9025/4
           *)
          Unix.putenv "TZ" ""
      | Some old -> Unix.putenv "TZ" old);
      tm)
    else failwith (spf "wrong datetime format: %s" s)

  let () =
    Testutil.test "Datetime" (fun () ->
        let now = Unix.gmtime (Unix.gettimeofday ()) in
        let s : string = unwrap now in
        let now' = wrap s in
        if not (now =*= now') then
          failwith
            (spf
               "the date %s which was unwrapped as %s is not the same when \
                wrapped back as %s"
               (Dumper.dump now) s (Dumper.dump now')))
end

module Ruleid = struct
  type t = Rule_ID.t [@@deriving show]

  let unwrap = Rule_ID.to_string
  let wrap = Rule_ID.of_string
end

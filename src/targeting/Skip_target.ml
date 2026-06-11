(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Skip targets.
*)
open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_t
module Log = Log_targeting.Log

(****************************************************************************)
(* Minified files detection (via whitespace stats) *)
(****************************************************************************)

(*
   The 7% threshold works well for javascript files based on looking at
   .min.js files and other .js found in various github repos.

   - very few minified files have more than 5% of whitespace. Those with more
     than 5% contain embedded data strings e.g.
     "1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 ...".
   - no legitimate source files have less than 8% of whitespace and are larger
     than 500 bytes at the same time.
   - only some really short source files (2-3 lines) were found to have
     between 5% and 8% whitespace.

  However, this threshold works less well for java files because it is not
  unusual to have large import blocks at the top, resulting in a very low
  whitespace fraction. This should not affect very many Java files, so for
  now it is ok that we leave it. The consequence of reducing the frequency
  is that Semgrep would likely take longer as it scans more costly minified
  files. TODO we might want to do some benchmarking and change this default
*)
(* coupling: this number is iuncluded in Scan_CLI.ml's docs *)
let min_whitespace_frequency = 0.07

(*
   This is for the few minified files that embed a bunch of space-separated
   items in large data strings.
   0.001 is an average of 1000 bytes per line, which doesn't occur with
   normal programming languages. It can approach this with Markdown or maybe
   files that are mostly comments made of very long lines.
*)
let min_line_frequency = 0.001

type whitespace_stat = {
  sample_size : int;
  (* size of the block; possibly the whole file *)
  ws_freq : float;
  (* whitespace: CR, LF, space, tab *)
  line_freq : float; (* frequency of lines = 1/(avg line length) *)
}

let whitespace_stat_of_string s =
  (* number of lines = number of LF characters + 1 *)
  let lines = ref 1 in
  let whitespace = ref 0 in
  let other = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | ' '
    | '\t'
    | '\r' ->
        incr whitespace
    | '\n' ->
        incr whitespace;
        incr lines
    | __else__ -> incr other
  done;
  let total = !whitespace + !other in
  let sample_size = String.length s in
  if total =|= 0 then { sample_size; ws_freq = 1.; line_freq = 1. }
  else
    let ws_freq = float !whitespace /. float total in
    let line_freq = float !lines /. float total in
    { sample_size; ws_freq; line_freq }

let whitespace_stat_of_block ?block_size path =
  let s = Guess_lang.get_first_block ?block_size path in
  whitespace_stat_of_string s

let is_minified (path : Fpath.t) =
  let stat = whitespace_stat_of_block ~block_size:4096 path in
  (*
   A small file could contain a long URL with no whitespace without being
   minified. That's why we require a minimum file size.
*)
  if stat.sample_size > 1000 then
    if stat.ws_freq < min_whitespace_frequency then
      Error
        {
          Out.path;
          reason = Minified;
          details =
            Some
              (spf "file contains too little whitespace: %.3f%% (min = %.1f%%)"
                 (100. *. stat.ws_freq)
                 (100. *. min_whitespace_frequency));
          rule_id = None;
        }
    else if stat.line_freq < min_line_frequency then
      Error
        {
          Out.path;
          reason = Minified;
          details =
            Some
              (spf
                 "file contains too few lines for its size: %.4f%% (min = \
                  %.2f%%)"
                 (100. *. stat.line_freq)
                 (100. *. min_line_frequency));
          rule_id = None;
        }
    else Ok path
  else Ok path

(****************************************************************************)
(* Big file filtering *)
(****************************************************************************)

(*
   Some source files are really huge (> 20 MB) and they cause
   some annoying 'out of memory' crash that sometimes even the use
   of mem_limit_mb above does not solve.

   We could configure the size limit based on a per-language basis if we
   know that a language parser can handle larger files.
*)
let is_big max_bytes path =
  let size = UFile.filesize path in
  match size with
  | Ok size when max_bytes > 0 && size > max_bytes ->
      Error
        {
          Out.path;
          reason = Too_big;
          details =
            Some
              (spf "target file size exceeds %i bytes at %i bytes" max_bytes
                 size);
          rule_id = None;
        }
  | Ok _ -> Ok path
  | Error (code, _func, info) ->
      Log.warn (fun m ->
          m "is_big: unexpected error when reading %s: %s (code %s)" !!path info
            (Unix.error_message code));
      Ok path

let exclude_big_files max_target_bytes paths =
  let max_bytes = max_target_bytes in
  paths |> Result_.partition (is_big max_bytes)

(*************************************************************************)
(* Access permission filtering *)
(*************************************************************************)
(*
   Filter out folders and files that don't have sufficient access permissions.

   For Git projects, we only filter on regular files since folders are not
   returned to us by 'git ls-files'. This is why semgrep won't report folders
   with insufficient permissions for Git projects.

   For other projects, we scan the file tree ourselves and need to check
   folder permissions (read+execute on Unix, read on Windows).
*)

let skip_inaccessible_dir_path fpath : Out.skipped_target =
  {
    Out.path = fpath;
    reason = Insufficient_permissions;
    details = Some "folder lacks sufficient access permissions";
    rule_id = None;
  }

let skip_inaccessible_file_path fpath : Out.skipped_target =
  {
    Out.path = fpath;
    reason = Insufficient_permissions;
    details = Some "file lacks sufficient access permissions";
    rule_id = None;
  }

let dir_has_access_permissions (dir : Fpath.t) =
  try
    Unix.access !!dir [ R_OK; X_OK ];
    true
  with
  | Unix.Unix_error _ -> false

let file_has_access_permissions (file : Fpath.t) =
  try
    Unix.access !!file [ R_OK ];
    true
  with
  | Unix.Unix_error _ -> false

let filter_dir_access_permissions (dir : Fpath.t) :
    (Fpath.t, Out.skipped_target) result =
  if dir_has_access_permissions dir then Ok dir
  else Error (skip_inaccessible_dir_path dir)

let filter_file_access_permissions (file : Fpath.t) :
    (Fpath.t, Out.skipped_target) result =
  if file_has_access_permissions file then Ok file
  else Error (skip_inaccessible_file_path file)

let exclude_inaccessible_files files =
  Result_.partition filter_file_access_permissions files

(*************************************************************************)
(* Binary file filtering *)
(*************************************************************************)
(*
   Filter out files that File_type classifies as non-source and whose first
   bytes match a known binary-format signature (PNG, PDF, ZIP, ELF, ...).
   We check the file extension first but do not trust it because for secrets
   we scan every file and there is always the chance that a .png file is not
   actually a png but a text file with a secret instead. We assume that if
   the binary-format signature matches the file extension then we can correctly
   classify the file as binary. If the extension is unknown, we check if the
    file matches any known binary signature.
*)

(* Maps a (lowercased) file extension to the byte-prefix signatures that a
   file of that type may begin with. A file matches if its first bytes start
   the prefix corresponding to its extension.

   coupling: extensions here should correspond to the Obj/Binary/Media/Doc/
   Archive cases of File_type.file_type_of_file.

   Caveats:
   - RIFF containers (wav, avi, webp) all begin with the ASCII bytes "RIFF"
     at offset 0. Since RIFF is exclusively a binary container format, the
     leading "RIFF" alone is a sufficient signal; we don't inspect the format
     tag (WAVE/AVI /WEBP) at offset 8.
   - A couple of file types may not have extensions at all, e.g. Unix executables.
     These are handled in the fallback sniff for extension-less binaries.
   - Some File_type "binary" extensions have no reliable leading magic and are
     intentionally omitted: svg (XML text), ppm/pgm/pbm (ASCII "P3".."P6"),
     tga (footer-based), xpm (C source), font, byte, top, and most Obj
     extensions (cmi, cmo, cmx, o, a, log, out, dat, ...). With no matching
     signature these are treated as non-binary and scanned. *)
let magic_by_ext : (string * string list) list =
  [
    (* Images *)
    ("png", [ "\x89PNG\r\n\x1a\n" ]);
    ("jpg", [ "\xff\xd8\xff" ]);
    ("jpeg", [ "\xff\xd8\xff" ]);
    ("gif", [ "GIF87a"; "GIF89a" ]);
    ("tif", [ "II*\x00"; "MM\x00*" ]);
    ("tiff", [ "II*\x00"; "MM\x00*" ]);
    ("ico", [ "\x00\x00\x01\x00" ]);
    ("icns", [ "icns" ]);
    ("psd", [ "8BPS" ]);
    ("xcf", [ "gimp xcf " ]);
    ("webp", [ "RIFF" ]);
    ("ai", [ "%PDF-" ]);
    (* modern Illustrator files are PDF-based *)
    ("swf", [ "FWS"; "CWS"; "ZWS" ]);
    (* Documents *)
    ("pdf", [ "%PDF-" ]);
    ("ps", [ "%!PS" ]);
    ("ppt", [ "\xd0\xcf\x11\xe0\xa1\xb1\x1a\xe1" ]);
    ( "indd",
      [ "\x06\x06\xED\xF5\xD8\x1D\x46\xE5\xBD\x31\xEF\xE7\xFE\x74\xB7\x1D" ] );
    (* MS Office CDF *)
    (* Archives and compression *)
    ("zip", [ "PK\x03\x04"; "PK\x05\x06"; "PK\x07\x08" ]);
    ("jar", [ "PK\x03\x04"; "PK\x05\x06"; "PK\x07\x08" ]);
    ("xlsx", [ "PK\x03\x04"; "PK\x05\x06"; "PK\x07\x08" ]);
    ("gz", [ "\x1f\x8b" ]);
    ("tgz", [ "\x1f\x8b" ]);
    ("gzip", [ "\x1f\x8b" ]);
    ("bz2", [ "BZh" ]);
    ("xz", [ "\xfd7zXZ\x00" ]);
    ("7z", [ "7z\xbc\xaf\x27\x1c" ]);
    ("rar", [ "Rar!\x1a\x07" ]);
    ("zst", [ "\x28\xb5\x2f\xfd" ]);
    ("lz4", [ "\x04\x22\x4d\x18" ]);
    (* Executables and object files *)
    ("exe", [ "MZ" ]);
    ("class", [ "\xca\xfe\xba\xbe" ]);
    ("wasm", [ "\x00asm" ]);
    (* Media (sound/video) *)
    ("ogg", [ "OggS" ]);
    ("flac", [ "fLaC" ]);
    ("mp3", [ "ID3"; "\xff\xfb"; "\xff\xf3"; "\xff\xf2" ]);
    ("wav", [ "RIFF" ]);
    ("avi", [ "RIFF" ]);
    (* Databases *)
    ("db", [ "SQLite format 3\x00" ]);
    (* Fonts *)
    ("ttf", [ "\x00\x01\x00\x00" ]);
    (* TrueType *)
    ("otf", [ "OTTO" ]);
    ("woff", [ "wOFF" ]);
    ("woff2", [ "wOF2" ]);
  ]

let magic_by_ext_tbl : (string, string list) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  List.iter (fun (ext, magics) -> Hashtbl.replace tbl ext magics) magic_by_ext;
  tbl

(* Signatures for formats with no distinguishing extension; only consulted in
   the fallback sniff for File_type.Other and extension-less binaries (e.g. a
   Unix executable classified as Binary with an empty extension). *)
let extensionless_magic : string list =
  [
    "\x7fELF" (* ELF *);
    "\xfe\xed\xfa\xce" (* Mach-O 32-bit *);
    "\xfe\xed\xfa\xcf" (* Mach-O 64-bit *);
    "\xce\xfa\xed\xfe" (* Mach-O 32-bit, byte-swapped *);
    "\xcf\xfa\xed\xfe" (* Mach-O 64-bit, byte-swapped *);
    "\xca\xfe\xba\xbe" (* Mach-O fat / Java class *);
  ]

(* Every known signature, for the fallback content sniff used when the
   extension is unknown. *)
let all_magic_prefixes : string list =
  List.concat_map snd magic_by_ext @ extensionless_magic

(* Implement some error handling so that we skip over the validation
   if an IO exn is raised *)
let read_first_block_opt ~block_size path =
  try Some (Guess_lang.get_first_block ~block_size path) with
  | Unix.Unix_error _
  | Sys_error _
  | End_of_file ->
      Log.warn (fun m ->
          m
            "is_binary: could not read first bytes of %s; treating as \
             non-binary"
            !!path);
      None

let is_binary (path : Fpath.t) =
  let _d, _b, e = Filename_.dbe_of_filename !!path in
  let e = String.lowercase_ascii e in
  let binary_error name =
    Error
      {
        Out.path;
        reason = Binary;
        details = Some (spf "target looks like a binary file (%s)" name);
        rule_id = None;
      }
  in
  let starts_with_any prefixes s =
    List.exists (fun prefix -> String.starts_with ~prefix s) prefixes
  in

  (* Longest prefix is 16 bytes for SQLite *)
  let block_size = 16 in

  match File_type.file_type_of_file path with
  (* Source-like file types are never skipped, regardless of content. *)
  | PL _
  | Text _
  | Config _ ->
      Ok path
  (* Everything else is a binary candidate: confirm with a magic-byte match. *)
  | Obj _
  | Binary _
  | Media _
  | Doc _
  | Archive _ -> (
      match read_first_block_opt ~block_size path with
      | None -> Ok path
      | Some s ->
          let looks_binary =
            match Hashtbl.find_opt magic_by_ext_tbl e with
            (* Known binary extension: check only that format's signature(s). *)
            | Some prefixes -> starts_with_any prefixes s
            (* Extension-less: the size heuristic may have put this in Obj
               rather than Other, so check every signature like Other does. *)
            | None when e = "" -> starts_with_any all_magic_prefixes s
            (* Unknown extension: scan for extension-less signatures. *)
            | None -> starts_with_any extensionless_magic s
          in
          if looks_binary then
            binary_error (if e = "" then "binary signature" else e)
          else Ok path)
  | Other _ -> (
      match read_first_block_opt ~block_size path with
      | None -> Ok path
      | Some s ->
          (* Scan for all known signatures. *)
          let looks_binary = starts_with_any all_magic_prefixes s in
          if looks_binary then binary_error "binary signature" else Ok path)

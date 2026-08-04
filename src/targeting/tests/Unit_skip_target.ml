(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for Skip_target.is_binary.

   Each test writes a small fixture file with a chosen name (extension) and
   byte content into a temporary file, then checks whether is_binary
   classifies it as binary (Error) or not (Ok).

   is_binary works in two stages:
   - File_type.file_type_of_file gates on the *extension*: source-like types
     (PL/Text/Config) are never binary, regardless of content.
   - For the remaining binary candidates, the *content* is matched against
     magic-byte signatures. A known extension checks only that format's
     signature(s); an unknown extension in a specific binary category
     (Obj/Binary/Media/Doc/Archive) checks the extension-less signatures
     (ELF/Mach-O/PE) plus RIFF; File_type.Other sniffs every signature.

   So both the extension AND the bytes matter, and there is no NUL-byte
   requirement.
*)

(* Binary = is_binary returns Error; NotBinary = it returns Ok. *)
type expected = Binary | NotBinary

let t = Testo.create ?skipped:Testutil.skip_on_windows

(* A magic prefix followed by some arbitrary binary-looking payload. *)
let magic prefix = prefix ^ "\x00\x00\x00 binary payload \x01\x02\x03"

(* name, filename suffix (controls the extension), file contents, expected *)
let cases : (string * string * string * expected) list =
  [
    (* --- Binary via a keyed extension: only that format's magic is checked. *)
    ("PNG (.png)", ".png", magic "\x89PNG\r\n\x1a\n", Binary);
    ("JPEG (.jpg)", ".jpg", magic "\xff\xd8\xff", Binary);
    ("GIF (.gif)", ".gif", magic "GIF89a", Binary);
    ("PDF (.pdf)", ".pdf", magic "%PDF-", Binary);
    ("ZIP (.zip)", ".zip", magic "PK\x03\x04", Binary);
    ("gzip (.gz)", ".gz", magic "\x1f\x8b", Binary);
    ("TrueType (.ttf)", ".ttf", magic "\x00\x01\x00\x00", Binary);
    ("SQLite (.db)", ".db", magic "SQLite format 3\x00", Binary);
    ("Windows PE (.exe)", ".exe", magic "MZ", Binary);
    (* .class is keyed to the same \xca\xfe\xba\xbe as a Mach-O fat binary; the
       extension disambiguates. *)
    ("Java class (.class)", ".class", magic "\xca\xfe\xba\xbe", Binary);
    (* Extensions are lowercased before lookup, so an uppercase suffix works. *)
    ("PNG, uppercase ext (.PNG)", ".PNG", magic "\x89PNG\r\n\x1a\n", Binary);
    (* --- Binary via a keyed extension that File_type does not itself recognize:
       it lands in File_type.Other, where the full signature set (which still
       includes these keyed magics) is sniffed. --- *)
    ("7z (.7z)", ".7z", magic "7z\xbc\xaf\x27\x1c", Binary);
    (* --- Binary via the extension-less fallback (ELF/Mach-O/PE). A keyed
       extension would never list these, but they show up on object files and
       stripped executables. --- *)
    ("ELF object (.o)", ".o", magic "\x7fELF", Binary);
    ("Mach-O object, big-endian (.o)", ".o", magic "\xfe\xed\xfa\xce", Binary);
    (* The variant emitted by real toolchains on Apple Silicon / x86-64. *)
    ("Mach-O object, 64-bit LE (.o)", ".o", magic "\xcf\xfa\xed\xfe", Binary);
    ("Mach-O fat binary (.o)", ".o", magic "\xca\xfe\xba\xbe", Binary);
    ("extension-less ELF executable", "", magic "\x7fELF", Binary);
    ("extension-less png", "", magic "\x89PNG\r\n\x1a\n", Binary);
    ("extension-less PE", "", magic "MZ", Binary);
    (* A >300KB extension-less file is classified as Obj (not Other) by
       File_type's size heuristic, but its content sniff must still cover
       every signature, so a large extension-less PNG is skipped. *)
    ( "large extension-less png (> size heuristic)",
      "",
      magic "\x89PNG\r\n\x1a\n" ^ String.make 300_001 ' ',
      Binary );
    (* --- Binary via RIFF (offset-8 format tag), reached on the unknown-
       extension path. --- *)
    ("RIFF/WAVE (.wav)", ".wav", "RIFF\x10\x00\x00\x00WAVEmore data", Binary);
    (* --- NotBinary: source-like extension gates out before any magic check,
       even when the content starts with a real signature. --- *)
    ("source .py with PNG magic", ".py", magic "\x89PNG\r\n\x1a\n", NotBinary);
    ("text .txt with PDF magic", ".txt", "%PDF- just text\n", NotBinary);
    ("text .txt starting with MZ", ".txt", "MZ but plain text\n", NotBinary);
    (* --- NotBinary: a known binary category but no magic match. --- *)
    (* .svg is Media but unkeyed and its XML body matches nothing. *)
    ("SVG XML (.svg)", ".svg", "<?xml version=\"1.0\"?><svg></svg>", NotBinary);
    (* .png whose content is not actually a PNG: the keyed check fails. *)
    ("'.png' with text content", ".png", "not really a png\n", NotBinary);
    (* --- NotBinary: File_type.Other (unknown extension), no signature; a NUL
       byte alone no longer matters. --- *)
    ("unknown ext, plain text", ".fixture", "plain text\n", NotBinary);
    ( "unknown ext, NUL but no magic",
      ".fixture",
      "plain text with a \x00 nul byte\n",
      NotBinary );
    (* Empty file. *)
    ("empty .py file", ".py", "", NotBinary);
  ]

let test_is_binary suffix contents expected () =
  Testo.with_temp_file ~suffix ~contents (fun path ->
      match (expected, Skip_target.is_binary path) with
      | Binary, Error _
      | NotBinary, Ok _ ->
          ()
      | Binary, Ok _ -> failwith "expected a binary file but is_binary said Ok"
      | NotBinary, Error _ ->
          failwith "expected a non-binary file but is_binary said Error")

let tests =
  Testo.categorize "Skip_target.is_binary"
    (List.map
       (fun (name, suffix, contents, expected) ->
         t name (test_is_binary suffix contents expected))
       cases)

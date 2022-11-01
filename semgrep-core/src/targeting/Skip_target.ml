(*
   Filter targets.

   TODO: note that some of the functions below are also present in
   semgrep-python so we might want to move all file-filtering in one
   place.
*)
open Common
module Resp = Output_from_core_t

(****************************************************************************)
(* Minified files detection (via whitespace stats) *)
(****************************************************************************)

(*
   We used to require a minimum of 7% of whitespace per file but this
   resulted in false positives. Consult the git history for the old
   heuristic.

   We now only look at the average line length, with a high threshold
   which will ensure that no legitimate source files will be skipped
   (no false positives).
   Some minified files may not be detected as such (some false negatives).

   For motivation, see this Java source file which has a very repetitive
   but was at least partially edited by hand. It starts with more than
   8192 bytes of import statements with no indentation:

     https://github.com/OpenClinica/OpenClinica/blob/e46944f8719fec87ef005d3a8151541844d7fed1/core/src/main/java/org/akaza/openclinica/dao/extract/OdmExtractDAO.java

   This file contains 4.2 % of whitespace, which is low. Most source files
   contain over 7 % of whitespace.
*)

(*
   An average of 300 bytes per line is very long. This doesn't occur with
   normal programming languages. It can approach this with Markdown or maybe
   files that are mostly comments made of very long lines.

   Our line count includes 2 imaginary bonus lines so as to tolerate longer
   lines in shorter files. For example, 600 bytes without a newline count
   as 1 + 2 bonus lines = 3 lines, giving an average of 200 bytes per line.
   These settings tolerate up to 900 bytes without a newline.

   TODO: A slightly less crude statistical approach (e.g. based on trigram
   frequencies) would probably work better than this.
*)
let max_avg_line_length = 300.
let bonus_lines = 2

type whitespace_stat = {
  sample_size : int; (* size of the block; possibly the whole file *)
  avg_line_length : float;
}

let whitespace_stat_of_string s =
  (* number of lines = number of LF characters + 1 + imaginary bonus lines *)
  let lines = ref (1 + bonus_lines) in
  let other = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\n' -> incr lines
    | __else__ -> incr other
  done;
  let total = !lines + !other in
  let sample_size = String.length s in
  assert (!lines > 0);
  let avg_line_length = float total /. float !lines in
  { sample_size; avg_line_length }

let whitespace_stat_of_block ?block_size path =
  let s = Guess_lang.get_first_block ?block_size path in
  whitespace_stat_of_string s

(*
   We used to use the average line length to determine whether
   a file is definitely not human-readable.
*)
let is_minified path =
  if not !Flag_semgrep.skip_minified_files then Ok path
  else
    let stat = whitespace_stat_of_block ~block_size:8192 path in
    if stat.avg_line_length > max_avg_line_length then
      Error
        {
          Resp.path;
          reason = Minified;
          details =
            spf
              "the average line length calculated after adding two extra blank \
               lines is greater than the threshold: %.1f bytes (max = %.1f)"
              stat.avg_line_length max_avg_line_length;
          rule_id = None;
        }
    else Ok path

let exclude_minified_files paths = Common.partition_result is_minified paths

(****************************************************************************)
(* Pad's Skip_code filtering *)
(****************************************************************************)

let exclude_files_in_skip_lists roots =
  let paths, skipped_paths =
    Skip_code.filter_files_if_skip_list ~root:roots roots
  in
  let skipped =
    skipped_paths
    |> Common.map (fun path ->
           {
             Resp.path;
             reason = Excluded_by_config;
             details = "excluded by 'skip list' file";
             rule_id = None;
           })
  in
  (paths, skipped)

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
let exclude_big_files paths =
  let max_bytes = !Flag_semgrep.max_target_bytes in
  paths
  |> Common.partition_result (fun path ->
         let size = Common2.filesize path in
         if max_bytes > 0 && size > max_bytes then
           Error
             {
               Resp.path;
               reason = Too_big;
               details =
                 spf "target file size exceeds %i bytes at %i bytes" max_bytes
                   size;
               rule_id = None;
             }
         else Ok path)

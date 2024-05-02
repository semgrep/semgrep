let with_temp_file ?contents ?persist ?prefix ?suffix ?temp_dir _caps f =
  UTmp.with_temp_file ?contents ?persist ?prefix ?suffix ?temp_dir f

let get_temp_dir_name _caps = UTmp.get_temp_dir_name ()
let erase_temp_files _caps = UTmp.erase_temp_files ()

let new_temp_file ?prefix ?suffix ?temp_dir _caps =
  UTmp.new_temp_file ?prefix ?suffix ?temp_dir ()

let replace_named_pipe_by_regular_file_if_needed _caps =
  UTmp.replace_named_pipe_by_regular_file_if_needed

let replace_stdin_by_regular_file _caps = UTmp.replace_stdin_by_regular_file

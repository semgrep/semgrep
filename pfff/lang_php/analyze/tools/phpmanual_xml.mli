
val find_functions_reference_of_dir : 
  Common.dirname -> Common.filename list

val function_name_of_xml_filename:
  Common.filename -> string

val parse_xml:
  Common.filename -> Xml_parse.xml

val extract_useful_doc: Xml_parse.xml -> string

(* main entry point *)
val build_doc_function_finder: Common.dirname ->
  (string -> string)

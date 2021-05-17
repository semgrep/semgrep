Porting a Language in Semgrep-Core
--

Now that you have added you new language 'X' to pfff, do the following:
1. Add the new pfff submodule to semgrep-core. 
2. In `Check_semgrep.ml`, add 'X' to `lang_has_no_dollar_ids`/ If the grammar 
   has no dollar identifiers, add it above 'true'. Otherwise, add it above 'false'.
3. In `synthesizing/Pretty_print_generic.ml`, add 'X' to the appropriate functions:
   * print_bool
   * if_stmt
   * while_stmt
   * do_while
   * for_stmt
   * def_stmt
   * return
   * break
   * continue
   * literal
4. In `parsing/Test_parsing.ml`, add in 'X' to `dump_tree_sitter_cst_lang`.
   You can look to the other languages as reference to what code to add.
5. Create a file `parsing/Parse_X_tree_sitter.ml`. Add basic functionality to
   define the function `parse` and import module `Parse_tree_sitter_helpers`.
   You can look at csharp and kotlin files in order to get a better idea of how to 
   define the parse file function, but this file should contain something similar to:
   ```
   module H = Parse_tree_sitter_helpers

   let parse file = 
    H.wrap_parser
        (fun () ->
            Parallel.backtrace_when_exn := false
            Parallel.invoke Tree_sitter_X.Parse.file file ()
        )
   ```
6. In `parsing/dune`, add `tree-sitter-lang.X`.
7. Write a basic test case for your language in `tests/X/hello-world.X`. This can
   just be a hello-world function.
8. Test that the command
   `semgrep-core/bin/semgrep-core -dump_tree_sitter_cst test/X/hello-world`
   prints out a CST for your language.



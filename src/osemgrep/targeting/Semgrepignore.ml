(*
   Parse and interpret '.semgrepignore' files.

   Original implementation: ignores.py

   The legacy semgrepignore behavior is irregular and we're trying to move
   away from it and align closely with gitignore behavior.

   Old behavior:
   Here are the differences with gitignore listed in the legacy documentation:
   - '!' pattern negations aren't supported
   - character range patterns aren't supported
   - ':include' directives are a semgrepignore addition
   - '.semgrepignore' files placed anywhere in the tree are ignored (?)

   New behavior:
   - support '!' and character ranges to conform to gitignore syntax
   - don't support ':include' to conform to gitignore syntax
   - automatically use any '.gitignore' and use '.semgrepignore' additionally
     as if the latter was appended to the former.
   Support for negated patterns ('!') allows a .semgrepignore to
   undo exclusions made in a .gitignore.

   Migration plan:
   - print a deprecation notice if an ':include' directive is found in the
     root .semgrepignore.
   - stay silent otherwise: this is problematic only in the case of a
     .semgrepignore that doesn't include the .gitignore explicitly and
     contains fewer exclusions that the .gitignore. The new behavior will
     exclude more files than before.

   Questions:
   - Do some people really run semgrep on purpose on files that are excluded
     from source control?
     (we need to answer this to figure out the consequences of the migration
     plan)
*)

(*
   Preprocess a file to expand ':include' directives before parsing it
   using gitignore rules.

   Honor them with a deprecation warning.
*)

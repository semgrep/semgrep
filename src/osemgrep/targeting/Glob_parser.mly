%{
open Glob_matcher

type tmp =
| Fragment of segment_fragment
| Starstar

let convert_ellipses (fragments : tmp list) =
  match fragments with
  | [Starstar] -> Any_subpath
  | xs ->
     Segment (Common.map (function Fragment x -> x | Starstar -> Star) xs)
%}
%token SLASH QUESTION STAR STARSTAR EOF
%token <char> CHAR
%token <Glob_matcher.char_class> CHAR_CLASS

%start <Glob_matcher.segment list> segments
%%

segments:
| frags=list(fragment) SLASH comps=segments
     { convert_ellipses frags :: comps }
| frags=list(fragment) EOF
     { [convert_ellipses frags] }

fragment:
| c=CHAR
     { Fragment (Char c) }
| cc=CHAR_CLASS
     { Fragment (Char_class cc) }
| QUESTION
     { Fragment Question }
| STAR
     { Fragment Star }
| STARSTAR
     { Starstar }

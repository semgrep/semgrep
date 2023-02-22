%{
open Glob_matcher
%}
%token SLASH QUESTION STAR STARSTAR EOF
%token <char> CHAR
%token <Glob_matcher.char_class> CHAR_CLASS

%start <Glob_matcher.component list> components
%%

components:
| STARSTAR SLASH comps=components
     { Ellipsis :: comps }
| STARSTAR EOF
     { [Ellipsis] }
| frags=list(fragment) SLASH comps=components
     { Component frags :: comps }
| frags=list(fragment) EOF
     { [Component frags] }

fragment:
| c=CHAR
     { Char c }
| cc=CHAR_CLASS
     { Char_class cc }
| QUESTION
     { Question }
| STAR
     { Star }

#!/usr/bin/perl

my $after_percentpercent = 0;

while(<>) {
    if(/^%%/) { $after_percentpercent = 1; }

  # remove C-style comments
  s#/\*\(\*#(*#g;
  s#\*\)\*/#*)#g;
  s#/\*#(*#g;
  s#\*/#*)#g;

  # remove pad sectioning number
  s#\(\*[0-9] #(* #g;

    if($after_percentpercent) {
  
  # use chars instead of long tokens for parens-like tokens
  s#\bLP\b#"("#g;
  s#\bRP\b#")"#g;
  s#\bLB\b#"["#g;
  s#\bRB\b#"]"#g;
  s#\bLC\b#"{"#g;
  s#\bRC\b#"}"#g;

  # ocaml extra "parenthesis" operators
  #s#\bTOBracketLess\b#"[<"#g;
  #s#\bTGreaterCBracket\b#">]"#g;
  #s#\bTOBracketPipe\b#"[|"#g;
  #s#\bTPipeCBracket\b#"|]"#g;
  #s#\bTOBraceLess\b#"{<"#g;
  #s#\bTGreaterCBrace\b#">}"#g;

  # use chars instead of long tokens for common punctuators
  s#\bSM\b#";"#g;
  s#\bCM\b#","#g;
  s#\bDOT\b#"."#g;
  s#\bCOLON\b#":"#g;
  s#\bAT\b#"@"#g;

  #s#\bTSemiColonSemiColon\b#";;"#g;
  #s#\bTDotDot\b#".."#g;
  s#\bCOLONCOLON\b#"::"#g;
  #s#\bTQuestionQuestion\b#"??"#g;

  #s#\bTQuestion\b#"?"#g;
  #s#\bBITNOT\b#"~"#g;
  #s/\bTSharp\b/"#"/g;
  #s#\bTColonGreater\b#":>"#g;

  #s#\bBITOR\b#"|"#g;
  #s#\bTAssignMutable\b#"<-"#g;
  #s#\bTAssign\b#":="#g;
  #s#\bTBang\b#"!"#g;
  s#\bARROW\b#"->"#g;
  #s#\bTUnderscore\b#"_"#g;

  # semgrep!
  s#\bDOTS\b#"..."#g;
  s#\bLDots\b#"<..."#g;
  s#\bRDots\b#"...>"#g;

  # use chars instead of long tokens for important operators
  #s#\bLMULT\b#"*"#g;
  #s#\bPOW\b#"**"#g;
  s#\bEQ\b#"="#g;

  #s#\bLCOLAS\b#":="#g;
  #s#\bLCOMM\b#"<-"#g;

  #s#\bTQuote\b#"'"#g;
  #s#\bBACKQUOTE\b#"`"#g;

  }  

  print;
}

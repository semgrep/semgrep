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
  s#\bTOPAR\b#"("#g;
  s#\bTCPAR\b#")"#g;
  s#\bTOBRA\b#"["#g;
  s#\bTCBRA\b#"]"#g;
  s#\bTOBRACE\b#"{"#g;
  s#\bTCBRACE\b#"}"#g;

  # OCaml extra "parenthesis" operators
  #s#\bTOBracketLess\b#"[<"#g;
  #s#\bTGreaterCBracket\b#">]"#g;
  #s#\bTOBracketPipe\b#"[|"#g;
  #s#\bTPipeCBracket\b#"|]"#g;
  #s#\bTOBraceLess\b#"{<"#g;
  #s#\bTGreaterCBrace\b#">}"#g;

  # use chars instead of long tokens for common punctuators
  s#\bTSEMICOLON\b#";"#g;
  s#\bTCOMMA\b#","#g;
  s#\bTDOT\b#"."#g;
  s#\bTCOLON\b#":"#g;
  s#\bT__AT\b#"@"#g;
  s#\bTDOLLAR\b#"\$"#g;
  s#\bTDOLLARDOLLAR\b#"\$\$"#g;

  #s#\bTSemiColonSemiColon\b#";;"#g;
  #s#\bTDotDot\b#".."#g;
  s#\bTCOLCOL\b#"::"#g;
  #s#\bTQuestionQuestion\b#"??"#g;

  #s#\bTQuestion\b#"?"#g;
  #s#\bBITNOT\b#"~"#g;
  #s/\bTSharp\b/"#"/g;
  #s#\bTColonGreater\b#":>"#g;

  #s#\bBITOR\b#"|"#g;
  #s#\bTAssignMutable\b#"<-"#g;
  #s#\bTAssign\b#":="#g;
  #s#\bTBang\b#"!"#g;
  s#\bT_ARROW\b#"=>"#g;
  s#\bT_DOUBLE_ARROW\b#"==>"#g;
  s#\bT_OBJECT_OPERATOR\b#"->"#g;
  #s#\bTUnderscore\b#"_"#g;

  # semgrep!
  s#\bDOTS\b#"..."#g;
  s#\bLDots\b#"<..."#g;
  s#\bRDots\b#"...>"#g;

  # use chars instead of long tokens for important operators
  #s#\bLMULT\b#"*"#g;
  #s#\bPOW\b#"**"#g;
  #s#\bEQ\b#"="#g;

  #s#\bLCOLAS\b#":="#g;
  #s#\bLCOMM\b#"<-"#g;

  #s#\bTQuote\b#"'"#g;
  #s#\bBACKQUOTE\b#"`"#g;

  }

  print;
}

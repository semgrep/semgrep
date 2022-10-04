//#include <stdio.h>
//#include <assert.h>

void bad_func(unsigned int p[NUM]) {
  // sizeof(p) was incorrectly parsed as a type because of wrong typedef
  // heuristic in parsing_hacks_typdef.ml because wrong InParameter inference
  // in token_views_context.ml
  //ERROR: match
  for (int i = 0;  i < sizeof(p)/sizeof(p[0]); i++) {
  }
}

void good_func(unsigned int (*p)[NUM]) {
    //ERROR: match, this was correctly not-inferred as a type
    for(unsigned int i = 0; i < sizeof(*p)/sizeof(*p[0]); i++)
      (*p)[i] = CLEAR;
    return;
}


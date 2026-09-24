#if FIRST
int ignored = 0;
#elifdef SECOND
//ERROR: match
int selected = 1;
#elifndef THIRD
//ERROR: match
int selected = 2;
#else
int ignored = 3;
#endif

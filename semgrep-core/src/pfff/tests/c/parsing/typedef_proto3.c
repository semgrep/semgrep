
// the presence of this macro makes the
// typedef inference failed after :(
#define s_len(s) ((s)->ptr-(s)->base)

extern String*	s_append(String*, char*);

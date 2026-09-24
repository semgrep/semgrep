#if FIRST
int first;
#elifdef SECOND
int second;
#elifndef THIRD
int third;
#else
int fallback;
#endif
struct Fields {
#if FIRST
int first;
#elifdef SECOND
int second;
#elifndef THIRD
int third;
#endif
};
enum Commas {
#if FIRST
first,
#elifdef SECOND
second,
#elifndef THIRD
third,
#endif
last
};
enum NoComma {
#ifdef FIRST
first
#elifdef SECOND
second
#elifndef THIRD
third
#endif
};

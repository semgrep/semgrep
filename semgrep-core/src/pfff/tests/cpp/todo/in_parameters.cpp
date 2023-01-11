
// see also typedef.cpp

int foo(foo10 *, foo11 &);

// having discovered one type in on param is enough for the rest
int foo(long, foo11);

int foo(foo10 * b, foo11 &);

int foo(foo10 * b, foo11 * b);

int foo(foo10 b, int x = 1);

int foo(foo10 * b, foo11 b);

int foo(foo10_t, foo11_t);

int foo(foo10* x);

//TODO: void (*x)(foo10* x);

extern size_t strbuf_fread(struct strbuf *, size_t, FILE *);
void* dlcalloc(size_t, size_t);
void* dlrealloc(void*, size_t);


static int64_t normalizeTimestamp(int64_t timestamp, int timeUnit = 60);

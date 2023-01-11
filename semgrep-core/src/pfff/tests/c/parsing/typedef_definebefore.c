#include <foo.h>
extern Foo* foobar(int);

#define XX(a) a+b
// endofdefine should be considered like a declaration context
// see parsing_hacks_typedef.ml
Foo* foobar(int);


static Recover  *rtab;

#define csr32w(c, r, v) (*((c)->nic+((r)/4)) = (v))
static CtlrEtherIgbe* igbectlrhead;

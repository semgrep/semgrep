//ERROR: match
typedef int (&array_reference)[4];
typedef int (*array_pointer)[4];
typedef int (&&array_rvalue_reference)[4];

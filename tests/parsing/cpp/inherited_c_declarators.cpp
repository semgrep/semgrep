long const int sized_const;
void param_attribute(int value __attribute((unused)));
int __stdcall calling_convention(int value) { return value; }
typedef int (__stdcall *Callback)(int);

#define ANYDEFINE 1
enum myenum {
    enum_a,
//#ifdef OPTIONAL_ENUM_A2
//    enum_a2,
//#endif
    enum_b,
    enum_c
};
long myfunc(int arg1, int arg2)
{
    /* example comment */
    return arg1 + arg2;
}

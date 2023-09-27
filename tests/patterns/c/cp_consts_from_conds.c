void test1(int *ptr)
{
    if (ptr) {
        // OK:
        do_something(&ptr);
        return;
    }

    // ERROR:
    do_something(&ptr);
}

void test2(int *ptr)
{
    if (!ptr) {
        // ERROR:
        do_something(&ptr);
        return;
    }

    // OK:
    do_something(&ptr);
}

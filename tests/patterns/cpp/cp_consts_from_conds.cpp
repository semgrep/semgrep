void test1()
{
    int *int_ptr = nullptr;

    // ERROR:
    do_something(&int_ptr);

    if (int_ptr != nullptr) {
        return;
    }

    // ERROR:
    do_something(&int_ptr);
}

void test2()
{
    int *int_ptr = nullptr;

    // ERROR:
    do_something(&int_ptr);

    if (int_ptr == nullptr) {
        return;
    }

    // since (int_ptr != nullptr) we know that int_ptr cannot be nullptr anymore
    // OK:
    do_something(&int_ptr);
}

void test3(int *int_ptr)
{
    if (int_ptr == nullptr) {
        // ERROR:
        do_something(&int_ptr);
        return;
    }

    // since (int_ptr != nullptr) we know that int_ptr cannot be nullptr anymore
    // OK:
    do_something(&int_ptr);
}

void test4(int *int_ptr)
{
    if (int_ptr != nullptr) {
        // since (int_ptr != nullptr) we know that int_ptr cannot be nullptr
        // OK:
        do_something(&int_ptr);
        return;
    }

    // ERROR:
    do_something(&int_ptr);
}

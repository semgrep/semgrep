void* delete_call(Data* from) {
    // An example from `facebook/folly`. This is a low priority issue
    // ok: double-delete
    delete get(*from);
}

void delete_cast(Mutex *mutex) {
    // This is logically the same as `delete_no_cast` above
    // ok: double-delete
    delete (CRITICAL_SECTION*)mutex;
}
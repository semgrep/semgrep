#include <memory>
using namespace std;

void foo() {
    int *i = 0;

    // ruleid: match-with-template
    shared_ptr p;

    // ruleid: match-with-template
    shared_ptr<int> p;
}


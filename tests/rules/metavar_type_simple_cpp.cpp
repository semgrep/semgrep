#include <fstream>

using namespace std;

void test_001() {
    ifstream in;
    // ruleid: match-simple-metavar-type
    in.get(str, 2);

    mystream my;
    // ok: type-mismatch
    my.get(str, 2);
}

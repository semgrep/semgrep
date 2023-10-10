#include <fstream>

using namespace std;

void test_001() {
    ifstream in;
    // ruleid: match-multiple-metavar-type
    in.get(str, 2);

    mystream my;
    // ruleid: match-multiple-metavar-type
    my.get(str, 2);

    yourstream your;
    // ok: match-multiple-metavar-type
    your.get(str, 2);
}

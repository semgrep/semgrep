/* https://github.com/returntocorp/semgrep/issues/3727 */
int main() {
    int x = 3;
    int y = 5;
    /* ruleid: c-comparing-const-vars */
    if (x < y) {
        print("less");
    }
}


const char* PASSWORD = "password";

int main(int argc, char *argv[]) {
    const char* inner_password = "password";

    //ERROR: match
    foo("password");
    //ERROR: match
    foo(PASSWORD);
    //ERROR: match
    foo(inner_password);
}

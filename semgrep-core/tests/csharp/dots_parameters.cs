public class ParametersWithEllipsis {
    void SomeMethod() {
    }

    void SomeMethod(int notamatch) {
    }

    //ERROR:
    void SomeMethod(int match) {
    }

    //ERROR:
    void SomeMethod(int before, int match) {
    }

    //ERROR:
    void SomeMethod(int match, int after) {
    }

    //ERROR:
    void SomeMethod(int before, int match, int after) {
    }
}
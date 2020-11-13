class Bar {
    void main() {
        boolean myBoolean;

        // ruleid:hardcoded-conditional
        if (myBoolean = true) {
            continue;
        }

        // ruleid:hardcoded-conditional
        if (true) {
            continue;
        }

        // ruleid:hardcoded-conditional
        if (true && false) {
            continue;
        }

        // ok
        if (myBoolean) {

        }

        // ok
        if (myBoolean == myBoolean) {
            continue;
        }

        // ok
        if (myBoolean != myBoolean) {
            continue;
        }

    }
}
